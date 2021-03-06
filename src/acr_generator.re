open Acr_ast;

open Acr_utils;

module Type_parser = Acr_type_parser;

let sprintf = Printf.sprintf;

let type_flatten_composed_fields = (parent_name, field, sub_fields) => {
  let sub_fields =
    sub_fields
    |> List.map(
         (f: field) =>
           switch f.fields {
           | None =>
             sprintf(
               "  %s: %s",
               f.response_name,
               Type_parser.to_reason_type(Type_parser.scalar_to_reason, f.type_)
             )
           | Some(_) =>
             sprintf(
               "  %s: %s",
               f.response_name,
               Type_parser.to_reason_type(
                 (_) => sprintf("%s%s_%s", parent_name, field.response_name, f.response_name),
                 f.type_
               )
             )
           }
       );
  sprintf("type %s%s = {\n%s\n};", parent_name, field.response_name, join(",\n", sub_fields))
};

let rec type_flatten_fields = (parent_name, field: field) =>
  switch field.fields {
  | None => []
  | Some(sub_fields) =>
    List.concat([
      List.concat(
        List.map(type_flatten_fields(parent_name ++ field.response_name ++ "_"), sub_fields)
      ),
      [type_flatten_composed_fields(parent_name, field, sub_fields)]
    ])
  };

let convert_to_reason_composed_fields = (parent_name, field, sub_fields) => {
  let sub_fields =
    sub_fields
    |> List.map(
         (f: field) =>
           switch f.fields {
           | None =>
             sprintf(
               "  %s: %s",
               f.response_name,
               Type_parser.to_converter(
                 ~initial_optional=false,
                 ~initial_mapper="((x) => x)",
                 ~field="data##" ++ f.response_name,
                 ~type_=f.type_
               )
             )
           | Some(_) =>
             sprintf(
               "  %s: %s",
               f.response_name,
               Type_parser.to_converter(
                 ~field=sprintf("data##%s", f.response_name),
                 ~initial_optional=false,
                 ~initial_mapper=
                   sprintf("to_%s%s_%s", parent_name, field.response_name, f.response_name),
                 ~type_=f.type_
               )
             )
           }
       );
  sprintf(
    "let to_%s%s = (data: Js.t({..})): %s%s => {\n%s\n};",
    parent_name,
    field.response_name,
    parent_name,
    field.response_name,
    join(",\n", sub_fields)
  )
};

let query_variables_type = (variables: list(variable)) => {
  let variables =
    variables
    |> List.map(
         (variable: variable) =>
           sprintf(
             "  %s: %s",
             variable.name,
             Type_parser.to_reason_type(Type_parser.scalar_to_reason, variable.type_)
           )
       );
  sprintf("type queryVariables = {\n%s\n};", join(",\n", variables))
};

let rec convert_to_reason_fields = (parent_name, field: field) =>
  switch field.fields {
  | None => []
  | Some(sub_fields) =>
    List.concat([
      List.concat(
        List.map(convert_to_reason_fields(parent_name ++ field.response_name ++ "_"), sub_fields)
      ),
      [convert_to_reason_composed_fields(parent_name, field, sub_fields)]
    ])
  };

let variables_to_js = (variables: list(variable)) =>
  variables
  |> List.map(
       (variable: variable) =>
         sprintf(
           "  \"%s\": %s",
           variable.name,
           Type_parser.to_js_converter(
             ~field=sprintf("vars.%s", variable.name),
             ~type_=variable.type_,
             ~nullType="Null",
             ()
           )
         )
     );

let generate_operation = (operation: operation) => {
  let res =
    sprintf(
      {eos|
module %s = {
type gql;
[@bs.module] external gql : string => gql = "graphql-tag";
let query = gql({| %s |});

type hoc = [@bs] (ReasonReact.reactClass => ReasonReact.reactClass);
type graphql('a) = [@bs] ((gql, Js.t('a)) => hoc);
[@bs.module "react-apollo"] external _graphql : graphql('a) = "graphql";

|eos},
      capitalize(operation.operation_name),
      operation.source
    );
  let root_field = first_or_fail(operation.fields);
  let all_types = type_flatten_fields("", root_field);
  let all_converters = convert_to_reason_fields("", root_field);
  let res =
    sprintf(
      {|%s
%s

type data = {
  loading: bool,
  error: option(Js.Exn.t),
  network_status: React_apollo.networkStatus,
  refetch: unit => unit,
  %s: %s,
};

%s
|},
      res,
      join("\n\n", all_types),
      root_field.response_name,
      Type_parser.to_reason_type(
        ~initial_optional=true,
        (_) => root_field.response_name,
        root_field.type_
      ),
      join("\n\n", all_converters)
    );
  let res =
    sprintf(
      {|
      %s
      let to_data = (data: Js.t({..})): data => {
        loading: Js.to_bool(data##loading),
        error: Js.Nullable.to_opt(data##error),
        network_status: React_apollo.to_network_status(data##networkStatus),
        %s: %s,
        refetch: data##refetch,
      };
      |},
      res,
      root_field.response_name,
      Type_parser.to_converter(
        ~initial_optional=true,
        ~field=sprintf("data##%s", root_field.response_name),
        ~initial_mapper=sprintf("to_%s", root_field.response_name),
        ~type_=root_field.type_
      )
    );
  let variables = def([], operation.variables);
  let res =
    if (List.length(variables) == 0) {
      sprintf(
        {|
%s

let graphql = (~component, ~fromJs) => {
  let componentJsClass =
    ReasonReact.wrapReasonForJs(
      ~component,
      (props: Js.t({..})) => {
        let data: data = to_data(props##data);
        fromJs(~props, ~data)
      }
    );
  [@bs] ([@bs] _graphql(query, Js.Obj.empty()))(componentJsClass)
};
|},
        res
      )
    } else {
      sprintf(
        {|
  %s
  %s


  let graphql = (~component, ~fromJs, ~variables) => {
    let componentJsClass =
      ReasonReact.wrapReasonForJs(
        ~component,
        (props: Js.t({..})) => {
          let data: data = to_data(props##data);
          fromJs(~props, ~data)
        }
      );
    let options = {
      "options":
        [@bs]
        (
          (jsProps) => {
            let vars = variables(jsProps);
            {"variables": {%s}}
          }
        )
    };
    [@bs] ([@bs] _graphql(query, options))(componentJsClass)
  };
|},
        res,
        query_variables_type(variables),
        join(",\n", variables_to_js(variables))
      )
    };
  sprintf("%s\n};", res)
};

let generate = (file: file) : string => {
  let operations = List.map(generate_operation, file.operations);
  join("\n\n", operations)
};