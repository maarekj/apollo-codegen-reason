type type_ = {
  kind: string,
  name: string,
  description: string,
};

let to_type data => {
  kind: data##kind,
  name: data##name,
  description: data##description,
};

type fragment;

type variable = {
  name: string,
  type_: string,
};

let to_variable data => {
  name: data##name,
  type_: data##_type,
};

type arg_value = {
  kind: string,
  variableName: string,
};

let to_arg_value data => {
  kind: data##kind,
  variableName: data##variableName,
};

type arg = {
  name: string,
  value: arg_value,
};

let to_arg data => {
  name: data##name,
  value: to_arg_value data##value,
};

type field = {
  response_name: string,
  field_name: string,
  type_: string,
  is_conditional: bool,
  is_deprecated: bool,
  args: option (list arg),
  fields: option (list field),
};

let rec to_field data => {
  response_name: data##responseName,
  field_name: data##fieldName,
  type_: data##_type,
  is_conditional: Js.to_bool data##isConditional,
  is_deprecated: Js.to_bool data##isDeprecated,
  args: switch (Js.Null_undefined.to_opt data##args) {
    | None => None
    | Some args => Some (args |> Array.to_list |> List.map to_arg)
  },
  fields: switch (Js.Null_undefined.to_opt data##fields) {
    | None => None
    | Some fields => Some (fields |> Array.to_list |> List.map to_field)
  },
};

type operation = {
  file_path: string,
  operation_name: string,
  operation_type: string,
  root_type: string,
  source: string,
  operation_id: string,
  variables: option (list variable),
  fields: list field,
};

let to_operation data => {
  file_path: data##filePath,
  operation_name: data##operationName,
  operation_type: data##operationType,
  root_type: data##rootType,
  source: data##source,
  operation_id: data##operationId,
  variables: switch (Js.Null_undefined.to_opt data##variables) {
    | None => None
    | Some fields => Some (fields |> Array.to_list |> List.map to_variable)
  },
  fields: data##fields |> Array.to_list |> List.map to_field,
};

type file = {
  operations: list operation,
  fragments: list fragment,
  types_used: list type_,
};

let to_file data => {
  operations: data##operations |> Array.to_list |> List.map to_operation,
  fragments: [],
  types_used: data##typesUsed |> Array.to_list |> List.map to_type,
};

let from_json = to_file;