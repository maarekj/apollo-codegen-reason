let sprintf = Printf.sprintf;

exception ParsingError;
exception NotYetSupported;
exception MalformedType string string;
let raise_malformed_type type_ => raise (MalformedType type_ (sprintf "The type \"%s\" is malformed" type_));

module Lexer = {
  type lexer = {
    string: string,
    current_pos: int,
  };

  let create_lexer str => {
    string: str,
    current_pos: 0,
  };

  type token =
    | LeftBrace
    | RightBrace
    | Word string
    | Exclamation
    | EOS;

  let word_regex = Js.Re.fromString "[a-zA-Z_][a-zA-Z_0-9]*";

  let get_token lexer => {
    if (lexer.current_pos >= (String.length lexer.string)) {
      EOS;
    } else {
      let char = String.get lexer.string lexer.current_pos;
      if (char == '[') {
        LeftBrace;
      } else if (char == ']') {
        RightBrace
      } else if (char == '!') {
        Exclamation
      } else {
        let next_string = Js.String.substr from::(lexer.current_pos) lexer.string;
        switch (Js.Re.exec next_string word_regex) {
          | None => EOS
          | Some result => Word (Array.get (Js.Re.matches result) 0)
        }
      }
    }
  };

  let inc_lexer lexer => {
    let current_token = get_token lexer;
    let inc = switch current_token {
      | LeftBrace | RightBrace | Exclamation | EOS => 1
      | Word str => String.length str
    };
    {
      ...lexer,
      current_pos: min (lexer.current_pos + inc) (String.length lexer.string)
    }
  };
};

module Parser = {
  open Lexer;

  type gqlType =
    | Array gqlType
    | Scalar string
    | Optional gqlType;

  type gqlTypeAux =
    | Array gqlTypeAux
    | Scalar string
    | Strict gqlTypeAux;
  
  let parse str => {
    let lexer = create_lexer str;

    let rec scalar_rule lexer => switch (get_token lexer) {
      |  Word str => Some (inc_lexer lexer, Scalar str)
      | _ => None
    } and array_rule lexer => switch (get_token lexer) {
      | LeftBrace => {
        let lexer = inc_lexer lexer;
        switch (main_rule lexer) {
          | None => None
          | Some (lexer, ast) => switch (get_token lexer) {
            | RightBrace => Some (inc_lexer lexer, Array ast)
            | _ => None
          }
        };
      }
      | _ => None 
    } and main_rule lexer => {
      let scalar = scalar_rule lexer;
      let array = array_rule lexer;

      switch (Js.Option.firstSome scalar array) {
        | None => None
        | Some (lexer, ast) => switch (get_token lexer) {
          | Exclamation => Some (inc_lexer lexer, Strict ast)
          | _ => Some (lexer, ast)
        }
      };
    };

    let rec normalize_ast (optional: bool) (node: gqlTypeAux): gqlType => switch (node) {
      | Array node => {
        let n: gqlType = Array (normalize_ast true node);
        optional ? Optional (n) : n
      }
      | Scalar str => {
        let n: gqlType = Scalar str;
        optional ? Optional (n) : n
      }
      | Strict node => normalize_ast false node
    };

    switch (main_rule lexer) {
      | None => None
      | Some (_, ast) => Some (normalize_ast true ast)
    };
  };

  let parse_or_fail str => switch (parse str) {
    | None => raise ParsingError
    | Some node => node
  };
};

let is_bool str => str == "boolean" || str == "bool" || str == "Boolean" || str == "Bool"; 

let scalar_to_reason str => switch str {
  | "Int" | "int" => "int"
  | "String" | "string" => "string"
  | "Boolean" | "boolean" | "Bool" | "bool" => "bool"
  | "ID" | "Id" | "id" => "string"
  | "NullableBoolean" => "bool"
  | _ => raise NotYetSupported
};

let to_reason_type initial_optional::initial_optional=false scalar_to_reason str => {
  let rec _to_reason_type (node: Parser.gqlType) => switch (node) {
    | Array node => sprintf "list (%s)" (_to_reason_type node)
    | Scalar scalar => scalar_to_reason scalar
    | Optional node => sprintf "option (%s)" (_to_reason_type node)
  };

  switch (initial_optional, Parser.parse str) {
    | (_, None) => raise_malformed_type str
    | (false, Some node) => _to_reason_type node
    | (true, Some (Optional node)) => _to_reason_type (Optional node)
    | (true, Some node) => _to_reason_type (Optional node)
  };
};

let to_converter initial_optional::initial_optional=false ::initial_mapper ::field ::type_ => {  
  let apply_mapper field mapper => switch mapper {
    | "(fun x => x)" => field
    | mapper => sprintf "(%s) |> %s" field mapper
  };

  let mapper_list mapper => switch mapper {
    | "(fun x => x)" => "Array.to_list"
    | mapper => sprintf "Array.to_list |> List.map (%s)" mapper
  };

  let mapper_option mapper => switch mapper {
  | "(fun x => x)" => "Js.Null_undefined.to_opt"
  | mapper => sprintf "(fun x => switch (x |> Js.Null_undefined.to_opt) {
  | None => None
  | Some data => Some (%s)
})" (apply_mapper "data" mapper)
  };

  let compose_mapper mapper1 mapper2 => switch (mapper1, mapper2) {
    | ("(fun x => x)", "(fun x => x)") => "(fun x => x)"
    | ("(fun x => x)", map) => map
    | (map, "(fun x => x)") => map
    | (mapper1, mapper2) => sprintf "(fun x => x |> (%s) |> (%s))" mapper1 mapper2
  };

  let bool_mapper = compose_mapper initial_mapper "Js.to_bool";

  let _to_converter (node: Parser.gqlType) => switch (node) {
    | Scalar str => {
      let mapper = is_bool str ? bool_mapper : initial_mapper;
      apply_mapper field mapper;
    }
    | Array (Optional (Scalar str))  => {
      let mapper = is_bool str ? bool_mapper : initial_mapper;
      apply_mapper field (mapper_list (mapper_option (mapper)));
    }
    | Array (Scalar str) => {
      let mapper = is_bool str ? bool_mapper : initial_mapper;
      apply_mapper field (mapper_list (mapper));
    }
    | Optional (Scalar str) => {
      let mapper = is_bool str ? bool_mapper : initial_mapper;
      apply_mapper field (mapper_option (compose_mapper mapper "(fun x => x)"));
    }
    | Optional (Array (Scalar str)) => {
      let mapper = is_bool str ? bool_mapper : initial_mapper;
      apply_mapper field (mapper_option (mapper_list mapper));
    }
    | Optional (Array (Optional (Scalar str))) => {
      let mapper = is_bool str ? bool_mapper : initial_mapper;
      apply_mapper field (mapper_option (mapper_list (mapper_option mapper)));
    }
    | _ => raise NotYetSupported
  };

  switch (initial_optional, Parser.parse type_) {
    | (_, None) => raise_malformed_type type_
    | (false, Some node) => _to_converter node
    | (true, Some (Optional node)) => _to_converter (Optional node)
    | (true, Some node) => _to_converter (Optional node)
  };
};

let to_js_converter ::field ::type_ => {
  let _to_js_converter (node: Parser.gqlType) => switch (node) {
    | Scalar b when (is_bool b) => sprintf "(%s) |> Js.Boolean.to_js_boolean" field
    | Scalar _ => field
    | Optional (Scalar b) when (is_bool b) || (b == "NullableBoolean") => sprintf "switch (%s) {
  | None => Js.null
  | Some b => Some (b |> Js.Boolean.to_js_boolean) |> Js.Null.from_opt
}" field
    | Optional (Scalar _) => sprintf "(%s) |> Js.Null.from_opt" field
    | _ => raise NotYetSupported
  };

  switch (Parser.parse type_) {
    | None => raise_malformed_type type_
    | Some node => _to_js_converter node
  };
};