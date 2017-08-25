open Jest;

describe "lexer" (fun () => {
  open Expect;
  open! Expect.Operators;
  open Acr.Type_parser.Lexer;
  
  let lexer = create_lexer "[String]!";

  test "get_token" (fun _ => expect (get_token lexer) == LeftBrace);
  test "inc_lexer |> get_token" (fun _ => expect (lexer |> inc_lexer |> get_token) == Word "String");
  test "get_token" (fun _ => expect (get_token lexer) == LeftBrace);

  let lexer = inc_lexer lexer;
  test "inc_lexer |> get_token" (fun _ => expect (lexer |> get_token) == Word "String");

  let lexer = inc_lexer lexer;
  test "inc_lexer |> get_token" (fun _ => expect (lexer |> get_token) == RightBrace);

  let lexer = inc_lexer lexer;
  test "inc_lexer |> get_token" (fun _ => expect (lexer |> get_token) == Exclamation);

  let lexer = inc_lexer lexer;
  test "inc_lexer |> get_token" (fun _ => expect (lexer |> get_token) == EOS);
});

describe "parser" (fun () => {
  open Expect;
  open! Expect.Operators;
  open Acr.Type_parser.Parser;

  test "parse [String]!" (fun _ => expect (parse "[String]!") == Some (
    Array (Optional (Scalar "String"))
  ));

  test "parse [String]" (fun _ => expect (parse "[String]") == Some (
    Optional (Array (Optional (Scalar "String")))
  ));

  test "parse String" (fun _ => expect (parse "String") == Some (
    Optional (Scalar "String")
  ));

  test "parse String!" (fun _ => expect (parse "String!") == Some (
    (Scalar "String")
  ));

  test "parse [String!]!" (fun _ => expect (parse "[String!]!") == Some (
    (Array (Scalar "String"))
  ));
  
  test "parse [[String]]" (fun _ => expect (parse "[[String]]") == Some (
    Optional (Array (Optional (Array (Optional (Scalar "String")))))
  ));

  test "parse Bool!" (fun _ => expect (parse "Bool!]") == Some (
    Scalar "Bool"
  ));

  test "parse Boolean!" (fun _ => expect (parse "Boolean") == Some (
    Optional (Scalar "Boolean")
  ));
});

describe "to_reason_type" (fun () => {
  open Expect;
  open! Expect.Operators;
  open Acr.Type_parser;

  describe "with scalar_to_reason" (fun _ => {
    let trt = to_reason_type scalar_to_reason;

    test "NullableBoolean" (fun _ => expect (trt "NullableBoolean") == "option (bool)");
    test "[NullableBoolean]!" (fun _ => expect (trt "[NullableBoolean]!") == "list (option (bool))");
    test "[String]!" (fun _ => expect (trt "[String]!") == "list (option (string))");
    test "[String]" (fun _ => expect (trt "[String]") == "option (list (option (string)))");
    test "String" (fun _ => expect (trt "String") == "option (string)");
    test "String!" (fun _ => expect (trt "String!") == "string");
    test "[String!]!" (fun _ => expect (trt "[String!]!") == "list (string)");
    test "[[String]]" (fun _ => expect (trt "[[String]]") == "option (list (option (list (option (string)))))");
    test "[[String!]!]!" (fun _ => expect (trt "[[String!]!]!") == "list (list (string))");

    test "fail with an unknown type" (fun _ => expect (fun _ => {let _ = trt "[Unknown]"; ()}) |> toThrowMessage "NotYetSupported");
    test "fail with an unknown type" (fun _ => expect (fun _ => {let _ = trt "Unknown"; ()}) |> toThrowMessage "NotYetSupported");
    test "fail with an malformed type" (fun _ => expect (fun _ => {let _ = trt "[String"; ()}) |> toThrowMessage "The type \"[String\" is malformed");
    test "fail with an malformed type" (fun _ => expect (fun _ => {let _ = trt "!String"; ()}) |> toThrowMessage "The type \"!String\" is malformed");
  });

  describe "with custom scalar" (fun _ => {
    let trt = to_reason_type (fun _ => "my_type");
    
    test "[String]!" (fun _ => expect (trt "[String]!") == "list (option (my_type))");
    test "[String]" (fun _ => expect (trt "[String]") == "option (list (option (my_type)))");
    test "String" (fun _ => expect (trt "String") == "option (my_type)");
    test "String!" (fun _ => expect (trt "String!") == "my_type");
    test "[String!]!" (fun _ => expect (trt "[String!]!") == "list (my_type)");
    test "[[String]]" (fun _ => expect (trt "[[String]]") == "option (list (option (list (option (my_type)))))");
    test "[[String!]!]!" (fun _ => expect (trt "[[String!]!]!") == "list (list (my_type))");

    test "fail with an malformed type" (fun _ => expect (fun _ => {let _ = trt "[String"; ()}) |> toThrowMessage "The type \"[String\" is malformed");
    test "fail with an malformed type" (fun _ => expect (fun _ => {let _ = trt "!String"; ()}) |> toThrowMessage "The type \"!String\" is malformed");
  });

  describe "with initial_optional = true" (fun _ => {
    let trt = to_reason_type initial_optional::true scalar_to_reason;

    test "[String]!" (fun _ => expect (trt "[String]!") == "option (list (option (string)))");
    test "[String]" (fun _ => expect (trt "[String]") == "option (list (option (string)))");
    test "String" (fun _ => expect (trt "String") == "option (string)");
    test "String!" (fun _ => expect (trt "String!") == "option (string)");
    test "[String!]!" (fun _ => expect (trt "[String!]!") == "option (list (string))");
    test "[[String]]" (fun _ => expect (trt "[[String]]") == "option (list (option (list (option (string)))))");
    test "[[String!]!]!" (fun _ => expect (trt "[[String!]!]!") == "option (list (list (string)))");
  });
});

describe "to_converter" (fun () => {
  open Expect;
  open! Expect.Operators;
  open Acr.Type_parser;

  let toc type_ => to_converter initial_optional::false field::"data##field" initial_mapper::"(fun x => x)" ::type_;

  test "Bool!" (fun _ => expect (toc "Bool!") == "(data##field) |> Js.to_bool");
  test "Bool" (fun _ => expect (toc "Bool") == "(data##field) |> (fun x => switch (x |> Js.Null_undefined.to_opt) {
  | None => None
  | Some data => Some ((data) |> Js.to_bool)
})");
  test "String!" (fun _ => expect (toc "String!") == "data##field");
  test "String" (fun _ => expect (toc "String") == "(data##field) |> Js.Null_undefined.to_opt");
  test "[String!]!" (fun _ => expect (toc "[String!]!") == "(data##field) |> Array.to_list");
  test "[String!]" (fun _ => expect (toc "[String!]") == "(data##field) |> (fun x => switch (x |> Js.Null_undefined.to_opt) {
  | None => None
  | Some data => Some ((data) |> Array.to_list)
})");
  test "[String]!" (fun _ => expect (toc "[String]!") == "(data##field) |> Array.to_list |> List.map (Js.Null_undefined.to_opt)");
  test "[String]" (fun _ => expect (toc "[String]") == "(data##field) |> (fun x => switch (x |> Js.Null_undefined.to_opt) {
  | None => None
  | Some data => Some ((data) |> Array.to_list |> List.map (Js.Null_undefined.to_opt))
})");
  test "[Bool]" (fun _ => expect (toc "[Bool]") == "(data##field) |> (fun x => switch (x |> Js.Null_undefined.to_opt) {
  | None => None
  | Some data => Some ((data) |> Array.to_list |> List.map ((fun x => switch (x |> Js.Null_undefined.to_opt) {
  | None => None
  | Some data => Some ((data) |> Js.to_bool)
})))
})");
  test "[Bool]!" (fun _ => expect (toc "[Bool]!") == "(data##field) |> Array.to_list |> List.map ((fun x => switch (x |> Js.Null_undefined.to_opt) {
  | None => None
  | Some data => Some ((data) |> Js.to_bool)
}))");
});

describe "to_js_converter" (fun () => {
  open Expect;
  open! Expect.Operators;
  open Acr.Type_parser;

  let tojsc type_ => to_js_converter field::"data.field" ::type_;

  test "Bool!" (fun _ => expect (tojsc "Bool!") == "(data.field) |> Js.Boolean.to_js_boolean");
  test "Bool" (fun _ => expect (tojsc "Bool") == "switch (data.field) {
  | None => Js.null
  | Some b => Some (b |> Js.Boolean.to_js_boolean) |> Js.Null.from_opt
}");
  test "NullableBoolean" (fun _ => expect (tojsc "NullableBoolean") == "switch (data.field) {
  | None => Js.null
  | Some b => Some (b |> Js.Boolean.to_js_boolean) |> Js.Null.from_opt
}");
  test "String!" (fun _ => expect (tojsc "String!") == "data.field");
  test "String" (fun _ => expect (tojsc "String") == "(data.field) |> Js.Null.from_opt");
});