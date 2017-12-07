open Jest;

describe(
  "lexer",
  () => {
    open Expect;
    open! Expect.Operators;
    open Acr.Type_parser.Lexer;
    let lexer = create_lexer("[String]!");
    test("get_token", (_) => expect(get_token(lexer)) == LeftBrace);
    test(
      "inc_lexer |> get_token",
      (_) => expect(lexer |> inc_lexer |> get_token) == Word("String")
    );
    test("get_token", (_) => expect(get_token(lexer)) == LeftBrace);
    let lexer = inc_lexer(lexer);
    test("inc_lexer |> get_token", (_) => expect(lexer |> get_token) == Word("String"));
    let lexer = inc_lexer(lexer);
    test("inc_lexer |> get_token", (_) => expect(lexer |> get_token) == RightBrace);
    let lexer = inc_lexer(lexer);
    test("inc_lexer |> get_token", (_) => expect(lexer |> get_token) == Exclamation);
    let lexer = inc_lexer(lexer);
    test("inc_lexer |> get_token", (_) => expect(lexer |> get_token) == EOS)
  }
);

describe(
  "parser",
  () => {
    open Expect;
    open! Expect.Operators;
    open Acr.Type_parser.Parser;
    test(
      "parse [String]!",
      (_) => expect(parse("[String]!")) == Some(Array(Optional(Scalar("String"))))
    );
    test(
      "parse [String]",
      (_) => expect(parse("[String]")) == Some(Optional(Array(Optional(Scalar("String")))))
    );
    test("parse String", (_) => expect(parse("String")) == Some(Optional(Scalar("String"))));
    test("parse String!", (_) => expect(parse("String!")) == Some(Scalar("String")));
    test("parse [String!]!", (_) => expect(parse("[String!]!")) == Some(Array(Scalar("String"))));
    test(
      "parse [[String]]",
      (_) =>
        expect(parse("[[String]]"))
        == Some(Optional(Array(Optional(Array(Optional(Scalar("String")))))))
    );
    test("parse Bool!", (_) => expect(parse("Bool!]")) == Some(Scalar("Bool")));
    test("parse Boolean!", (_) => expect(parse("Boolean")) == Some(Optional(Scalar("Boolean"))))
  }
);

describe(
  "to_reason_type",
  () => {
    open Expect;
    open! Expect.Operators;
    open Acr.Type_parser;
    describe(
      "with scalar_to_reason",
      (_) => {
        let trt = to_reason_type(scalar_to_reason);
        test("NullableBoolean", (_) => expect(trt("NullableBoolean")) == "option(bool)");
        test(
          "[NullableBoolean]!",
          (_) => expect(trt("[NullableBoolean]!")) == "list(option(bool))"
        );
        test("[String]!", (_) => expect(trt("[String]!")) == "list(option(string))");
        test("[String]", (_) => expect(trt("[String]")) == "option(list(option(string)))");
        test("String", (_) => expect(trt("String")) == "option(string)");
        test("String!", (_) => expect(trt("String!")) == "string");
        test("[String!]!", (_) => expect(trt("[String!]!")) == "list(string)");
        test(
          "[[String]]",
          (_) => expect(trt("[[String]]")) == "option(list(option(list(option(string)))))"
        );
        test("[[String!]!]!", (_) => expect(trt("[[String!]!]!")) == "list(list(string))");
        test(
          "fail with an unknown type",
          (_) =>
            expect(
              (_) => {
                let _ = trt("[Unknown]");
                ()
              }
            )
            |> toThrowMessage("NotYetSupported")
        );
        test(
          "fail with an unknown type",
          (_) =>
            expect(
              (_) => {
                let _ = trt("Unknown");
                ()
              }
            )
            |> toThrowMessage("NotYetSupported")
        );
        test(
          "fail with an malformed type",
          (_) =>
            expect(
              (_) => {
                let _ = trt("[String");
                ()
              }
            )
            |> toThrowMessage("The type \"[String\" is malformed")
        );
        test(
          "fail with an malformed type",
          (_) =>
            expect(
              (_) => {
                let _ = trt("!String");
                ()
              }
            )
            |> toThrowMessage("The type \"!String\" is malformed")
        )
      }
    );
    describe(
      "with custom scalar",
      (_) => {
        let trt = to_reason_type((_) => "my_type");
        test("[String]!", (_) => expect(trt("[String]!")) == "list(option(my_type))");
        test("[String]", (_) => expect(trt("[String]")) == "option(list(option(my_type)))");
        test("String", (_) => expect(trt("String")) == "option(my_type)");
        test("String!", (_) => expect(trt("String!")) == "my_type");
        test("[String!]!", (_) => expect(trt("[String!]!")) == "list(my_type)");
        test(
          "[[String]]",
          (_) => expect(trt("[[String]]")) == "option(list(option(list(option(my_type)))))"
        );
        test("[[String!]!]!", (_) => expect(trt("[[String!]!]!")) == "list(list(my_type))");
        test(
          "fail with an malformed type",
          (_) =>
            expect(
              (_) => {
                let _ = trt("[String");
                ()
              }
            )
            |> toThrowMessage("The type \"[String\" is malformed")
        );
        test(
          "fail with an malformed type",
          (_) =>
            expect(
              (_) => {
                let _ = trt("!String");
                ()
              }
            )
            |> toThrowMessage("The type \"!String\" is malformed")
        )
      }
    );
    describe(
      "with initial_optional = true",
      (_) => {
        let trt = to_reason_type(~initial_optional=true, scalar_to_reason);
        test("[String]!", (_) => expect(trt("[String]!")) == "option(list(option(string)))");
        test("[String]", (_) => expect(trt("[String]")) == "option(list(option(string)))");
        test("String", (_) => expect(trt("String")) == "option(string)");
        test("String!", (_) => expect(trt("String!")) == "option(string)");
        test("[String!]!", (_) => expect(trt("[String!]!")) == "option(list(string))");
        test(
          "[[String]]",
          (_) => expect(trt("[[String]]")) == "option(list(option(list(option(string)))))"
        );
        test(
          "[[String!]!]!",
          (_) => expect(trt("[[String!]!]!")) == "option(list(list(string)))"
        )
      }
    )
  }
);

describe(
  "to_converter",
  () => {
    open Expect;
    open! Expect.Operators;
    open Acr.Type_parser;
    let toc = (type_) =>
      to_converter(
        ~initial_optional=false,
        ~field="data##field",
        ~initial_mapper="((x) => x)",
        ~type_
      );
    test("Bool!", (_) => expect(toc("Bool!")) == "(data##field) |> Js.to_bool");
    test(
      "Bool",
      (_) =>
        expect(toc("Bool"))
        == "(data##field) |> ((x) => switch (x |> Js.Nullable.to_opt) {\n  | None => None\n  | Some(data) => Some((data) |> Js.to_bool)\n})"
    );
    test("String!", (_) => expect(toc("String!")) == "data##field");
    test("String", (_) => expect(toc("String")) == "(data##field) |> Js.Nullable.to_opt");
    test("[String!]!", (_) => expect(toc("[String!]!")) == "(data##field) |> Array.to_list");
    test(
      "[String!]",
      (_) =>
        expect(toc("[String!]"))
        == "(data##field) |> ((x) => switch (x |> Js.Nullable.to_opt) {\n  | None => None\n  | Some(data) => Some((data) |> Array.to_list)\n})"
    );
    test(
      "[String]!",
      (_) =>
        expect(toc("[String]!"))
        == "(data##field) |> Array.to_list |> List.map(Js.Nullable.to_opt)"
    );
    test(
      "[String]",
      (_) =>
        expect(toc("[String]"))
        == "(data##field) |> ((x) => switch (x |> Js.Nullable.to_opt) {\n  | None => None\n  | Some(data) => Some((data) |> Array.to_list |> List.map(Js.Nullable.to_opt))\n})"
    );
    test(
      "[Bool]",
      (_) =>
        expect(toc("[Bool]"))
        == "(data##field) |> ((x) => switch (x |> Js.Nullable.to_opt) {\n  | None => None\n  | Some(data) => Some((data) |> Array.to_list |> List.map(((x) => switch (x |> Js.Nullable.to_opt) {\n  | None => None\n  | Some(data) => Some((data) |> Js.to_bool)\n})))\n})"
    );
    test(
      "[Bool]!",
      (_) =>
        expect(toc("[Bool]!"))
        == "(data##field) |> Array.to_list |> List.map(((x) => switch (x |> Js.Nullable.to_opt) {\n  | None => None\n  | Some(data) => Some((data) |> Js.to_bool)\n}))"
    )
  }
);

describe(
  "to_js_converter",
  () => {
    open Expect;
    open! Expect.Operators;
    open Acr.Type_parser;
    let tojsc = (type_) => to_js_converter(~field="data.field", ~type_);
    test("Bool!", (_) => expect(tojsc("Bool!")) == "(data.field) |> Js.Boolean.to_js_boolean");
    test(
      "Bool",
      (_) =>
        expect(tojsc("Bool"))
        == "switch (data.field) {\n  | None => Js.null\n  | Some(b) => Some(b |> Js.Boolean.to_js_boolean) |> Js.Nullable.from_opt\n}"
    );
    test(
      "NullableBoolean",
      (_) =>
        expect(tojsc("NullableBoolean"))
        == "switch (data.field) {\n  | None => Js.null\n  | Some(b) => Some(b |> Js.Boolean.to_js_boolean) |> Js.Nullable.from_opt\n}"
    );
    test("String!", (_) => expect(tojsc("String!")) == "data.field");
    test("String", (_) => expect(tojsc("String")) == "(data.field) |> Js.Nullable.from_opt")
  }
);
