type inspectOpts = {
  .
  "showHidden": bool,
  "depth": int,
  "colors": bool
};

[@bs.module "util"] external inspect : ('a, inspectOpts) => 'b = "inspect";

let log_inspect = (~depth=10, obj) =>
  Js.log(inspect(obj, {"showHidden": true, "depth": depth, "colors": true}));

let first_or_fail = (l) =>
  switch l {
  | [] => Js.Exn.raiseError("The list must be not empty")
  | [x, ..._] => x
  };

let def = Js.Option.default;

let join = (glue, list) => {
  let rec _join = (list, acc) =>
    switch list {
    | [] => acc
    | [a] => acc ++ a
    | [a, ...rest] => _join(rest, acc ++ a ++ glue)
    };
  _join(list, "")
};

let capitalize = (str) =>
  Js.String.toUpperCase(Js.String.get(str, 0))
  ++ Js.String.slice(~from=1, ~to_=Js.String.length(str), str);

let space_regex = Js.Re.fromStringWithFlags("[\t\n\r ]+", ~flags="g");

let start_space_regex = Js.Re.fromString("^[\t\n\r ]+");

let end_space_regex = Js.Re.fromString("[\t\n\r ]+$");

let clear_space = (str) => Js.String.replaceByRe(space_regex, " ", str);

let left_trim_space = (str) => Js.String.replaceByRe(start_space_regex, "", str);

let right_trim_space = (str) => Js.String.replaceByRe(end_space_regex, "", str);

let trim_space = (str) => str |> left_trim_space |> right_trim_space;
