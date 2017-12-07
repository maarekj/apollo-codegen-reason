module Ast = Acr_ast;

module Generator = Acr_generator;

let queryJsonFile = Node.Process.argv[2];

let queryJsonStr = Node.Fs.readFileSync(queryJsonFile, `utf8);

external magic : 'a => Js.t({..}) = "%identity";

let queryJson = magic(Js.Json.parseExn(queryJsonStr));

let queryJson = Ast.from_json(queryJson);

Js.log(Generator.generate(queryJson));
