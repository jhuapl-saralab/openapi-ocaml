Openapi for Ocaml
=================

This library provides basic types for parsing/serializing openapi
specifications and a nearly drop-in replacement for `Opium.App` that
registers endpoints in a specification and serves `openapi.json` and
`docs` pages based on the `fastapi` implementation.

Usage
=====

Basic usage should work equivalently to `Opium.App`. So instead of
```
let open Opium in
App.empty
|> App.get "/path/:foo" (fun h -> Response.of_plain_text "hello world")
|> App.run_command
```

Just do:
```
let open Opium in
let module App = Openapi.App in
App.empty
|> App.get "/path/:foo" (fun h -> Response.of_plain_text "hello world")
|> App.run_command
```

The endpoint creation methods provided by `Openapi.Opium.App` accept
additional optional arguments to provide specification information for
to document the endpoint. Check the type signatures of these methods
for more information.

Demo App
========
This library includes a demo app in `bin/demo` (that won't be installed
by `opam`). To see the generated spec/documentation in action, run:
```
$ dune build
$ ./_build/default/bin/demo.exe
```
Then browse to http://localhost:8888/docs

Ppx Deriving Json Schema
========================

The openapi-ocaml repo also defines a ppx deriving plugin for
generating JSON Schema values from OCaml type definitions. It supports
record, polymorphic variant, tuple, and union datatypes.

Parameterized types are not properly handled.

The types `'a list` and `'a option` are handled specially using the
JSON schema `array` type and `oneOf [null; 'a_schema]` respectively.

## Installation

```
$ opam install ppx_deriving_json_schema
```

## Usage

In your ocaml file:
```
type some_type = {int_field : int; string_field : string}
[@@deriving yojson, json_schema]
```

Will generate a value `some_type_schema` defining a JSON schema
compatible with JSON serialization produced by the `ppx_json_conv`
deriver.

## Subschemas

By default, references to other types within the type definition are
reflected as JSON schema references in the generated schema (see
Attributes) below. The reference will take the form
`#/$defs/type_name_schema` for a reference to type `type_name`.

The prefix (`#/$defs/`) can be overriden using `defs` argument to the
deriver, as in:
```
type my_type { x : some_type }
[@@deriving yojson, json_schema {defs = "#/components/schemas"}]
```

## Attributes

The JSON schema generator respects the `@key`, `@yojson.option`,
`@default`, `@yojson.drop_*` and `@name` attributes used by
`ppx_yojson_conv` to direct serialization/deserialization.

Additionally, the attribute `@inline_schema` can be used to direct
schema generation to inline the schema for a type reference (by
inlining the result of `type_name_schema` for a reference to the type
`type_name`) rather than using a schema reference