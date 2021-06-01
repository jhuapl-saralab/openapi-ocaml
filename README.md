Openapi for Ocaml
=================

I got jealous of the python `fastapi` generated api documentation page
and decided to implement comparable functionality for apps written in
ocaml.

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
App.empty ()
|> App.get "/path/:foo" (fun h -> Response.of_plain_text "hello world")
|> App.run_command
```

The endpoint creation methods provided by `Openapi.Opium.App` accept
additional optional arguments to provide specification information for
to document the endpoint. Check the type signatures of these methods
for more information.


