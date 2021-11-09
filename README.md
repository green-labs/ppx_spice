# Spice

A ReScript PPX, which generates JSON (de)serializers.

`Spice` is originated from
* The `Spice melange` in the novel, Dune
* A flavor for the (polymorphic) variant

> This PPX is highly influenced by [Decco](https://github.com/reasonml-labs/decco) and developed with forking the source codes of Decco. Spice has implemented all the features of Decco@1.5.0 and additional useful features for (polymorphic) variant of its own.

## Motivation

1. Parse the string instead of the array to the (polymorphic) variant

To parse the JSON data, [Decco](https://github.com/reasonml-labs/decco) is being heavily used in many projects. But, there's a restriction to parse the JSON string into a (polymorphic) variant with the Decco. The JSON data should be formed in an array. It is obvious at some point. But generally, we face the string data which needs to be parsed into the variant in most use cases.

2. Parse/stringify the Unicode string

There are many cases to parse and stringify the string data into (polymorphic) variants. Furthermore, the Unicode string needs to be handled with a variant. Currently, pattern matching is not working for the Unicode string in ReScript, the Spice is using `if ... then ... else` to compare the Unicode string in case of adding `@spice.as` attribute.

## Example

1. Variant

```rescript
@spice
type t = | @spice.as(`하나`) One | @spice.as(`second`) Two

// automatically generated
let t_encode = ...

// automatically generated
let t_decode = ...

let encoded = One->t_encode // Js.Json.JSONString(`하나`)

let decoded = Js.Json.string(`second`)->t_decode // Belt.Result.Ok(Two)
```

2. Record

```rescript
@spice
type t = {
  @spice.key("spice-label") label: string,
  @spice.key("spice-value") value: int,
}

let sample = Js.Dict.empty()
sample->Js.Dict.set("spice-label", Js.Json.string("sample"))
sample->Js.Dict.set("spice-value", Js.Json.number(1.0))
let sampleJson = sample->Js.Json.object_

let sampleRecord: t = {
  label: "sample",
  value: 1,
}


let encoded = sampleRecord->Records.t_encode // sampleJson

let decoded = sampleJson->Records.t_decode // Belt.Result.Ok(sampleRecord)
```

## Install

```
yarn add -D @greenlabs/ppx-spice
```

```
// bsconfig.json
"ppx-flags": [
  ...,
  "@greenlabs/ppx-spice/ppx"
],
```

## Development

### With Esy

1. Install dependencies

```
cd src
esy install
```

2. Build

```
(make sure in /src directory)
esy build
```

3. Test

```
cd test

(install dependencies)
yarn

(build --watch)
yarn res:clean && yarn res:watch

(run test --watch)
yarn test:watch
```

### With Dune

1. Create a sandbox with opam

```
opam switch create spice 4.12.1
```

2. Install dependencies

```
opam install dune ppxlib ocaml-lsp-server ocamlformat ocp-indent
```

3. Build

```
dune build
```

4. Test

```
cd test

(install dependencies)
yarn

(build --watch)
yarn res:clean && yarn res:watch

(run test --watch)
yarn test:watch
```
