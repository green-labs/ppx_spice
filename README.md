# Spice

A ReScript PPX, which generates JSON (de)serializers.

`Spice` is originated from

- The `Spice melange` in the novel, Dune
- A flavor for the (polymorphic) variant

> This PPX is highly influenced by [Decco](https://github.com/reasonml-labs/decco) and developed with forking the source codes of Decco. Spice has implemented all the features of Decco@1.5.0 and additional useful features for the (polymorphic) variant of its own.

## Motivation

1. Parse the string instead of the array to the (polymorphic) variant

To parse the JSON data, [Decco](https://github.com/reasonml-labs/decco) is being heavily used in many projects. But, there's a restriction to parse the JSON string into a (polymorphic) variant with the Decco. The JSON data should be formed in an array. It is obvious at some point. But generally, we face the string data which needs to be parsed into the variant in most use cases.

Whenever it is needed to parse the response in Json, the custom encoder/decoder functions are needed to parse the Json string into the (polymorphic) variant with Decco. But you don't need to write it with the Spice as long as you add `@spice.as` in (polymorphic) variants.

with Decco

```rescript
@decco
type status = WAITING | PROCESSING | SUCCESS | FAIL

let encoderStatus = v =>
  switch v {
  | WAITING => "waiting"
  | PROCESSING => "processing"
  | SUCCESS => "success"
  | FAIL => "fail"
  }->Js.Json.string

let decoderStatus = json => {
  switch json |> Js.Json.classify {
  | Js.Json.JSONString(str) =>
    switch str {
    | "waiting" => WAITING->Ok
    | "processing" => PROCESSING->Ok
    | "success" => SUCCESS->Ok
    | "fail" => FAIL->Ok
    | _ => Error({Decco.path: "", message: "Expected JSONString", value: json})
    }
  | _ => Error({Decco.path: "", message: "Expected JSONString", value: json})
  }
}

let codecStatus: Decco.codec<status> = (encoderStatus, decoderStatus)


@decco
type data = {
  status: @decco.codec(codecStatus) status,
}
```

with Spice

```rescript
@spice
type status =
  | @spice.as("waiting") WAITING
  | @spice.as("processing") PROCESSING
  | @spice.as("success") SUCCESS
  | @spice.as("fail") FAIL

@spice
type data = {
  status: status,
}
```

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

let encoded = One->t_encode // Js.Json.string(`하나`)

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

## Getting Started

Read our [Guide with examples](docs/GUIDE.md)

## Install

```sh
yarn add -D @greenlabs/ppx-spice
```

```json
// bsconfig.json

"bs-dependencies": [
  "@greenlabs/ppx-spice"
],
"ppx-flags": [
  ...,
  "@greenlabs/ppx-spice/ppx"
],
```

## Development

### With Dune

Make sure running the below commands in `/src`.

1. Create a sandbox with opam

```
opam switch create spice 4.12.1
```

2. Install dependencies

```
opam install . --deps-only
```

3. Build

```
dune build
```

4. Test

Make sure running tests in `/test`

```
cd test

(install dependencies)
yarn

(build --watch)
yarn res:clean && yarn res:watch

(run test --watch)
yarn test:watch
```
