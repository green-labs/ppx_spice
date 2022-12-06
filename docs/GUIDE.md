# Ppx_spice Guide

This document walks you through the basics of using ppx_spice. You can try with examples in the `/examples` directory.

## Guide Index

- [Basic Usage](#basic-usage)
- [Variants](#variants)
- [Advanced Usage](#advanced-usage)

### Basic Usage

The following example shows how to use ppx_spice to encode and decode a JSON value.

```rescript
let data = %raw(`
  {
    "id": 1,
    "nickname": "bob"
  }
`)

@spice
type user = {
  id: int,
  nickname?: string,
}

let user: Belt.Result.t<user, Spice.decodeError> = data->user_decode // user_decode is generated by ppx_spice

let json: Js.Json.t = user->Belt.Result.getExn->user_encode // user_encode is generated by ppx_spice
```

### Variants

The following example shows how to use ppx_spice to encode and decode a variant.

```rescript
let data = %raw(`
  {
    "id": 1,
    "nickname": "bob",
    "language": "ReScript"
  }
`)

@spice
type language =
  | @spice.as("ReScript") ReScript
  | @spice.as("OCaml") OCaml
  | @spice.as("TypeScript") TypeScript
  | @spice.as("JavaScript") JavaScript

@spice
type user = {
  id: int,
  nickname?: string,
  language: language,
}

let user: Belt.Result.t<user, Spice.decodeError> = json->user_decode

let json: Js.Json.t = user->Result.getExn->user_encode
```

`@spice.as("...")` is used to specify the JSON value of the variant. Without it, the variant name is also used as the JSON value, but the JSON value should be formed as an array to be parsed as a variant. The second example shows how to use it for the case of variant with argument.

```rescript
@spice
type language =
  | ReScript(string) // <- with the argument
  | OCaml
  | TypeScript
  | JavaScript

@spice
type user = {
  id: int,
  nickname?: string,
  language: language,
}

let data = %raw(`
  {
    "id": 1,
    "nickname": "bob",
    "language": ["ReScript", "awesome"] // <- This is the array
  }
`)
```

### Advanced Usage

The following example shows how to use ppx_spice to encode and decode a JSON value with a custom codec.

```rescript
@spice
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
    | _ => Error({Spice.path: "", message: "Expected JSONString", value: json})
    }
  | _ => Error({Spice.path: "", message: "Expected JSONString", value: json})
  }
}

let codecStatus: Spice.codec<status> = (encoderStatus, decoderStatus)


@spice
type data = {
  status: @spice.codec(codecStatus) status,
}

let data = %raw(`
  {
    "status": "success"
  }
`)

let data = data->data_decode

let json = data->Result.getExn->data_encode
```