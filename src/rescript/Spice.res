type decodeError = {
  path: string,
  message: string,
  value: Js.Json.t,
}

type result<'a> = Belt.Result.t<'a, decodeError>
type decoder<'a> = Js.Json.t => result<'a>
type encoder<'a> = 'a => Js.Json.t
type codec<'a> = (encoder<'a>, decoder<'a>)

let error = (~path=?, message, value) => {
  let path = switch path {
  | None => ""
  | Some(s) => s
  }
  Belt.Result.Error({path, message, value})
}

let stringToJson = s => Js.Json.string(s)
let stringFromJson = j =>
  switch Js.Json.decodeString(j) {
  | Some(s) => Belt.Result.Ok(s)
  | None => Belt.Result.Error({path: "", message: "Not a string", value: j})
  }

let intToJson = i => i |> float_of_int |> Js.Json.number
let intFromJson = j =>
  switch Js.Json.decodeNumber(j) {
  | Some(f) =>
    float_of_int(Js.Math.floor(f)) == f
      ? Belt.Result.Ok(Js.Math.floor(f))
      : Belt.Result.Error({path: "", message: "Not an integer", value: j})

  | _ => Belt.Result.Error({path: "", message: "Not a number", value: j})
  }

let int64ToJson = i => i |> Int64.float_of_bits |> Js.Json.number

let int64FromJson = j =>
  switch Js.Json.decodeNumber(j) {
  | Some(n) => Belt.Result.Ok(Int64.bits_of_float(n))
  | None => error("Not a number", j)
  }

let int64ToJsonUnsafe = i => i |> Int64.to_float |> Js.Json.number

let int64FromJsonUnsafe = j =>
  switch Js.Json.decodeNumber(j) {
  | Some(n) => Belt.Result.Ok(Int64.of_float(n))
  | None => error("Not a number", j)
  }

let floatToJson = v => v |> Js.Json.number
let floatFromJson = j =>
  switch Js.Json.decodeNumber(j) {
  | Some(f) => Belt.Result.Ok(f)
  | None => Belt.Result.Error({path: "", message: "Not a number", value: j})
  }

let boolToJson = v => v |> Js.Json.boolean
let boolFromJson = j =>
  switch Js.Json.decodeBoolean(j) {
  | Some(b) => Belt.Result.Ok(b)
  | None => Belt.Result.Error({path: "", message: "Not a boolean", value: j})
  }

let unitToJson = () => Js.Json.number(0.0)
let unitFromJson = _ => Belt.Result.Ok()

let arrayToJson = (encoder, arr) => arr |> Js.Array.map(encoder) |> Js.Json.array

let arrayFromJson = (decoder, json) =>
  switch Js.Json.decodeArray(json) {
  | Some(arr) => Js.Array.reducei((acc, jsonI, i) =>
      switch (acc, decoder(jsonI)) {
      | (Belt.Result.Error(_), _) => acc

      | (_, Belt.Result.Error({path} as error)) =>
        Belt.Result.Error({...error, path: "[" ++ (string_of_int(i) ++ ("]" ++ path))})

      | (Belt.Result.Ok(prev), Belt.Result.Ok(newVal)) =>
        Belt.Result.Ok(Js.Array.concat([newVal], prev))
      }
    , Belt.Result.Ok([]), arr)

  | None => Belt.Result.Error({path: "", message: "Not an array", value: json})
  }

let listToJson = (encoder, list) => list |> Array.of_list |> arrayToJson(encoder)

let listFromJson = (decoder, json) =>
  json |> arrayFromJson(decoder) |> Belt.Result.map(_, Array.to_list)

let optionToJson = (encoder, opt) =>
  switch opt {
  | Some(x) => encoder(x)
  | None => Js.Json.null
  }

let filterOptional = arr =>
  arr
  |> Belt.Array.keep(_, ((_, isOptional, x)) => !(isOptional && x == Js.Json.null))
  |> Belt.Array.map(_, ((k, _, v)) => (k, v))

let optionFromJson = (decoder, json) =>
  switch Js.Json.decodeNull(json) {
  | Some(_) => Belt.Result.Ok(None)
  | None => decoder(json) |> Belt.Result.map(_, v => Some(v))
  }

let resultToJson = (okEncoder, errorEncoder, result) =>
  switch result {
  | Belt.Result.Ok(v) => [Js.Json.string("Ok"), okEncoder(v)]
  | Belt.Result.Error(e) => [Js.Json.string("Error"), errorEncoder(e)]
  } |> Js.Json.array

let resultFromJson = (okDecoder, errorDecoder, json) =>
  switch Js.Json.decodeArray(json) {
  | Some([variantConstructorId, payload]) =>
    switch Js.Json.decodeString(variantConstructorId) {
    | Some("Ok") => okDecoder(payload)->Belt.Result.map(v => Belt.Result.Ok(v))

    | Some("Error") =>
      switch errorDecoder(payload) {
      | Belt.Result.Ok(v) => Belt.Result.Ok(Belt.Result.Error(v))
      | Belt.Result.Error(e) => Belt.Result.Error(e)
      }

    | Some(_) => error("Expected either \"Ok\" or \"Error\"", variantConstructorId)
    | None => error("Not a string", variantConstructorId)
    }
  | Some(_) => error("Expected exactly 2 values in array", json)
  | None => error("Not an array", json)
  }

let dictToJson = (encoder, dict) => dict->Js.Dict.map((. a) => encoder(a), _)->Js.Json.object_

let dictFromJson = (decoder, json) =>
  switch Js.Json.decodeObject(json) {
  | Some(dict) =>
    dict
    ->Js.Dict.entries
    ->Belt.Array.reduce(Ok(Js.Dict.empty()), (acc, (key, value)) =>
      switch (acc, decoder(value)) {
      | (Error(_), _) => acc

      | (_, Error({path} as error)) => Error({...error, path: "." ++ (key ++ path)})

      | (Ok(prev), Ok(newVal)) =>
        let () = prev->Js.Dict.set(key, newVal)
        Ok(prev)
      }
    )
  | None => Error({path: "", message: "Not a dict", value: json})
  }

module Codecs = {
  include Spice_Codecs
  let string = (stringToJson, stringFromJson)
  let int = (intToJson, intFromJson)
  let int64Unsafe = (int64ToJsonUnsafe, int64FromJsonUnsafe)
  let float = (floatToJson, floatFromJson)
  let bool = (boolToJson, boolFromJson)
  let array = (arrayToJson, arrayFromJson)
  let list = (listToJson, listFromJson)
  let option = (optionToJson, optionFromJson)
  let unit = (unitToJson, unitFromJson)
}
