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

let stringToJson = (s): Js.Json.t => Js.Json.String(s)
let stringFromJson = j =>
  switch (j: Js.Json.t) {
  | Js.Json.String(s) => Belt.Result.Ok(s)
  | _ => Belt.Result.Error({path: "", message: "Not a string", value: j})
  }

let intToJson = (i): Js.Json.t => Js.Json.Number(float_of_int(i))
let intFromJson = j =>
  switch (j: Js.Json.t) {
  | Js.Json.Number(f) =>
    float_of_int(Js.Math.floor(f)) == f
      ? Belt.Result.Ok(Js.Math.floor(f))
      : Belt.Result.Error({path: "", message: "Not an integer", value: j})

  | _ => Belt.Result.Error({path: "", message: "Not a number", value: j})
  }

let int64ToJson = (i): Js.Json.t => Js.Json.Number(Int64.float_of_bits(i))

let int64FromJson = j =>
  switch (j: Js.Json.t) {
  | Js.Json.Number(n) => Belt.Result.Ok(Int64.bits_of_float(n))
  | _ => error("Not a number", j)
  }

let int64ToJsonUnsafe = (i): Js.Json.t => Js.Json.number(Int64.to_float(i))

let int64FromJsonUnsafe = j =>
  switch (j: Js.Json.t) {
  | Js.Json.Number(n) => Belt.Result.Ok(Int64.of_float(n))
  | _ => error("Not a number", j)
  }

let floatToJson = (v): Js.Json.t => Js.Json.Number(v)
let floatFromJson = j =>
  switch (j: Js.Json.t) {
  | Js.Json.Number(f) => Belt.Result.Ok(f)
  | _ => Belt.Result.Error({path: "", message: "Not a number", value: j})
  }

let boolToJson = (v): Js.Json.t =>
  switch v {
  | true => Js.Json.Boolean(true)
  | false => Js.Json.Boolean(false)
  }
let boolFromJson = j =>
  switch (j: Js.Json.t) {
  | Js.Json.Boolean(true) => Belt.Result.Ok(true)
  | Js.Json.Boolean(false) => Belt.Result.Ok(false)
  | _ => Belt.Result.Error({path: "", message: "Not a boolean", value: j})
  }

let unitToJson = (): Js.Json.t => Js.Json.Number(0.0)
let unitFromJson = _ => Belt.Result.Ok()

let arrayToJson = (encoder, arr): Js.Json.t => Js.Json.Array(Js.Array.map(encoder, arr))

let arrayFromJson = (decoder, json) =>
  switch (json: Js.Json.t) {
  | Js.Json.Array(arr) => Js.Array.reducei((acc, jsonI, i) =>
      switch (acc, decoder(jsonI)) {
      | (Belt.Result.Error(_), _) => acc

      | (_, Belt.Result.Error({path} as error)) =>
        Belt.Result.Error({...error, path: "[" ++ (string_of_int(i) ++ ("]" ++ path))})

      | (Belt.Result.Ok(prev), Belt.Result.Ok(newVal)) =>
        Belt.Result.Ok(Js.Array.concat([newVal], prev))
      }
    , Belt.Result.Ok([]), arr)

  | _ => Belt.Result.Error({path: "", message: "Not an array", value: json})
  }

let listToJson = (encoder, list) => arrayToJson(encoder, Belt.List.toArray(list))

let listFromJson = (decoder, json) =>
  Belt.Result.map(arrayFromJson(decoder, json), Belt.List.fromArray)

let filterOptional = arr =>
  Belt.Array.map(
    Belt.Array.keep(arr, ((_, isOptional, x)) => !(isOptional && x == Js.Json.null)),
    ((k, _, v)) => (k, v),
  )

let optionToJson = (encoder, opt): Js.Json.t =>
  switch opt {
  | Some(x) => encoder(x)
  | None => Js.Json.Null
  }

let optionFromJson = (decoder, json) =>
  switch (json: Js.Json.t) {
  | Js.Json.Null => Belt.Result.Ok(None)
  | _ => Belt.Result.map(decoder(json), v => Some(v))
  }

let resultToJson = (okEncoder, errorEncoder, result): Js.Json.t => Js.Json.Array(
  switch result {
  | Belt.Result.Ok(v) => [Js.Json.String("Ok"), okEncoder(v)]
  | Belt.Result.Error(e) => [Js.Json.String("Error"), errorEncoder(e)]
  },
)

let resultFromJson = (okDecoder, errorDecoder, json) =>
  switch (json: Js.Json.t) {
  | Js.Json.Array([variantConstructorId, payload]) =>
    switch variantConstructorId {
    | Js.Json.String("Ok") => okDecoder(payload)->Belt.Result.map(v => Belt.Result.Ok(v))

    | Js.Json.String("Error") =>
      switch errorDecoder(payload) {
      | Belt.Result.Ok(v) => Belt.Result.Ok(Belt.Result.Error(v))
      | Belt.Result.Error(e) => Belt.Result.Error(e)
      }

    | Js.Json.String(_) => error("Expected either \"Ok\" or \"Error\"", variantConstructorId)
    | _ => error("Not a string", variantConstructorId)
    }
  | Js.Json.Array(_) => error("Expected exactly 2 values in array", json)
  | _ => error("Not an array", json)
  }

let dictToJson = (encoder, dict): Js.Json.t => Js.Json.Object(Js.Dict.map((. a) => encoder(a), dict))

let dictFromJson = (decoder, json) =>
  switch (json: Js.Json.t) {
  | Js.Json.Object(dict) =>
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
  | _ => Error({path: "", message: "Not a dict", value: json})
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
