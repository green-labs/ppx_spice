type decodeError = {
  path: string,
  message: string,
  value: Js.Json.t,
}

type result<'a> = result<'a, decodeError>
type decoder<'a> = Js.Json.t => result<'a>
type encoder<'a> = 'a => Js.Json.t
type codec<'a> = (encoder<'a>, decoder<'a>)

let error = (~path=?, message, value) => {
  let path = switch path {
  | None => ""
  | Some(s) => s
  }
  Error({path, message, value})
}

let stringToJson = (s): Js.Json.t => Js.Json.String(s)
let stringFromJson = j =>
  switch (j: Js.Json.t) {
  | Js.Json.String(s) => Ok(s)
  | _ => Error({path: "", message: "Not a string", value: j})
  }

let intToJson = (i): Js.Json.t => Js.Json.Number(Float.fromInt(i))
let intFromJson = j =>
  switch (j: Js.Json.t) {
  | Js.Json.Number(f) =>
    Float.fromInt(Js.Math.floor(f)) == f
      ? Ok(Js.Math.floor(f))
      : Error({path: "", message: "Not an integer", value: j})

  | _ => Error({path: "", message: "Not a number", value: j})
  }

let bigintToJson = (i): Js.Json.t => Js.Json.Number(BigInt.toFloat(i))

let bigintFromJson = j =>
  switch (j: Js.Json.t) {
  | Js.Json.Number(n) => Ok(BigInt.fromFloat(n))
  | _ => error("Not a number", j)
  }

let floatToJson = (v): Js.Json.t => Js.Json.Number(v)
let floatFromJson = j =>
  switch (j: Js.Json.t) {
  | Js.Json.Number(f) => Ok(f)
  | _ => Error({path: "", message: "Not a number", value: j})
  }

let boolToJson = (v): Js.Json.t =>
  switch v {
  | true => Js.Json.Boolean(true)
  | false => Js.Json.Boolean(false)
  }
let boolFromJson = j =>
  switch (j: Js.Json.t) {
  | Js.Json.Boolean(true) => Ok(true)
  | Js.Json.Boolean(false) => Ok(false)
  | _ => Error({path: "", message: "Not a boolean", value: j})
  }

let unitToJson = (): Js.Json.t => Js.Json.Number(0.0)
let unitFromJson = _ => Ok()

let arrayToJson = (encoder, arr): Js.Json.t => Js.Json.Array(Js.Array.map(encoder, arr))

let arrayFromJson = (decoder, json) =>
  switch (json: Js.Json.t) {
  | Js.Json.Array(arr) => Js.Array.reducei((acc, jsonI, i) =>
      switch (acc, decoder(jsonI)) {
      | (Error(_), _) => acc

      | (_, Error({path} as error)) =>
        Error({...error, path: "[" ++ (Int.toString(i) ++ ("]" ++ path))})

      | (Ok(prev), Ok(newVal)) =>
        Ok(Js.Array.concat([newVal], prev))
      }
    , Ok([]), arr)

  | _ => Error({path: "", message: "Not an array", value: json})
  }

let listToJson = (encoder, list) => arrayToJson(encoder, Belt.List.toArray(list))

let listFromJson = (decoder, json) =>
  Belt.Result.map(arrayFromJson(decoder, json), Belt.List.fromArray)

let filterOptional = arr =>
  Belt.Array.keepMap(arr, ((k, v)) => switch v {
    | Some(v) => Some(k, v)
    | None => None
  })

let optionToJson = (encoder, opt): option<Js.Json.t> =>
  switch opt {
  | Some(x) => Some(encoder(x))
  | None => None
  }

let optionFromJson = (decoder, json) =>
  switch (json: Js.Json.t) {
  | Js.Json.Null => Ok(None)
  | _ => Belt.Result.map(decoder(json), v => Some(v))
  }

let nullToJson = (encoder, opt): Js.Json.t =>
  switch opt {
  | Js.Null.Value(x) => encoder(x)
  | Null => Js.Json.Null
  }

let nullFromJson = (decoder, json) =>
  switch (json: Js.Json.t) {
  | Js.Json.Null => Ok(Js.Null.Null)
  | _ => Belt.Result.map(decoder(json), v => Js.Null.Value(v))
  }

let resultToJson = (okEncoder, errorEncoder, result): Js.Json.t => Js.Json.Array(
  switch result {
  | Ok(v) => [Js.Json.String("Ok"), okEncoder(v)]
  | Error(e) => [Js.Json.String("Error"), errorEncoder(e)]
  },
)

let resultFromJson = (okDecoder, errorDecoder, json) =>
  switch (json: Js.Json.t) {
  | Js.Json.Array([variantConstructorId, payload]) =>
    switch variantConstructorId {
    | Js.Json.String("Ok") => okDecoder(payload)->Belt.Result.map(v => Ok(v))

    | Js.Json.String("Error") =>
      switch errorDecoder(payload) {
      | Ok(v) => Ok(Error(v))
      | Error(e) => Error(e)
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
  let bigint = (bigintToJson, bigintFromJson)
  let float = (floatToJson, floatFromJson)
  let bool = (boolToJson, boolFromJson)
  let array = (arrayToJson, arrayFromJson)
  let list = (listToJson, listFromJson)
  let option = (optionToJson, optionFromJson)
  let unit = (unitToJson, unitFromJson)
}
