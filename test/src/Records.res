@spice
type t = {
  @spice.key("spice-label") label: string,
  @spice.key("spice-value") value: int,
}

@spice
type t1 = {
  label: string,
  value: int,
}

@spice
type tOp = {
  label: option<string>,
  value?: int,
}

@spice
type t2 = {nullable?: string}

// let de = v =>
//   switch (v: Js.Json.t) {
//   | Js.Json.Object(dict) =>
//     switch (Spice.optionFromJson(Spice.stringFromJson, ...))(
//       Belt.Option.getWithDefault(Js.Dict.get(dict, "label"), Js.Json.null),
//     ) {
//     | Ok(label) =>
//       switch (Spice.optionFromJson(Spice.intFromJson, ...))(
//         Belt.Option.getWithDefault(Js.Dict.get(dict, "value"), Js.Json.null),
//       ) {
//       | Ok(value) => Ok({label, value: ?value})
//       | Error(e: Spice.decodeError) => Error(e)
//       }
//     | Error(e: Spice.decodeError) => Error(e)
//     }
//   | _ => Spice.error("", v)
//   }
