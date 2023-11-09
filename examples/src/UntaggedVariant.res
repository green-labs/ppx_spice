type t0 = A | B
type t1 = {
  a: string,
  b: int,
}
type t2 = (string, int)

let t0_encode = v =>
  (
    (v): Js.Json.t =>
      switch v {
      | A => Js.Json.Array([Js.Json.String("A")])
      | B => Js.Json.Array([Js.Json.String("B")])
      }
  )(v)

let t0_decode = v =>
  (
    v =>
      switch (v: Js.Json.t) {
      | Js.Json.Array([]) => Spice.error("Expected variant, found empty array", v)
      | Js.Json.Array(json_arr) =>
        switch Belt.Array.getExn(json_arr, 0) {
        | Js.Json.String("A") =>
          if Js.Array.length(json_arr) != 1 {
            Spice.error("Invalid number of arguments to variant constructor", v)
          } else {
            Ok(A)
          }
        | Js.Json.String("B") =>
          if Js.Array.length(json_arr) != 1 {
            Spice.error("Invalid number of arguments to variant constructor", v)
          } else {
            Ok(B)
          }
        | _ => Spice.error("Invalid variant constructor", Belt.Array.getExn(json_arr, 0))
        }
      | _ => Spice.error("Not a variant", v)
      }
  )(v)

let t1_encode = v =>
  (
    (v): Js.Json.t => Js.Json.Object(
      Js.Dict.fromArray(
        Spice.filterOptional([
          ("a", false, Spice.stringToJson(v.a)),
          ("b", false, Spice.intToJson(v.b)),
        ]),
      ),
    )
  )(v)

let t1_decode = v =>
  (
    v =>
      switch (v: Js.Json.t) {
      | Js.Json.Object(dict) =>
        switch Spice.stringFromJson(
          Belt.Option.getWithDefault(Js.Dict.get(dict, "a"), Js.Json.Null),
        ) {
        | Ok(a) =>
          switch Spice.intFromJson(
            Belt.Option.getWithDefault(Js.Dict.get(dict, "b"), Js.Json.null),
          ) {
          | Ok(b) => Ok({a, b})
          | Error(e: Spice.decodeError) =>
            Error({...e, path: "." ++ "b" ++ e.path})
          }
        | Error(e: Spice.decodeError) =>
          Error({...e, path: "." ++ "a" ++ e.path})
        }
      | _ => Spice.error("Not an object", v)
      }
  )(v)

let t2_encode = v =>
  (((v0, v1)): Js.Json.t => Js.Json.Array([Spice.stringToJson(v0), Spice.intToJson(v1)]))(v)

let t2_decode = v =>
  (
    json =>
      switch (json: Js.Json.t) {
      | Js.Json.Array([v0, v1]) =>
        switch (Spice.stringFromJson(v0), Spice.intFromJson(v1)) {
        | (Ok(v0), Ok(v1)) => Ok((v0, v1))
        | (Error(e: Spice.decodeError), _) =>
          Error({...e, path: "[0]" ++ e.path})
        | (_, Error(e: Spice.decodeError)) =>
          Error({...e, path: "[1]" ++ e.path})
        }
      | Js.Json.Array(_) => Spice.error("Incorrect cardinality", json)
      | _ => Spice.error("Not a tuple", json)
      }
  )(v)
