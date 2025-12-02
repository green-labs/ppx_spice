type t0 = A | B
type t1 = {
  a: string,
  b: int,
}
type t2 = (string, int)

let t0_encode = v =>
  (
    (v): JSON.t =>
      switch v {
      | A => JSON.Array([JSON.String("A")])
      | B => JSON.Array([JSON.String("B")])
      }
  )(v)

let t0_decode = v =>
  (
    v =>
      switch (v: JSON.t) {
      | JSON.Array([]) => Spice.error("Expected variant, found empty array", v)
      | JSON.Array(json_arr) =>
        switch Array.getUnsafe(json_arr, 0) {
        | JSON.String("A") =>
          if Array.length(json_arr) != 1 {
            Spice.error("Invalid number of arguments to variant constructor", v)
          } else {
            Ok(A)
          }
        | JSON.String("B") =>
          if Array.length(json_arr) != 1 {
            Spice.error("Invalid number of arguments to variant constructor", v)
          } else {
            Ok(B)
          }
        | _ => Spice.error("Invalid variant constructor", Array.getUnsafe(json_arr, 0))
        }
      | _ => Spice.error("Not a variant", v)
      }
  )(v)

let t1_encode = v =>
  (
    (v): JSON.t => JSON.Object(
      Dict.fromArray(
        Spice.filterOptional([
          ("a", Some(Spice.stringToJson(v.a))),
          ("b", Some(Spice.intToJson(v.b))),
        ]),
      ),
    )
  )(v)

let t1_decode = v =>
  (
    v =>
      switch (v: JSON.t) {
      | JSON.Object(dict) =>
        switch Spice.stringFromJson(Option.getOr(Dict.get(dict, "a"), JSON.Null)) {
        | Ok(a) =>
          switch Spice.intFromJson(Option.getOr(Dict.get(dict, "b"), JSON.Null)) {
          | Ok(b) => Ok({a, b})
          | Error(e: Spice.decodeError) => Error({...e, path: "." ++ "b" ++ e.path})
          }
        | Error(e: Spice.decodeError) => Error({...e, path: "." ++ "a" ++ e.path})
        }
      | _ => Spice.error("Not an object", v)
      }
  )(v)

let t2_encode = v =>
  (((v0, v1)): JSON.t => JSON.Array([Spice.stringToJson(v0), Spice.intToJson(v1)]))(v)

let t2_decode = v =>
  (
    json =>
      switch (json: JSON.t) {
      | JSON.Array([v0, v1]) =>
        switch (Spice.stringFromJson(v0), Spice.intFromJson(v1)) {
        | (Ok(v0), Ok(v1)) => Ok((v0, v1))
        | (Error(e: Spice.decodeError), _) => Error({...e, path: "[0]" ++ e.path})
        | (_, Error(e: Spice.decodeError)) => Error({...e, path: "[1]" ++ e.path})
        }
      | JSON.Array(_) => Spice.error("Incorrect cardinality", json)
      | _ => Spice.error("Not a tuple", json)
      }
  )(v)
