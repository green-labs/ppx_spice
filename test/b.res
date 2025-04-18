@spice
type t = {
  @spice.key("spice-label") label: string,
  @spice.key("spice-value") value: int,
}
let t_encode = v =>
  (
    (v): Js.Json.t => Js.Json.Object(
      Js.Dict.fromArray(
        Spice.filterOptional([
          ("spice-label", Some(Spice.stringToJson(v.label))),
          ("spice-value", Some(Spice.intToJson(v.value))),
        ]),
      ),
    )
  )(v)
and t_decode = v =>
  (
    v =>
      switch (v: Js.Json.t) {
      | Js.Json.Object(dict) =>
        let label_result = Belt.Option.getWithDefault(
          Belt.Option.map(Js.Dict.get(dict, "spice-label"), Spice.stringFromJson),
          Spice.error(\"^"("spice-label", " missing"), v),
        )
        let value_result = Belt.Option.getWithDefault(
          Belt.Option.map(Js.Dict.get(dict, "spice-value"), Spice.intFromJson),
          Spice.error(\"^"("spice-value", " missing"), v),
        )
        switch (label_result, value_result) {
        | (Ok(label), Ok(value)) => Ok({label, value})
        | (Error(e), _) => Spice.error(~path="spice-label", e.message, e.value)
        | (_, Error(e)) => Spice.error(~path="spice-value", e.message, e.value)
        }
      | _ => Spice.error("Not an object", v)
      }
  )(v)

@spice
type t1 = {
  label: string,
  value: int,
}
let t1_encode = v =>
  (
    (v): Js.Json.t => Js.Json.Object(
      Js.Dict.fromArray(
        Spice.filterOptional([
          (*j"label", Some(Spice.stringToJson(v.label))),
          (*j"value", Some(Spice.intToJson(v.value))),
        ]),
      ),
    )
  )(v)
and t1_decode = v =>
  (
    v =>
      switch (v: Js.Json.t) {
      | Js.Json.Object(dict) =>
        let label_result = Belt.Option.getWithDefault(
          Belt.Option.map(Js.Dict.get(dict, *j"label"), Spice.stringFromJson),
          Spice.error(\"^"(*j"label", " missing"), v),
        )
        let value_result = Belt.Option.getWithDefault(
          Belt.Option.map(Js.Dict.get(dict, *j"value"), Spice.intFromJson),
          Spice.error(\"^"(*j"value", " missing"), v),
        )
        switch (label_result, value_result) {
        | (Ok(label), Ok(value)) => Ok({label, value})
        | (Error(e), _) => Spice.error(~path=*j"label", e.message, e.value)
        | (_, Error(e)) => Spice.error(~path=*j"value", e.message, e.value)
        }
      | _ => Spice.error("Not an object", v)
      }
  )(v)

@spice
type tOp = {
  label: option<string>,
  value?: int,
}
let tOp_encode = v =>
  (
    (v): Js.Json.t => Js.Json.Object(
      Js.Dict.fromArray(
        Spice.filterOptional([
          (*j"label", (Spice.optionToJson(Spice.stringToJson, ...))(v.label)),
          (*j"value", (Spice.optionToJson(Spice.intToJson, ...))(v.value)),
        ]),
      ),
    )
  )(v)
and tOp_decode = v =>
  (
    v =>
      switch (v: Js.Json.t) {
      | Js.Json.Object(dict) =>
        let label_result = Belt.Option.getWithDefault(
          Belt.Option.map(
            Js.Dict.get(dict, *j"label"),
            Spice.optionFromJson(Spice.stringFromJson, ...)
          ),
          Ok(None),
        )
        let value_result = Belt.Option.getWithDefault(
          Belt.Option.map(
            Js.Dict.get(dict, *j"value"),
            Spice.optionFromJson(Spice.intFromJson, ...)
          ),
          Ok(None),
        )
        switch (label_result, value_result) {
        | (Ok(label), Ok(value)) => Ok({label, ?value})
        | (Error(e), _) => Spice.error(~path=*j"label", e.message, e.value)
        | (_, Error(e)) => Spice.error(~path=*j"value", e.message, e.value)
        }
      | _ => Spice.error("Not an object", v)
      }
  )(v)

@spice
type t2 = {
  o: option<string>,
  n: Js.null<string>,
  on?: Js.null<string>,
  n2: Js.Null.t<string>,
}
let t2_encode = v =>
  (
    (v): Js.Json.t => Js.Json.Object(
      Js.Dict.fromArray(
        Spice.filterOptional([
          (*j"o", (Spice.optionToJson(Spice.stringToJson, ...))(v.o)),
          (*j"n", Some((Spice.nullToJson(Spice.stringToJson, ...))(v.n))),
          (*j"on", (Spice.optionToJson(Spice.nullToJson(Spice.stringToJson, ...), ...))(v.on)),
          (*j"n2", Some((Spice.nullToJson(Spice.stringToJson, ...))(v.n2))),
        ]),
      ),
    )
  )(v)
and t2_decode = v =>
  (
    v =>
      switch (v: Js.Json.t) {
      | Js.Json.Object(dict) =>
        let o_result = Belt.Option.getWithDefault(
          Belt.Option.map(
            Js.Dict.get(dict, *j"o"),
            Spice.optionFromJson(Spice.stringFromJson, ...)
          ),
          Ok(None),
        )
        let n_result = Belt.Option.getWithDefault(
          Belt.Option.map(Js.Dict.get(dict, *j"n"), Spice.nullFromJson(Spice.stringFromJson, ...)),
          Spice.error(\"^"(*j"n", " missing"), v),
        )
        let on_result = Belt.Option.getWithDefault(
          Belt.Option.map(
            Js.Dict.get(dict, *j"on"),
            Spice.optionFromJson(Spice.nullFromJson(Spice.stringFromJson, ...), ...)
          ),
          Ok(None),
        )
        let n2_result = Belt.Option.getWithDefault(
          Belt.Option.map(Js.Dict.get(dict, *j"n2"), Spice.nullFromJson(Spice.stringFromJson, ...)),
          Spice.error(\"^"(*j"n2", " missing"), v),
        )
        switch (o_result, n_result, on_result, n2_result) {
        | (Ok(o), Ok(n), Ok(on), Ok(n2)) => Ok({o, n, ?on, n2})
        | (Error(e), _, _, _) => Spice.error(~path=*j"o", e.message, e.value)
        | (_, Error(e), _, _) => Spice.error(~path=*j"n", e.message, e.value)
        | (_, _, Error(e), _) => Spice.error(~path=*j"on", e.message, e.value)
        | (_, _, _, Error(e)) => Spice.error(~path=*j"n2", e.message, e.value)
        }
      | _ => Spice.error("Not an object", v)
      }
  )(v)

@spice
type t3 = {
  @spice.default(0) value: int,
  @spice.default(Some(1)) value2?: int,
}
let t3_encode = v =>
  (
    (v): Js.Json.t => Js.Json.Object(
      Js.Dict.fromArray(
        Spice.filterOptional([
          (*j"value", Some(Spice.intToJson(v.value))),
          (*j"value2", (Spice.optionToJson(Spice.intToJson, ...))(v.value2)),
        ]),
      ),
    )
  )(v)
and t3_decode = v =>
  (
    v =>
      switch (v: Js.Json.t) {
      | Js.Json.Object(dict) =>
        let value_result = Belt.Option.getWithDefault(
          Belt.Option.map(Js.Dict.get(dict, *j"value"), Spice.intFromJson),
          Ok(0),
        )
        let value2_result = Belt.Option.getWithDefault(
          Belt.Option.map(
            Js.Dict.get(dict, *j"value2"),
            Spice.optionFromJson(Spice.intFromJson, ...)
          ),
          Ok(Some(1)),
        )
        switch (value_result, value2_result) {
        | (Ok(value), Ok(value2)) => Ok({value, ?value2})
        | (Error(e), _) => Spice.error(~path=*j"value", e.message, e.value)
        | (_, Error(e)) => Spice.error(~path=*j"value2", e.message, e.value)
        }
      | _ => Spice.error("Not an object", v)
      }
  )(v)

@spice
type t4 = {
  a: bigint,
  b?: bigint,
  c: option<bigint>,
}
let t4_encode = v =>
  (
    (v): Js.Json.t => Js.Json.Object(
      Js.Dict.fromArray(
        Spice.filterOptional([
          (*j"a", Some(Spice.bigintToJson(v.a))),
          (*j"b", (Spice.optionToJson(Spice.bigintToJson, ...))(v.b)),
          (*j"c", (Spice.optionToJson(Spice.bigintToJson, ...))(v.c)),
        ]),
      ),
    )
  )(v)
and t4_decode = v =>
  (
    v =>
      switch (v: Js.Json.t) {
      | Js.Json.Object(dict) =>
        let a_result = Belt.Option.getWithDefault(
          Belt.Option.map(Js.Dict.get(dict, *j"a"), Spice.bigintFromJson),
          Spice.error(\"^"(*j"a", " missing"), v),
        )
        let b_result = Belt.Option.getWithDefault(
          Belt.Option.map(
            Js.Dict.get(dict, *j"b"),
            Spice.optionFromJson(Spice.bigintFromJson, ...)
          ),
          Ok(None),
        )
        let c_result = Belt.Option.getWithDefault(
          Belt.Option.map(
            Js.Dict.get(dict, *j"c"),
            Spice.optionFromJson(Spice.bigintFromJson, ...)
          ),
          Ok(None),
        )
        switch (a_result, b_result, c_result) {
        | (Ok(a), Ok(b), Ok(c)) => Ok({a, ?b, c})
        | (Error(e), _, _) => Spice.error(~path=*j"a", e.message, e.value)
        | (_, Error(e), _) => Spice.error(~path=*j"b", e.message, e.value)
        | (_, _, Error(e)) => Spice.error(~path=*j"c", e.message, e.value)
        }
      | _ => Spice.error("Not an object", v)
      }
  )(v)

