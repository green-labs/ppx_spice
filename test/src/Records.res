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
type t2 = {
  o: option<string>,
  n: Null.t<string>,
  on?: Null.t<string>,
  n2: Null.t<string>,
}

@spice
type t3 = {
  @spice.default(0) value: int,
  @spice.default(Some(1)) value2?: int,
}

@spice
type t4 = {
  a: bigint,
  b?: bigint,
  c: option<bigint>,
}

@spice
type t5<'data> = {a: array<'data>}

let t5_string_encode = t5_encode(Spice.stringToJson)
let t5_string_decode = t5_decode(Spice.stringFromJson)

@spice
type t6<'key, 'value> = {
  key: 'key,
  value: 'value,
}

let t6_string_int_encode = t6_encode(Spice.stringToJson, Spice.intToJson)
let t6_string_int_decode = t6_decode(Spice.stringFromJson, Spice.intFromJson)
