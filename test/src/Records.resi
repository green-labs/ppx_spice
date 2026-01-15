@spice
type t = {
  label: string,
  value: int,
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

let t5_string_encode: t5<string> => JSON.t
let t5_string_decode: JSON.t => Spice.result<t5<string>>

@spice
type t6<'key, 'value> = {
  key: 'key,
  value: 'value,
}

let t6_string_int_encode: t6<string, int> => JSON.t
let t6_string_int_decode: JSON.t => Spice.result<t6<string, int>>
