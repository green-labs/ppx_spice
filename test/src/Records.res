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
  n: Js.null<string>,
  on?: Js.null<string>,
  n2: Js.Null.t<string>
}

@spice
type t3 = {
  @spice.default(0) value: int
  @spice.default(Some(1)) value2?: int
}
