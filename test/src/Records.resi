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
  n: Js.null<string>,
  on?: Js.null<string>,
  n2: Js.Null.t<string>
}
