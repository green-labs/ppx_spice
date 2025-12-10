module M = {
  @spice
  type t = {a: string}
}

@spice
type response = {
  data: option<JSON.t>,
  errors: option<array<M.t>>,
}
