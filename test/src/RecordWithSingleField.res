module M = {
  @spice
  type t = {a: string}
}

@spice
type response = {
  data: option<Js.Json.t>,
  errors: option<array<M.t>>,
}
