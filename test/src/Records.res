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
