@spice
type t = | @spice.as("m³") M3

module Action = {
  @spice
  type t =
    | @spice.as("create") Create
    | @spice.as("update") Update
    | @spice.as("delete") Delete
}
