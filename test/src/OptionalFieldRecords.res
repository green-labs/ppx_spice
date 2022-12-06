@spice
type t0 = {
  a: int,
  b?: int,
}

@spice
type t1 = {
  a: int,
  bs?: array<int>,
}

@spice
type b =
  | @spice.as("B0") B0
  | @spice.as("B1") B1
  | @spice.as("B2") B2

@spice
type t2 = {
  a: int,
  bs?: array<b>,
}
