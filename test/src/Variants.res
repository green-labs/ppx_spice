@spice
type t = | @spice.as(`하나`) One | @spice.as(`둘`) Two

@spice
type t1 = One1 | Two1

@spice @unboxed
type t2 = | @spice.as(`하나`) One2(int)

@spice @unboxed
type t3 = One3(int)

@spice
type t4 = | @spice.as(1.0) One | @spice.as(2.0) Two
