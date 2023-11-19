@spice
type t = [@spice.as(`하나`) #one | @spice.as(`둘`) #two]

@spice
type t1 = [#one | #two]

@spice
type t2 = [@spice.as(1.0) #one | @spice.as(2.0) #two]
