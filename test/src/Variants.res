@spice
type t = | @spice.as(1.) One | @spice.as("둘") Two

@spice
type t1 = One1 | Two1

@spice @unboxed
type t2 = | @spice.as(`하나`) One2(int)

@spice @unboxed
type t3 = One3(int)

@spice
type t4 = | @spice.as(1.0) One | @spice.as(2.0) Two

// Types for testing error paths
@spice.decode
type withArgs = WithArgs(int, string)

// Generic variants with type parameters
@spice
type t5<'a> = Some('a) | None

let t5_string_encode = t5_encode(Spice.stringToJson)
let t5_string_decode = t5_decode(Spice.stringFromJson)

@spice
type t6<'a, 'b> = Left('a) | Right('b) | Both('a, 'b)

let t6_string_int_encode = t6_encode(Spice.stringToJson, Spice.intToJson)
let t6_string_int_decode = t6_decode(Spice.stringFromJson, Spice.intFromJson)
