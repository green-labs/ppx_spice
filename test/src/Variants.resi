@spice
type t = One | Two

@spice
type t1 = One1 | Two1

@spice @unboxed
type t2 = One2(int)

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

let t5_string_encode: t5<string> => JSON.t
let t5_string_decode: JSON.t => Spice.result<t5<string>>

@spice
type t6<'a, 'b> = Left('a) | Right('b) | Both('a, 'b)

let t6_string_int_encode: t6<string, int> => JSON.t
let t6_string_int_decode: JSON.t => Spice.result<t6<string, int>>
