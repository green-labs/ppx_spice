@spice
type t = [#one | #two]

@spice
type t1 = [#one | #two]

@spice
type t2 = [@spice.as(1.0) #one | @spice.as(2.0) #two]

// Types for testing error paths
@spice.decode
type withArgs = [#WithArgs(int, string)]

// Generic polyvariants with type parameters
@spice
type t3<'a> = [#Some('a) | #None]

let t3_string_encode: t3<string> => JSON.t
let t3_string_decode: JSON.t => Spice.result<t3<string>>

@spice
type t4<'a, 'b> = [#Left('a) | #Right('b) | #Both('a, 'b)]

let t4_string_int_encode: t4<string, int> => JSON.t
let t4_string_int_decode: JSON.t => Spice.result<t4<string, int>>
