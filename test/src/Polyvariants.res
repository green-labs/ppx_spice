@spice
type t = [@spice.as(`하나`) #one | @spice.as(`둘`) #two]

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

let t3_string_encode = t3_encode(Spice.stringToJson)
let t3_string_decode = t3_decode(Spice.stringFromJson)

@spice
type t4<'a, 'b> = [#Left('a) | #Right('b) | #Both('a, 'b)]

let t4_string_int_encode = t4_encode(Spice.stringToJson, Spice.intToJson)
let t4_string_int_decode = t4_decode(Spice.stringFromJson, Spice.intFromJson)
