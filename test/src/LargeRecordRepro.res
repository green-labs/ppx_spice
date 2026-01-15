// Reproduction of exponential compile time bug
// Compile time grows ~4x per 2 additional fields:
// - 20 fields: ~1s
// - 22 fields: ~4s
// - 24 fields: ~17s
// - 26 fields: ~71s
// - 28 fields: >180s (timeout)
// - 31 fields: effectively infinite
//
// The issue is in the generated decoder code which creates nested switch
// statements with exponential complexity in the ReScript type checker.

@spice
type t = {
  f1: option<string>,
  f2: option<string>,
  f3: option<string>,
  f4: option<string>,
  f5: option<string>,
  f6: option<string>,
  f7: option<string>,
  f8: option<string>,
  f9: option<string>,
  f10: option<string>,
  f11: option<string>,
  f12: option<string>,
  f13: option<string>,
  f14: option<string>,
  f15: option<string>,
  f16: option<string>,
  f17: option<string>,
  f18: option<string>,
  f19: option<string>,
  f20: option<string>,
  f21: option<string>,
  f22: option<string>,
  f23: option<string>,
  f24: option<string>,
  f25: option<string>,
  f26: option<string>,
}
