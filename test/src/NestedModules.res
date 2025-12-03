// Test case for nested modules with @spice types
// This tests that the PPX processes @spice types inside nested module structures

module Inner = {
  @spice
  type t = {
    name: string,
    value: int,
  }
}

module Outer = {
  module Nested = {
    @spice
    type t = {
      label: string,
      count: float,
    }
  }

  @spice
  type wrapper = {
    inner: Inner.t,
    nested: Nested.t,
  }
}

// Top-level type that uses nested module types
@spice
type combined = {
  inner: Inner.t,
  nested: Outer.Nested.t,
}

// Mark wrapper codecs as used to avoid warning 32
let _ = Outer.wrapper_encode
let _ = Outer.wrapper_decode
