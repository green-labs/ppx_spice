// Test case for nested modules with @spice types in signatures
// This tests that the PPX processes @spice types inside nested module signatures

module Inner: {
  @spice
  type t = {
    name: string,
    value: int,
  }
}

module Outer: {
  module Nested: {
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

@spice
type combined = {
  inner: Inner.t,
  nested: Outer.Nested.t,
}
