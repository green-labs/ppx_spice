// Test cases for duplicate attribute error handling
// Uncomment each to verify the expected compile error

// Should error: "Duplicate @spice.key attribute"
// @spice
// type duplicateKey = {
//   @spice.key("foo") @spice.key("bar") name: string,
// }

// Should error: "Duplicate @spice.default attribute"
// @spice
// type duplicateDefault = {@spice.default("a") @spice.default("b") name: string}

