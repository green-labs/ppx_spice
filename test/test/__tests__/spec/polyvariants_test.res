open Zora

let testEqual = (t, name, lhs, rhs) =>
  t->test(name, async t => {
    t->equal(lhs, rhs, name)
  })

zoraBlock("polymorphic variants with attribute", t => {
  let polyvariantEncoded = #one->Polyvariants.t_encode
  t->testEqual("encode 하나", polyvariantEncoded, JSON.String(`하나`))

  let polyvariantEncoded = #two->Polyvariants.t_encode
  t->testEqual("encode 둘", polyvariantEncoded, JSON.String(`둘`))

  let polyvariantDecoded = JSON.String(`하나`)->Polyvariants.t_decode
  t->testEqual("decode 하나", polyvariantDecoded, Ok(#one))

  let polyvariantDecoded = JSON.String(`둘`)->Polyvariants.t_decode
  t->testEqual("decode 둘", polyvariantDecoded, Ok(#two))
})

zoraBlock("polymorphic variants", t => {
  let polyvariantEncoded = #one->Polyvariants.t1_encode
  t->testEqual(`encode #one`, polyvariantEncoded, JSON.Array([JSON.String(`one`)]))

  let polyvariantEncoded = #two->Polyvariants.t1_encode
  t->testEqual(`encode #two`, polyvariantEncoded, JSON.Array([JSON.String(`two`)]))

  let polyvariantDecoded = JSON.Array([JSON.String(`one`)])->Polyvariants.t1_decode
  t->testEqual(`decode one`, polyvariantDecoded, Ok(#one))

  let polyvariantDecoded = JSON.Array([JSON.String(`two`)])->Polyvariants.t1_decode
  t->testEqual(`decode two`, polyvariantDecoded, Ok(#two))
})

zoraBlock("polymorphic variants with @spice.as number", t => {
  let polyvariantEncoded = #one->Polyvariants.t2_encode
  t->testEqual("encode 1.0", polyvariantEncoded, JSON.Number(1.0))

  let polyvariantEncoded = #two->Polyvariants.t2_encode
  t->testEqual("encode 2.0", polyvariantEncoded, JSON.Number(2.0))

  let polyvariantDecoded = JSON.Number(1.0)->Polyvariants.t2_decode
  t->testEqual("decode 1.0", polyvariantDecoded, Ok(#one))

  let polyvariantDecoded = JSON.Number(2.0)->Polyvariants.t2_decode
  t->testEqual("decode 2.0", polyvariantDecoded, Ok(#two))
})

zoraBlock("polymorphic variants with arguments", t => {
  // #None - no args
  let encoded = #None->Polyvariants.t3_encode
  t->testEqual("encode #None", encoded, JSON.Array([JSON.String("None")]))

  let decoded = JSON.Array([JSON.String("None")])->Polyvariants.t3_decode
  t->testEqual("decode #None", decoded, Ok(#None))

  // #Single(int) - one arg
  let encoded = #Single(42)->Polyvariants.t3_encode
  t->testEqual("encode #Single", encoded, JSON.Array([JSON.String("Single"), JSON.Number(42.0)]))

  let decoded = JSON.Array([JSON.String("Single"), JSON.Number(42.0)])->Polyvariants.t3_decode
  t->testEqual("decode #Single", decoded, Ok(#Single(42)))

  // #Multiple(string, int, bool) - multiple args
  let encoded = #Multiple("hello", 123, true)->Polyvariants.t3_encode
  t->testEqual(
    "encode #Multiple",
    encoded,
    JSON.Array([
      JSON.String("Multiple"),
      JSON.String("hello"),
      JSON.Number(123.0),
      JSON.Boolean(true),
    ]),
  )

  let decoded =
    JSON.Array([
      JSON.String("Multiple"),
      JSON.String("hello"),
      JSON.Number(123.0),
      JSON.Boolean(true),
    ])->Polyvariants.t3_decode
  t->testEqual("decode #Multiple", decoded, Ok(#Multiple("hello", 123, true)))

  // Error cases - test nested error paths
  let decoded =
    JSON.Array([JSON.String("Single"), JSON.String("not an int")])->Polyvariants.t3_decode
  t->test("decode #Single with wrong type", async t => {
    switch decoded {
    | Error({path}) => t->equal(path, "[1]", "error path should be [1]")
    | Ok(_) => t->fail("should have failed")
    }
  })

  let decoded =
    JSON.Array([
      JSON.String("Multiple"),
      JSON.String("hello"),
      JSON.String("not an int"),
      JSON.Boolean(true),
    ])->Polyvariants.t3_decode
  t->test("decode #Multiple with wrong type at index 2", async t => {
    switch decoded {
    | Error({path}) => t->equal(path, "[2]", "error path should be [2]")
    | Ok(_) => t->fail("should have failed")
    }
  })
})
