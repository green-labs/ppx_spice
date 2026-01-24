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

zoraBlock("polyvariant error path includes correct index", t => {
  // Polyvariant with args: ["WithArgs", int, string]
  // Index 0 is the constructor name, index 1 is the int, index 2 is the string
  // When index 1 fails, path should be "[1]" not "[0]"
  let invalidJson = JSON.Array([
    JSON.String("WithArgs"),
    JSON.String("not an int"), // should be int at index 1
    JSON.String("valid string"),
  ])

  let decoded = invalidJson->Polyvariants.withArgs_decode
  t->test("error path shows [1] for first argument", async t => {
    switch decoded {
    | Error({path}) => t->equal(path, "[1]", "path should be [1]")
    | Ok(_) => t->fail("expected decode to fail")
    }
  })
})

zoraBlock("generic polyvariant with single type parameter", t => {
  let someEncoded = #Some("hello")->Polyvariants.t3_string_encode
  t->testEqual(
    `encode #Some("hello")`,
    someEncoded,
    JSON.Array([JSON.String("Some"), JSON.String("hello")]),
  )

  let noneEncoded = #None->Polyvariants.t3_string_encode
  t->testEqual(`encode #None`, noneEncoded, JSON.Array([JSON.String("None")]))

  let someDecoded =
    JSON.Array([JSON.String("Some"), JSON.String("hello")])->Polyvariants.t3_string_decode
  t->testEqual(`decode #Some`, someDecoded, Ok(#Some("hello")))

  let noneDecoded = JSON.Array([JSON.String("None")])->Polyvariants.t3_string_decode
  t->testEqual(`decode #None`, noneDecoded, Ok(#None))
})

zoraBlock("generic polyvariant with multiple type parameters", t => {
  let leftEncoded = #Left("hello")->Polyvariants.t4_string_int_encode
  t->testEqual(
    `encode #Left("hello")`,
    leftEncoded,
    JSON.Array([JSON.String("Left"), JSON.String("hello")]),
  )

  let rightEncoded = #Right(42)->Polyvariants.t4_string_int_encode
  t->testEqual(
    `encode #Right(42)`,
    rightEncoded,
    JSON.Array([JSON.String("Right"), JSON.Number(42.0)]),
  )

  let bothEncoded = #Both("hello", 42)->Polyvariants.t4_string_int_encode
  t->testEqual(
    `encode #Both("hello", 42)`,
    bothEncoded,
    JSON.Array([JSON.String("Both"), JSON.String("hello"), JSON.Number(42.0)]),
  )

  let leftDecoded =
    JSON.Array([JSON.String("Left"), JSON.String("hello")])->Polyvariants.t4_string_int_decode
  t->testEqual(`decode #Left`, leftDecoded, Ok(#Left("hello")))

  let rightDecoded =
    JSON.Array([JSON.String("Right"), JSON.Number(42.0)])->Polyvariants.t4_string_int_decode
  t->testEqual(`decode #Right`, rightDecoded, Ok(#Right(42)))

  let bothDecoded =
    JSON.Array([
      JSON.String("Both"),
      JSON.String("hello"),
      JSON.Number(42.0),
    ])->Polyvariants.t4_string_int_decode
  t->testEqual(`decode #Both`, bothDecoded, Ok(#Both("hello", 42)))
})
