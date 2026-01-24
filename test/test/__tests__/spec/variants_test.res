open Zora

let testEqual = (t, name, lhs, rhs) =>
  t->test(name, async t => {
    t->equal(lhs, rhs, name)
  })

zoraBlock("variants with @spice.as", t => {
  let variantEncoded = Variants.One->Variants.t_encode
  t->testEqual(`encode 하나`, variantEncoded, JSON.Number(1.))

  let variantEncoded = Variants.Two->Variants.t_encode
  t->testEqual(`encode 둘`, variantEncoded, JSON.String(`둘`))

  let variantDecoded = JSON.Number(1.)->Variants.t_decode
  t->testEqual(`decode 하나`, variantDecoded, Ok(Variants.One))

  let variantDecoded = JSON.String(`둘`)->Variants.t_decode
  t->testEqual(`decode 둘`, variantDecoded, Ok(Variants.Two))
})

zoraBlock(`variants without @spice.as`, t => {
  let variantEncoded = Variants.One1->Variants.t1_encode
  t->testEqual(`encode One1`, variantEncoded, JSON.Array([JSON.String(`One1`)]))

  let variantEncoded = Variants.Two1->Variants.t1_encode
  t->testEqual(`encode Two1`, variantEncoded, JSON.Array([JSON.String(`Two1`)]))

  let variantDecoded = JSON.Array([JSON.String(`One1`)])->Variants.t1_decode
  t->testEqual(`decode ["One1"]`, variantDecoded, Ok(Variants.One1))

  let variantDecoded = JSON.Array([JSON.String(`Two1`)])->Variants.t1_decode
  t->testEqual(`decode ["Two1"]`, variantDecoded, Ok(Variants.Two1))
})

zoraBlock("unboxed variants with @spice.as", t => {
  let variantEncoded = Variants.One2(0)->Variants.t2_encode
  t->testEqual(`encode 하나`, variantEncoded, JSON.Number(0.0))

  let variantDecoded = JSON.Number(0.0)->Variants.t2_decode
  t->testEqual(`decode 하나`, variantDecoded, Ok(Variants.One2(0)))
})

zoraBlock(`unboxed variants without @spice.as`, t => {
  let variantEncoded = Variants.One3(0)->Variants.t3_encode
  t->testEqual(`encode One3(0)`, variantEncoded, JSON.Number(0.0))

  let variantDecoded = JSON.Number(0.0)->Variants.t3_decode
  t->testEqual(`decode 0`, variantDecoded, Ok(Variants.One3(0)))
})

zoraBlock("variants with @spice.as number", t => {
  let variantEncoded = Variants.One->Variants.t4_encode
  t->testEqual(`encode 1.0`, variantEncoded, JSON.Number(1.0))

  let variantEncoded = Variants.Two->Variants.t4_encode
  t->testEqual(`encode 2.0`, variantEncoded, JSON.Number(2.0))

  let variantDecoded = JSON.Number(1.0)->Variants.t4_decode
  t->testEqual(`decode 1.0`, variantDecoded, Ok(Variants.One))

  let variantDecoded = JSON.Number(2.0)->Variants.t4_decode
  t->testEqual(`decode 2.0`, variantDecoded, Ok(Variants.Two))
})

zoraBlock("variant error path includes correct index", t => {
  // Variant with args: ["WithArgs", int, string]
  // Index 0 is the constructor name, index 1 is the int, index 2 is the string
  // When index 1 fails, path should be "[1]" not "[0]"
  let invalidJson = JSON.Array([
    JSON.String("WithArgs"),
    JSON.String("not an int"), // should be int at index 1
    JSON.String("valid string"),
  ])

  let decoded = invalidJson->Variants.withArgs_decode
  t->test("error path shows [1] for first argument", async t => {
    switch decoded {
    | Error({path}) => t->equal(path, "[1]", "path should be [1]")
    | Ok(_) => t->fail("expected decode to fail")
    }
  })
})

zoraBlock("generic variant with single type parameter", t => {
  let someEncoded = Variants.Some("hello")->Variants.t5_string_encode
  t->testEqual(
    `encode Some("hello")`,
    someEncoded,
    JSON.Array([JSON.String("Some"), JSON.String("hello")]),
  )

  let noneEncoded = Variants.None->Variants.t5_string_encode
  t->testEqual(`encode None`, noneEncoded, JSON.Array([JSON.String("None")]))

  let someDecoded =
    JSON.Array([JSON.String("Some"), JSON.String("hello")])->Variants.t5_string_decode
  t->testEqual(`decode Some`, someDecoded, Ok(Variants.Some("hello")))

  let noneDecoded = JSON.Array([JSON.String("None")])->Variants.t5_string_decode
  t->testEqual(`decode None`, noneDecoded, Ok(Variants.None))
})

zoraBlock("generic variant with multiple type parameters", t => {
  let leftEncoded = Variants.Left("hello")->Variants.t6_string_int_encode
  t->testEqual(
    `encode Left("hello")`,
    leftEncoded,
    JSON.Array([JSON.String("Left"), JSON.String("hello")]),
  )

  let rightEncoded = Variants.Right(42)->Variants.t6_string_int_encode
  t->testEqual(
    `encode Right(42)`,
    rightEncoded,
    JSON.Array([JSON.String("Right"), JSON.Number(42.0)]),
  )

  let bothEncoded = Variants.Both("hello", 42)->Variants.t6_string_int_encode
  t->testEqual(
    `encode Both("hello", 42)`,
    bothEncoded,
    JSON.Array([JSON.String("Both"), JSON.String("hello"), JSON.Number(42.0)]),
  )

  let leftDecoded =
    JSON.Array([JSON.String("Left"), JSON.String("hello")])->Variants.t6_string_int_decode
  t->testEqual(`decode Left`, leftDecoded, Ok(Variants.Left("hello")))

  let rightDecoded =
    JSON.Array([JSON.String("Right"), JSON.Number(42.0)])->Variants.t6_string_int_decode
  t->testEqual(`decode Right`, rightDecoded, Ok(Variants.Right(42)))

  let bothDecoded =
    JSON.Array([
      JSON.String("Both"),
      JSON.String("hello"),
      JSON.Number(42.0),
    ])->Variants.t6_string_int_decode
  t->testEqual(`decode Both`, bothDecoded, Ok(Variants.Both("hello", 42)))
})
