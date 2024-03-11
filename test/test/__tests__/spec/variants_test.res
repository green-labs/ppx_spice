open Zora

let testEqual = (t, name, lhs, rhs) =>
  t->test(name, async t => {
    t->equal(lhs, rhs, name)
  })

zoraBlock("variants with @spice.as", t => {
  let variantEncoded = Variants.One->Variants.t_encode
  t->testEqual(`encode 하나`, variantEncoded, Js.Json.number(1.))

  let variantEncoded = Variants.Two->Variants.t_encode
  t->testEqual(`encode 둘`, variantEncoded, Js.Json.string(`둘`))

  let variantDecoded = Js.Json.number(1.)->Variants.t_decode
  t->testEqual(`decode 하나`, variantDecoded, Ok(Variants.One))

  let variantDecoded = Js.Json.string(`둘`)->Variants.t_decode
  t->testEqual(`decode 둘`, variantDecoded, Ok(Variants.Two))
})

zoraBlock(`variants without @spice.as`, t => {
  let variantEncoded = Variants.One1->Variants.t1_encode
  t->testEqual(`encode One1`, variantEncoded, Js.Json.array([Js.Json.string(`One1`)]))

  let variantEncoded = Variants.Two1->Variants.t1_encode
  t->testEqual(`encode Two1`, variantEncoded, Js.Json.array([Js.Json.string(`Two1`)]))

  let variantDecoded = Js.Json.array([Js.Json.string(`One1`)])->Variants.t1_decode
  t->testEqual(`decode ["One1"]`, variantDecoded, Ok(Variants.One1))

  let variantDecoded = Js.Json.array([Js.Json.string(`Two1`)])->Variants.t1_decode
  t->testEqual(`decode ["Two1"]`, variantDecoded, Ok(Variants.Two1))
})

zoraBlock("unboxed variants with @spice.as", t => {
  let variantEncoded = Variants.One2(0)->Variants.t2_encode
  t->testEqual(`encode 하나`, variantEncoded, Js.Json.number(0.0))

  let variantDecoded = Js.Json.number(0.0)->Variants.t2_decode
  t->testEqual(`decode 하나`, variantDecoded, Ok(Variants.One2(0)))
})

zoraBlock(`unboxed variants without @spice.as`, t => {
  let variantEncoded = Variants.One3(0)->Variants.t3_encode
  t->testEqual(`encode One3(0)`, variantEncoded, Js.Json.number(0.0))

  let variantDecoded = Js.Json.number(0.0)->Variants.t3_decode
  t->testEqual(`decode 0`, variantDecoded, Ok(Variants.One3(0)))
})

zoraBlock("variants with @spice.as number", t => {
  let variantEncoded = Variants.One->Variants.t4_encode
  t->testEqual(`encode 1.0`, variantEncoded, Js.Json.number(1.0))

  let variantEncoded = Variants.Two->Variants.t4_encode
  t->testEqual(`encode 2.0`, variantEncoded, Js.Json.number(2.0))

  let variantDecoded = Js.Json.number(1.0)->Variants.t4_decode
  t->testEqual(`decode 1.0`, variantDecoded, Ok(Variants.One))

  let variantDecoded = Js.Json.number(2.0)->Variants.t4_decode
  t->testEqual(`decode 2.0`, variantDecoded, Ok(Variants.Two))
})
