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
