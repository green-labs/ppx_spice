open Jest
open Expect
// open Belt

describe("variants with @spice.as", _ => {
  test(`encode 하나`, _ => {
    let variantEncoded = Variants.One->Variants.t_encode
    expect(variantEncoded) |> toEqual(Js.Json.string(`하나`))
  })
  test(`encode 둘`, _ => {
    let variantEncoded = Variants.Two->Variants.t_encode
    expect(variantEncoded) |> toEqual(Js.Json.string(`둘`))
  })
  test(`decode 하나`, _ => {
    let variantDecoded = Js.Json.string(`하나`)->Variants.t_decode
    expect(variantDecoded) |> toEqual(Ok(Variants.One))
  })
  test(`decode 둘`, _ => {
    let variantDecoded = Js.Json.string(`둘`)->Variants.t_decode
    expect(variantDecoded) |> toEqual(Ok(Variants.Two))
  })
})

describe(`variants without @spice.as`, _ => {
  test(`encode One1`, _ => {
    let variantEncoded = Variants.One1->Variants.t1_encode
    expect(variantEncoded) |> toEqual(Js.Json.array([Js.Json.string(`One1`)]))
  })
  test(`encode Two1`, _ => {
    let variantEncoded = Variants.Two1->Variants.t1_encode
    expect(variantEncoded) |> toEqual(Js.Json.array([Js.Json.string(`Two1`)]))
  })
  test(`decode ["One1"]`, _ => {
    let variantDecoded = Js.Json.array([Js.Json.string(`One1`)])->Variants.t1_decode
    expect(variantDecoded) |> toEqual(Ok(Variants.One1))
  })
  test(`decode ["Two1"]`, _ => {
    let variantDecoded = Js.Json.array([Js.Json.string(`Two1`)])->Variants.t1_decode
    expect(variantDecoded) |> toEqual(Ok(Variants.Two1))
  })
})

describe("unboxed variants with @spice.as", _ => {
  test(`encode 하나`, _ => {
    let variantEncoded = Variants.One2(0)->Variants.t2_encode
    expect(variantEncoded) |> toEqual(Js.Json.number(0.0))
  })
  test(`decode 하나`, _ => {
    let variantDecoded = Js.Json.number(0.0)->Variants.t2_decode
    expect(variantDecoded) |> toEqual(Ok(Variants.One2(0)))
  })
})

describe(`unboxed variants without @spice.as`, _ => {
  test(`encode One3(0)`, _ => {
    let variantEncoded = Variants.One3(0)->Variants.t3_encode
    expect(variantEncoded) |> toEqual(Js.Json.number(0.0))
  })
  test(`decode 0`, _ => {
    let variantDecoded = Js.Json.number(0.0)->Variants.t3_decode
    expect(variantDecoded) |> toEqual(Ok(Variants.One3(0)))
  })
})
