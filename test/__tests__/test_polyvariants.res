open Jest
open Expect
// open Belt

describe("polymorphic variants with attribute", _ => {
  test("encode 하나", _ => {
    let polyvariantEncoded = #one->Polyvariants.t_encode
    expect(polyvariantEncoded)->toEqual(Js.Json.string(`하나`))
  })
  test("encode 둘", _ => {
    let polyvariantEncoded = #two->Polyvariants.t_encode
    expect(polyvariantEncoded)->toEqual(Js.Json.string(`둘`))
  })
  test("decode 하나", _ => {
    let polyvariantDecoded = Js.Json.string(`하나`)->Polyvariants.t_decode
    expect(polyvariantDecoded)->toEqual(Ok(#one))
  })
  test("decode 둘", _ => {
    let polyvariantDecoded = Js.Json.string(`둘`)->Polyvariants.t_decode
    expect(polyvariantDecoded)->toEqual(Ok(#two))
  })
})

describe("polymorphic variants", _ => {
  test(`encode #one`, _ => {
    let polyvariantEncoded = #one->Polyvariants.t1_encode
    expect(polyvariantEncoded)->toEqual(Js.Json.array([Js.Json.string(`one`)]))
  })
  test(`encode #two`, _ => {
    let polyvariantEncoded = #two->Polyvariants.t1_encode
    expect(polyvariantEncoded)->toEqual(Js.Json.array([Js.Json.string(`two`)]))
  })
  test(`decode one`, _ => {
    let polyvariantDecoded = Js.Json.array([Js.Json.string(`one`)])->Polyvariants.t1_decode
    expect(polyvariantDecoded)->toEqual(Ok(#one))
  })
  test(`decode two`, _ => {
    let polyvariantDecoded = Js.Json.array([Js.Json.string(`two`)])->Polyvariants.t1_decode
    expect(polyvariantDecoded)->toEqual(Ok(#two))
  })
})
