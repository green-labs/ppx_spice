open Jest
open Expect
// open Belt

describe("ppx_spice test", _ => {
  test("polymorphic variant encode", _ => {
    let sample1Encoded = #one->Sample1.t_encode
    expect(sample1Encoded) |> toEqual("first"->Js.Json.string)
  })
  test("polymorphic variant decode", _ => {
    let sample1Decoded = "second"->Js.Json.string->Sample1.t_decode
    expect(sample1Decoded) |> toEqual(Ok(#two))
  })

  test("variant encode", _ => {
    let sample2Encoded = Sample2.One->Sample2.t_encode
    expect(sample2Encoded) |> toEqual("first"->Js.Json.string)
  })
  test("variant decode", _ => {
    let sample2Decoded = "second"->Js.Json.string->Sample2.t_decode
    expect(sample2Decoded) |> toEqual(Ok(Sample2.Two))
  })
})
