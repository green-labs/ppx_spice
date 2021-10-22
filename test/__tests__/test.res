open Jest
open Expect
// open Belt

describe("ppx_spice test", _ => {
  test("polymorphic variants", _ => {
    let sample1 = ["one"->Js.Json.string]->Js.Json.array->Sample1.t_decode
    expect(sample1) |> toEqual(Ok(#one))
  })

  test("variants encode", _ => {
    let sample2Encoded = Sample2.One->Sample2.t_encode
    expect(sample2Encoded) |> toEqual(["first"->Js.Json.string]->Js.Json.array)
  })
  test("variants decode", _ => {
    let sample2Decoded = ["second"->Js.Json.string]->Js.Json.array->Sample2.t_decode
    expect(sample2Decoded) |> toEqual(Ok(Sample2.Two))
  })
})
