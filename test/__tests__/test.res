open Jest
open Expect
// open Belt

describe("ppx_spice test", _ => {
  test("polymorphic variant encode", _ => {
    let sample1Encoded = #one->Sample1.t_encode
    expect(sample1Encoded) |> toEqual(`하나`)
  })
  test("polymorphic variant encode", _ => {
    let sample1Encoded = #two->Sample1.t_encode
    expect(sample1Encoded) |> toEqual(`둘`)
  })
  test("polymorphic variant decode", _ => {
    let sample1Decoded = `하나`->Sample1.t_decode
    expect(sample1Decoded) |> toEqual(Ok(#one))
  })
  test("polymorphic variant decode", _ => {
    let sample1Decoded = `둘`->Sample1.t_decode
    expect(sample1Decoded) |> toEqual(Ok(#two))
  })

  test("variant encode", _ => {
    let sample2Encoded = Sample2.One->Sample2.t_encode
    expect(sample2Encoded) |> toEqual(`하나`)
  })
  test("variant encode", _ => {
    let sample2Encoded = Sample2.Two->Sample2.t_encode
    expect(sample2Encoded) |> toEqual(`둘`)
  })
  test("variant decode", _ => {
    let sample2Decoded = `하나`->Sample2.t_decode
    expect(sample2Decoded) |> toEqual(Ok(Sample2.One))
  })
  test("variant decode", _ => {
    let sample2Decoded = `둘`->Sample2.t_decode
    expect(sample2Decoded) |> toEqual(Ok(Sample2.Two))
  })
})
