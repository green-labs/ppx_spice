open Jest
open Expect
// open Belt

describe("record with @spice.key", _ => {
  open Records

  let sample = Js.Dict.empty()
  sample->Js.Dict.set("spice-label", Js.Json.string("sample"))
  sample->Js.Dict.set("spice-value", Js.Json.number(1.0))
  let sampleJson = sample->Js.Json.object_

  let sampleRecord: t = {
    label: "sample",
    value: 1,
  }

  test(`encode`, _ => {
    let encoded = sampleRecord->Records.t_encode
    expect(encoded) |> toEqual(sampleJson)
  })

  test(`decode`, _ => {
    let decoded = sampleJson->Records.t_decode
    expect(decoded) |> toEqual(Belt.Result.Ok(sampleRecord))
  })
})

describe("record without @spice.key", _ => {
  open Records

  let sample = Js.Dict.empty()
  sample->Js.Dict.set("label", Js.Json.string("sample"))
  sample->Js.Dict.set("value", Js.Json.number(1.0))
  let sampleJson = sample->Js.Json.object_

  let sampleRecord: t1 = {
    label: "sample",
    value: 1,
  }

  test(`encode`, _ => {
    let encoded = sampleRecord->t1_encode
    expect(encoded) |> toEqual(sampleJson)
  })

  test(`decode`, _ => {
    let decoded = sampleJson->Records.t1_decode
    expect(decoded) |> toEqual(Belt.Result.Ok(sampleRecord))
  })
})
