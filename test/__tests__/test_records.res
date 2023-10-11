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
    expect(encoded)->toEqual(sampleJson)
  })

  test(`decode`, _ => {
    let decoded = sampleJson->Records.t_decode
    expect(decoded)->toEqual(Belt.Result.Ok(sampleRecord))
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
    expect(encoded)->toEqual(sampleJson)
  })

  test(`decode`, _ => {
    let decoded = sampleJson->Records.t1_decode
    expect(decoded)->toEqual(Belt.Result.Ok(sampleRecord))
  })
})

describe("record with optional field", _ => {
  open Records

  let sample1 = Js.Dict.empty()
  sample1->Js.Dict.set("label", Js.Json.string("sample"))
  sample1->Js.Dict.set("value", Js.Json.number(1.0))
  let sampleJson1 = sample1->Js.Json.object_

  let sampleRecord1: tOp = {
    label: Some("sample"),
    value: 1,
  }

  test(`encode`, _ => {
    let encoded = sampleRecord1->tOp_encode
    expect(encoded)->toEqual(sampleJson1)
  })

  test(`decode`, _ => {
    let decoded = sampleJson1->Records.tOp_decode
    expect(decoded)->toEqual(Belt.Result.Ok(sampleRecord1))
  })

  let sample2 = Js.Dict.empty()
  sample2->Js.Dict.set("label", Js.Json.string("sample"))
  let sampleJson2 = sample2->Js.Json.object_

  let sampleRecord2: tOp = {
    label: Some("sample"),
  }

  test(`encode omit optional field`, _ => {
    let encoded = sampleRecord2->tOp_encode
    expect(encoded)->toEqual(sampleJson2)
  })

  test(`decode omit optional field`, _ => {
    let decoded = sampleJson2->Records.tOp_decode
    expect(decoded)->toEqual(Belt.Result.Ok(sampleRecord2))
  })

  let sample3 = Js.Dict.empty()
  sample3->Js.Dict.set("label", Js.Json.null)
  let sampleJson3 = sample3->Js.Json.object_

  let sampleRecord3: tOp = {
    label: None,
  }

  test(`encode omit optional field with None field`, _ => {
    let encoded = sampleRecord3->tOp_encode
    expect(encoded)->toEqual(sampleJson3)
  })

  test(`decode omit optional field with None field`, _ => {
    let decoded = sampleJson3->Records.tOp_decode
    expect(decoded)->toEqual(Belt.Result.Ok(sampleRecord3))
  })
})
