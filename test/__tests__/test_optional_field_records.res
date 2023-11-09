open Jest
open Expect
// open Belt

describe("optional field record", _ => {
  open OptionalFieldRecords

  let sample = Js.Dict.empty()
  sample->Js.Dict.set("a", Js.Json.number(1.))
  sample->Js.Dict.set("b", Js.Json.number(1.))
  let sampleJson = sample->Js.Json.object_

  let sampleRecord: t0 = {
    a: 1,
    b: 1,
  }

  test(`encode`, _ => {
    let encoded = sampleRecord->t0_encode
    expect(encoded)->toEqual(sampleJson)
  })

  test(`decode`, _ => {
    let decoded = sampleJson->t0_decode
    expect(decoded)->toEqual(Ok(sampleRecord))
  })
})

describe("optional field record: array<int>", _ => {
  open OptionalFieldRecords

  let sample = Js.Dict.empty()
  sample->Js.Dict.set("a", Js.Json.number(1.))
  sample->Js.Dict.set("bs", Js.Json.array([Js.Json.number(1.)]))
  let sampleJson = sample->Js.Json.object_

  let sampleRecord: t1 = {
    a: 1,
    bs: [1],
  }

  test(`encode`, _ => {
    let encoded = sampleRecord->t1_encode
    expect(encoded)->toEqual(sampleJson)
  })

  test(`decode`, _ => {
    let decoded = sampleJson->t1_decode
    expect(decoded)->toEqual(Ok(sampleRecord))
  })
})

describe("optional field record: array<variant>", _ => {
  open OptionalFieldRecords

  let sample = Js.Dict.empty()
  sample->Js.Dict.set("a", Js.Json.number(1.))
  sample->Js.Dict.set("bs", Js.Json.array([Js.Json.string("B1")]))
  let sampleJson = sample->Js.Json.object_

  let sampleRecord: t2 = {
    a: 1,
    bs: [B1],
  }

  test(`encode`, _ => {
    let encoded = sampleRecord->t2_encode
    expect(encoded)->toEqual(sampleJson)
  })

  test(`decode`, _ => {
    let decoded = sampleJson->t2_decode
    expect(decoded)->toEqual(Ok(sampleRecord))
  })
})

describe("optional field record: omit array<variant>", _ => {
  open OptionalFieldRecords

  let sample = Js.Dict.empty()
  sample->Js.Dict.set("a", Js.Json.number(1.))

  let sampleJson = sample->Js.Json.object_

  let sampleRecord: t2 = {
    a: 1,
  }

  test(`encode`, _ => {
    let encoded = sampleRecord->t2_encode
    expect(encoded)->toEqual(sampleJson)
  })

  test(`decode`, _ => {
    let decoded = sampleJson->t2_decode
    expect(decoded)->toEqual(Ok(sampleRecord))
  })
})
