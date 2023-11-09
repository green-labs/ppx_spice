open Jest
open Expect
// open Belt

describe("encode only", _ => {
  open EncodeDecode

  let sample = Js.Dict.empty()
  sample->Js.Dict.set("name", Js.Json.string("Alice"))
  sample->Js.Dict.set("nickname", Js.Json.string("Ecila"))
  let sampleJson = sample->Js.Json.object_

  let sampleRecord: te = {
    name: "Alice",
    nickname: "Ecila",
  }

  test(`encode`, _ => {
    let encoded = sampleRecord->te_encode
    expect(encoded)->toEqual(sampleJson)
  })
})

describe("decode only", _ => {
  open EncodeDecode

  let sample = Js.Dict.empty()
  sample->Js.Dict.set("name", Js.Json.string("Alice"))
  sample->Js.Dict.set("nickname", Js.Json.string("Ecila"))
  let sampleJson = sample->Js.Json.object_

  let sampleRecord: td = {
    name: "Alice",
    nickname: "Ecila",
  }

  test(`decode`, _ => {
    let decoded = sampleJson->td_decode
    expect(decoded)->toEqual(Ok(sampleRecord))
  })
})
