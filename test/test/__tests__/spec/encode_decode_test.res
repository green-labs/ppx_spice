open Zora
open EncodeDecode

let testEqual = (t, name, lhs, rhs) =>
  t->test(name, async t => {
    t->equal(lhs, rhs, name)
  })

zoraBlock("encode only", t => {
  let sample = Js.Dict.empty()
  sample->Js.Dict.set("name", Js.Json.string("Alice"))
  sample->Js.Dict.set("nickname", Js.Json.string("Ecila"))
  let sampleJson = sample->Js.Json.object_

  let sampleRecord: te = {
    name: "Alice",
    nickname: "Ecila",
  }

  let encoded = sampleRecord->te_encode

  t->testEqual("encode", encoded, sampleJson)
})

zoraBlock("decode only", t => {
  let sample = Js.Dict.empty()
  sample->Js.Dict.set("name", Js.Json.string("Alice"))
  sample->Js.Dict.set("nickname", Js.Json.string("Ecila"))
  let sampleJson = sample->Js.Json.object_

  let sampleRecord: td = {
    name: "Alice",
    nickname: "Ecila",
  }

  let decoded = sampleJson->td_decode

  t->testEqual("decode", decoded, Ok(sampleRecord))
})
