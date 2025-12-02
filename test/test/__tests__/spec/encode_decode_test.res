open Zora
open EncodeDecode

let testEqual = (t, name, lhs, rhs) =>
  t->test(name, async t => {
    t->equal(lhs, rhs, name)
  })

zoraBlock("encode only", t => {
  let sample = dict{
    "name": JSON.String("Alice"),
    "nickname": JSON.String("Ecila"),
  }
  let sampleJson = sample->JSON.Object

  let sampleRecord: te = {
    name: "Alice",
    nickname: "Ecila",
  }

  let encoded = sampleRecord->te_encode

  t->testEqual("encode", encoded, sampleJson)
})

zoraBlock("decode only", t => {
  let sample = dict{
    "name": JSON.String("Alice"),
    "nickname": JSON.String("Ecila"),
  }
  let sampleJson = sample->JSON.Object

  let sampleRecord: td = {
    name: "Alice",
    nickname: "Ecila",
  }

  let decoded = sampleJson->td_decode

  t->testEqual("decode", decoded, Ok(sampleRecord))
})
