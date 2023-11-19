open Zora
open OptionalFieldRecords

let testEqual = (t, name, lhs, rhs) =>
  t->test(name, async t => {
    t->equal(lhs, rhs, name)
  })

zoraBlock("optional field record", t => {
  let sample = Js.Dict.empty()
  sample->Js.Dict.set("a", Js.Json.number(1.))
  sample->Js.Dict.set("b", Js.Json.number(1.))
  let sampleJson = sample->Js.Json.object_

  let sampleRecord: t0 = {
    a: 1,
    b: 1,
  }

  let encoded = sampleRecord->t0_encode
  t->testEqual("encode", encoded, sampleJson)

  let decoded = sampleJson->t0_decode
  t->testEqual("decode", decoded, Ok(sampleRecord))
})

zoraBlock("optional field record: array<int>", t => {
  let sample = Js.Dict.empty()
  sample->Js.Dict.set("a", Js.Json.number(1.))
  sample->Js.Dict.set("bs", Js.Json.array([Js.Json.number(1.)]))
  let sampleJson = sample->Js.Json.object_

  let sampleRecord: t1 = {
    a: 1,
    bs: [1],
  }
  let encoded = sampleRecord->t1_encode
  t->testEqual(`encode`, encoded, sampleJson)

  let decoded = sampleJson->t1_decode
  t->testEqual(`decode`, decoded, Ok(sampleRecord))
})

zoraBlock("optional field record: array<variant>", t => {
  let sample = Js.Dict.empty()
  sample->Js.Dict.set("a", Js.Json.number(1.))
  sample->Js.Dict.set("bs", Js.Json.array([Js.Json.string("B1")]))
  let sampleJson = sample->Js.Json.object_

  let sampleRecord: t2 = {
    a: 1,
    bs: [B1],
  }

  let encoded = sampleRecord->t2_encode
  t->testEqual(`encode`, encoded, sampleJson)

  let decoded = sampleJson->t2_decode
  t->testEqual(`decode`, decoded, Ok(sampleRecord))
})

zoraBlock("optional field record: omit array<variant>", t => {
  let sample = Js.Dict.empty()
  sample->Js.Dict.set("a", Js.Json.number(1.))

  let sampleJson = sample->Js.Json.object_
  let sampleRecord: t2 = {
    a: 1,
  }

  let encoded = sampleRecord->t2_encode
  t->testEqual(`encode`, encoded, sampleJson)

  // FIXME: https://github.com/rescript-lang/rescript-compiler/issues/6485
  // let decoded = sampleJson->t2_decode
  // t->testEqual(`decode`, decoded, Ok(sampleRecord))
})
