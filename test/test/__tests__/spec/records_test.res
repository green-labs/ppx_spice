open Zora

let testEqual = (t, name, lhs, rhs) =>
  t->test(name, async t => {
    t->equal(lhs, rhs, name)
  })

zoraBlock("record with @spice.key", t => {
  let sample = Js.Dict.empty()
  sample->Js.Dict.set("spice-label", Js.Json.string("sample"))
  sample->Js.Dict.set("spice-value", Js.Json.number(1.0))
  let sampleJson = sample->Js.Json.object_

  let sampleRecord: Records.t = {
    label: "sample",
    value: 1,
  }

  let encoded = sampleRecord->Records.t_encode
  t->testEqual(`encode`, encoded, sampleJson)

  let decoded = sampleJson->Records.t_decode
  t->testEqual(`decode`, decoded, Ok(sampleRecord))
})

zoraBlock("record without @spice.key", t => {
  let sample = Js.Dict.empty()
  sample->Js.Dict.set("label", Js.Json.string("sample"))
  sample->Js.Dict.set("value", Js.Json.number(1.0))
  let sampleJson = sample->Js.Json.object_

  let sampleRecord: Records.t1 = {
    label: "sample",
    value: 1,
  }

  let encoded = sampleRecord->Records.t1_encode
  t->testEqual(`encode`, encoded, sampleJson)

  let decoded = sampleJson->Records.t1_decode
  t->testEqual(`decode`, decoded, Ok(sampleRecord))
})

zoraBlock("record with optional field", t => {
  let sample1 = Js.Dict.empty()
  sample1->Js.Dict.set("label", Js.Json.string("sample"))
  sample1->Js.Dict.set("value", Js.Json.number(1.0))
  let sampleJson1 = sample1->Js.Json.object_

  let sampleRecord1: Records.tOp = {
    label: Some("sample"),
    value: 1,
  }

  let encoded = sampleRecord1->Records.tOp_encode
  t->testEqual(`encode`, encoded, sampleJson1)

  let decoded = sampleJson1->Records.tOp_decode
  t->testEqual(`decode`, decoded, Ok(sampleRecord1))

  let sample2 = Js.Dict.empty()
  sample2->Js.Dict.set("label", Js.Json.string("sample"))
  let sampleJson2 = sample2->Js.Json.object_

  let sampleRecord2: Records.tOp = {
    label: Some("sample"),
  }

  let encoded = sampleRecord2->Records.tOp_encode
  t->testEqual(`encode omit optional field`, encoded, sampleJson2)

  // FIXME: https://github.com/rescript-lang/rescript-compiler/issues/6485
  // let decoded = sampleJson2->Records.tOp_decode
  // t->testEqual(`decode omit optional field`, decoded, Ok(sampleRecord2))

  let sample3 = Js.Dict.empty()
  let sampleJson3 = sample3->Js.Json.object_

  let sampleRecord3: Records.tOp = {
    label: None,
  }

  let encoded = sampleRecord3->Records.tOp_encode
  t->testEqual(`encode omit optional field with None field`, encoded, sampleJson3)

  // FIXME: https://github.com/rescript-lang/rescript-compiler/issues/6485
  // let decoded = sampleJson3->Records.tOp_decode
  // t->testEqual(`decode omit optional field with None field`, decoded, Ok(sampleRecord3))
})

zoraBlock("record with null", t => {
  let sample = Js.Dict.empty()
  sample->Js.Dict.set("n", Js.Json.null)
  sample->Js.Dict.set("n2", Js.Json.string("n2"))
  let sampleJson = sample->Js.Json.object_

  let sampleRecord: Records.t2 = {
    o: None,
    n: Js.null,
    n2: Js.Null.return("n2"),
  }

  let encoded = sampleRecord->Records.t2_encode
  t->testEqual(`encode`, encoded, sampleJson)

  let decoded = sampleJson->Records.t2_decode
  t->testEqual(`decode`, decoded, Ok(sampleRecord))
})

zoraBlock("record with spice.default", t => {
  let sample = Js.Dict.empty()
  let sampleJson = sample->Js.Json.object_

  let sample2 = Js.Dict.empty()
  sample2->Js.Dict.set("value", Js.Json.number(0.0))
  sample2->Js.Dict.set("value2", Js.Json.number(1.0))
  let sampleJson2 = sample2->Js.Json.object_

  let sampleRecord: Records.t3 = {
    value: 0,
    value2: 1,
  }

  let encoded = sampleRecord->Records.t3_encode
  t->testEqual(`encode`, encoded, sampleJson2)

  let decoded = sampleJson->Records.t3_decode
  t->testEqual(`decode`, decoded, Ok(sampleRecord))
})

zoraBlock("record with bigint", t => {
  let sample = Js.Dict.empty()
  sample->Js.Dict.set("a", Js.Json.number(0.0))
  sample->Js.Dict.set("b", Js.Json.number(1.0))
  let sampleJson = sample->Js.Json.object_

  let sampleRecord: Records.t4 = {
    a: 0n,
    b: 1n,
    c: None,
  }

  let encoded = sampleRecord->Records.t4_encode
  t->testEqual(`encode`, encoded, sampleJson)

  let decoded = sampleJson->Records.t4_decode
  t->testEqual(`decode`, decoded, Ok(sampleRecord))
})
