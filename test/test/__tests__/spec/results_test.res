open Zora

let testEqual = (t, name, lhs, rhs) =>
  t->test(name, async t => {
    t->equal(lhs, rhs, name)
  })

zoraBlock("result<int, string> - Ok case", t => {
  let sample = JSON.Array([JSON.String("Ok"), JSON.Number(42.0)])

  let sampleRecord: Results.simpleResult = Ok(42)

  let encoded = sampleRecord->Results.simpleResult_encode
  t->testEqual(`encode Ok`, encoded, sample)

  let decoded = sample->Results.simpleResult_decode
  t->testEqual(`decode Ok`, decoded, Ok(sampleRecord))
})

zoraBlock("result<int, string> - Error case", t => {
  let sample = JSON.Array([JSON.String("Error"), JSON.String("something went wrong")])

  let sampleRecord: Results.simpleResult = Error("something went wrong")

  let encoded = sampleRecord->Results.simpleResult_encode
  t->testEqual(`encode Error`, encoded, sample)

  let decoded = sample->Results.simpleResult_decode
  t->testEqual(`decode Error`, decoded, Ok(sampleRecord))
})

zoraBlock("result<inner, string> - Ok with record", t => {
  let sample = JSON.Array([
    JSON.String("Ok"),
    JSON.Object(
      dict{
        "name": JSON.String("test"),
        "value": JSON.Number(123.0),
      }
    ),
  ])

  let sampleRecord: Results.recordResult = Ok({
    name: "test",
    value: 123,
  })

  let encoded = sampleRecord->Results.recordResult_encode
  t->testEqual(`encode Ok with record`, encoded, sample)

  let decoded = sample->Results.recordResult_decode
  t->testEqual(`decode Ok with record`, decoded, Ok(sampleRecord))
})

zoraBlock("result<inner, string> - Error case", t => {
  let sample = JSON.Array([JSON.String("Error"), JSON.String("validation failed")])

  let sampleRecord: Results.recordResult = Error("validation failed")

  let encoded = sampleRecord->Results.recordResult_encode
  t->testEqual(`encode Error`, encoded, sample)

  let decoded = sample->Results.recordResult_decode
  t->testEqual(`decode Error`, decoded, Ok(sampleRecord))
})

zoraBlock("result<result<int, string>, string> - nested Ok", t => {
  let sample = JSON.Array([
    JSON.String("Ok"),
    JSON.Array([JSON.String("Ok"), JSON.Number(99.0)]),
  ])

  let sampleRecord: Results.nestedResult = Ok(Ok(99))

  let encoded = sampleRecord->Results.nestedResult_encode
  t->testEqual(`encode nested Ok(Ok(...))`, encoded, sample)

  let decoded = sample->Results.nestedResult_decode
  t->testEqual(`decode nested Ok(Ok(...))`, decoded, Ok(sampleRecord))
})

zoraBlock("result<result<int, string>, string> - nested Error", t => {
  let sample = JSON.Array([
    JSON.String("Ok"),
    JSON.Array([JSON.String("Error"), JSON.String("inner error")]),
  ])

  let sampleRecord: Results.nestedResult = Ok(Error("inner error"))

  let encoded = sampleRecord->Results.nestedResult_encode
  t->testEqual(`encode nested Ok(Error(...))`, encoded, sample)

  let decoded = sample->Results.nestedResult_decode
  t->testEqual(`decode nested Ok(Error(...))`, decoded, Ok(sampleRecord))
})

zoraBlock("result<result<int, string>, string> - outer Error", t => {
  let sample = JSON.Array([JSON.String("Error"), JSON.String("outer error")])

  let sampleRecord: Results.nestedResult = Error("outer error")

  let encoded = sampleRecord->Results.nestedResult_encode
  t->testEqual(`encode outer Error`, encoded, sample)

  let decoded = sample->Results.nestedResult_decode
  t->testEqual(`decode outer Error`, decoded, Ok(sampleRecord))
})
