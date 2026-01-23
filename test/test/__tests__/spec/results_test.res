open Zora

let testEqual = (t, name, lhs, rhs) =>
  t->test(name, async t => {
    t->equal(lhs, rhs, name)
  })

zoraBlock("result<int, string> - Ok case", t => {
  let data: result<int, string> = Ok(42)
  let json = JSON.Array([JSON.String("Ok"), JSON.Number(42.0)])

  let encoded = data->Results.intResult_encode
  t->testEqual("encode Ok result", encoded, json)

  let decoded = json->Results.intResult_decode
  t->testEqual("decode Ok result", decoded, Ok(data))
})

zoraBlock("result<int, string> - Error case", t => {
  let data: result<int, string> = Error("something went wrong")
  let json = JSON.Array([JSON.String("Error"), JSON.String("something went wrong")])

  let encoded = data->Results.intResult_encode
  t->testEqual("encode Error result", encoded, json)

  let decoded = json->Results.intResult_decode
  t->testEqual("decode Error result", decoded, Ok(data))
})

zoraBlock("result<inner, string> - Ok case with record", t => {
  let data: result<Results.inner, string> = Ok({id: 1, name: "test"})
  let json = JSON.Array([
    JSON.String("Ok"),
    JSON.Object(dict{"id": JSON.Number(1.0), "name": JSON.String("test")}),
  ])

  let encoded = data->Results.innerResult_encode
  t->testEqual("encode Ok result with record", encoded, json)

  let decoded = json->Results.innerResult_decode
  t->testEqual("decode Ok result with record", decoded, Ok(data))
})

zoraBlock("nested result<result<int, string>, string>", t => {
  let data: result<result<int, string>, string> = Ok(Ok(42))
  let json = JSON.Array([
    JSON.String("Ok"),
    JSON.Array([JSON.String("Ok"), JSON.Number(42.0)]),
  ])

  let encoded = data->Results.nestedResult_encode
  t->testEqual("encode nested Ok result", encoded, json)

  let decoded = json->Results.nestedResult_decode
  t->testEqual("decode nested Ok result", decoded, Ok(data))
})

zoraBlock("nested result - inner Error", t => {
  let data: result<result<int, string>, string> = Ok(Error("inner error"))
  let json = JSON.Array([
    JSON.String("Ok"),
    JSON.Array([JSON.String("Error"), JSON.String("inner error")]),
  ])

  let encoded = data->Results.nestedResult_encode
  t->testEqual("encode nested inner Error result", encoded, json)

  let decoded = json->Results.nestedResult_decode
  t->testEqual("decode nested inner Error result", decoded, Ok(data))
})

zoraBlock("Result.t<int, string> - Ok case", t => {
  let data: Result.t<int, string> = Ok(100)
  let json = JSON.Array([JSON.String("Ok"), JSON.Number(100.0)])

  let encoded = data->Results.resultT_encode
  t->testEqual("encode Result.t Ok", encoded, json)

  let decoded = json->Results.resultT_decode
  t->testEqual("decode Result.t Ok", decoded, Ok(data))
})

zoraBlock("Belt.Result.t<int, string> - Ok case", t => {
  let data: Belt.Result.t<int, string> = Ok(200)
  let json = JSON.Array([JSON.String("Ok"), JSON.Number(200.0)])

  let encoded = data->Results.beltResultT_encode
  t->testEqual("encode Belt.Result.t Ok", encoded, json)

  let decoded = json->Results.beltResultT_decode
  t->testEqual("decode Belt.Result.t Ok", decoded, Ok(data))
})
