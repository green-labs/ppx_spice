open Zora

let testEqual = (t, name, lhs, rhs) =>
  t->test(name, async t => {
    t->equal(lhs, rhs, name)
  })

zoraBlock("array of strings", t => {
  let data = ["first", "second", "third", "fourth", "fifth"]
  let json = JSON.Array([
    JSON.String("first"),
    JSON.String("second"),
    JSON.String("third"),
    JSON.String("fourth"),
    JSON.String("fifth"),
  ])

  let encoded = data->Arrays.stringArray_encode
  t->testEqual("encode string array", encoded, json)

  let decoded = json->Arrays.stringArray_decode
  t->testEqual("decode string array preserves order", decoded, Ok(data))
})

zoraBlock("array of ints", t => {
  let data = [1, 2, 3, 4, 5]
  let json = JSON.Array([
    JSON.Number(1.0),
    JSON.Number(2.0),
    JSON.Number(3.0),
    JSON.Number(4.0),
    JSON.Number(5.0),
  ])

  let encoded = data->Arrays.intArray_encode
  t->testEqual("encode int array", encoded, json)

  let decoded = json->Arrays.intArray_decode
  t->testEqual("decode int array preserves order", decoded, Ok(data))
})

zoraBlock("array of floats", t => {
  let data = [1.1, 2.2, 3.3, 4.4, 5.5]
  let json = JSON.Array([
    JSON.Number(1.1),
    JSON.Number(2.2),
    JSON.Number(3.3),
    JSON.Number(4.4),
    JSON.Number(5.5),
  ])

  let encoded = data->Arrays.floatArray_encode
  t->testEqual("encode float array", encoded, json)

  let decoded = json->Arrays.floatArray_decode
  t->testEqual("decode float array preserves order", decoded, Ok(data))
})

zoraBlock("array of bools", t => {
  let data = [true, false, true, false, true]
  let json = JSON.Array([
    JSON.Boolean(true),
    JSON.Boolean(false),
    JSON.Boolean(true),
    JSON.Boolean(false),
    JSON.Boolean(true),
  ])

  let encoded = data->Arrays.boolArray_encode
  t->testEqual("encode bool array", encoded, json)

  let decoded = json->Arrays.boolArray_decode
  t->testEqual("decode bool array preserves order", decoded, Ok(data))
})

zoraBlock("array of records", t => {
  let data: array<Arrays.recordItem> = [
    {id: 1, name: "first"},
    {id: 2, name: "second"},
    {id: 3, name: "third"},
  ]
  let json = JSON.Array([
    JSON.Object(dict{"id": JSON.Number(1.0), "name": JSON.String("first")}),
    JSON.Object(dict{"id": JSON.Number(2.0), "name": JSON.String("second")}),
    JSON.Object(dict{"id": JSON.Number(3.0), "name": JSON.String("third")}),
  ])

  let encoded = data->Arrays.recordArray_encode
  t->testEqual("encode record array", encoded, json)

  let decoded = json->Arrays.recordArray_decode
  t->testEqual("decode record array preserves order", decoded, Ok(data))
})

zoraBlock("nested arrays", t => {
  let data = [[1, 2], [3, 4], [5, 6]]
  let json = JSON.Array([
    JSON.Array([JSON.Number(1.0), JSON.Number(2.0)]),
    JSON.Array([JSON.Number(3.0), JSON.Number(4.0)]),
    JSON.Array([JSON.Number(5.0), JSON.Number(6.0)]),
  ])

  let encoded = data->Arrays.nestedArray_encode
  t->testEqual("encode nested array", encoded, json)

  let decoded = json->Arrays.nestedArray_decode
  t->testEqual("decode nested array preserves order", decoded, Ok(data))
})

zoraBlock("empty array", t => {
  let data: array<string> = []
  let json = JSON.Array([])

  let encoded = data->Arrays.stringArray_encode
  t->testEqual("encode empty array", encoded, json)

  let decoded = json->Arrays.stringArray_decode
  t->testEqual("decode empty array", decoded, Ok(data))
})

zoraBlock("single element array", t => {
  let data = ["only"]
  let json = JSON.Array([JSON.String("only")])

  let encoded = data->Arrays.stringArray_encode
  t->testEqual("encode single element array", encoded, json)

  let decoded = json->Arrays.stringArray_decode
  t->testEqual("decode single element array", decoded, Ok(data))
})
