open Zora

let testEqual = (t, name, lhs, rhs) =>
  t->test(name, async t => {
    t->equal(lhs, rhs, name)
  })

zoraBlock("dict<string>", t => {
  let sample = dict{
    "key1": JSON.String("value1"),
    "key2": JSON.String("value2"),
    "key3": JSON.String("value3"),
  }
  let sampleJson = sample->JSON.Object

  let sampleRecord: Dicts.stringDict = dict{
    "key1": "value1",
    "key2": "value2",
    "key3": "value3",
  }

  let encoded = sampleRecord->Dicts.stringDict_encode
  t->testEqual(`encode`, encoded, sampleJson)

  let decoded = sampleJson->Dicts.stringDict_decode
  t->testEqual(`decode`, decoded, Ok(sampleRecord))
})

zoraBlock("dict<string> empty", t => {
  let sample = dict{}
  let sampleJson = sample->JSON.Object

  let sampleRecord: Dicts.stringDict = dict{}

  let encoded = sampleRecord->Dicts.stringDict_encode
  t->testEqual(`encode empty dict`, encoded, sampleJson)

  let decoded = sampleJson->Dicts.stringDict_decode
  t->testEqual(`decode empty dict`, decoded, Ok(sampleRecord))
})

zoraBlock("dict<int>", t => {
  let sample = dict{
    "a": JSON.Number(1.0),
    "b": JSON.Number(2.0),
    "c": JSON.Number(42.0),
  }
  let sampleJson = sample->JSON.Object

  let sampleRecord: Dicts.intDict = dict{
    "a": 1,
    "b": 2,
    "c": 42,
  }

  let encoded = sampleRecord->Dicts.intDict_encode
  t->testEqual(`encode`, encoded, sampleJson)

  let decoded = sampleJson->Dicts.intDict_decode
  t->testEqual(`decode`, decoded, Ok(sampleRecord))
})

zoraBlock("dict<inner>", t => {
  let sample = dict{
    "item1": JSON.Object(
      dict{
        "name": JSON.String("first"),
        "value": JSON.Number(10.0),
      }
    ),
    "item2": JSON.Object(
      dict{
        "name": JSON.String("second"),
        "value": JSON.Number(20.0),
      }
    ),
  }
  let sampleJson = sample->JSON.Object

  let sampleRecord: Dicts.recordDict = dict{
    "item1": ({
      name: "first",
      value: 10,
    }: Dicts.inner),
    "item2": ({
      name: "second",
      value: 20,
    }: Dicts.inner),
  }

  let encoded = sampleRecord->Dicts.recordDict_encode
  t->testEqual(`encode`, encoded, sampleJson)

  let decoded = sampleJson->Dicts.recordDict_decode
  t->testEqual(`decode`, decoded, Ok(sampleRecord))
})
