open Zora

let testEqual = (t, name, lhs, rhs) =>
  t->test(name, async t => {
    t->equal(lhs, rhs, name)
  })

// Test that @spice types inside nested module structures are processed correctly
zoraBlock("nested module Inner.t encode/decode", t => {
  let sample = dict{
    "name": JSON.String("test"),
    "value": JSON.Number(42.0),
  }
  let sampleJson = sample->JSON.Object

  let sampleRecord: NestedModules.Inner.t = {
    name: "test",
    value: 42,
  }

  // This will fail to compile if Inner.t_encode is not generated
  let encoded = sampleRecord->NestedModules.Inner.t_encode
  t->testEqual(`encode`, encoded, sampleJson)

  // This will fail to compile if Inner.t_decode is not generated
  let decoded = sampleJson->NestedModules.Inner.t_decode
  t->testEqual(`decode`, decoded, Ok(sampleRecord))
})

zoraBlock("deeply nested module Outer.Nested.t encode/decode", t => {
  let sample = dict{
    "label": JSON.String("nested"),
    "count": JSON.Number(3.14),
  }
  let sampleJson = sample->JSON.Object

  let sampleRecord: NestedModules.Outer.Nested.t = {
    label: "nested",
    count: 3.14,
  }

  // This will fail to compile if Outer.Nested.t_encode is not generated
  let encoded = sampleRecord->NestedModules.Outer.Nested.t_encode
  t->testEqual(`encode`, encoded, sampleJson)

  // This will fail to compile if Outer.Nested.t_decode is not generated
  let decoded = sampleJson->NestedModules.Outer.Nested.t_decode
  t->testEqual(`decode`, decoded, Ok(sampleRecord))
})

zoraBlock("nested module Outer.wrapper with references to other nested modules", t => {
  let sample = dict{
    "inner": JSON.Object(
      dict{
        "name": JSON.String("inner-test"),
        "value": JSON.Number(100.0),
      },
    ),
    "nested": JSON.Object(
      dict{
        "label": JSON.String("nested-test"),
        "count": JSON.Number(2.71),
      },
    ),
  }
  let sampleJson = sample->JSON.Object

  let sampleRecord: NestedModules.Outer.wrapper = {
    inner: {name: "inner-test", value: 100},
    nested: {label: "nested-test", count: 2.71},
  }

  let encoded = sampleRecord->NestedModules.Outer.wrapper_encode
  t->testEqual(`encode`, encoded, sampleJson)

  let decoded = sampleJson->NestedModules.Outer.wrapper_decode
  t->testEqual(`decode`, decoded, Ok(sampleRecord))
})

zoraBlock("top-level combined type with nested module references", t => {
  let sample = dict{
    "inner": JSON.Object(
      dict{
        "name": JSON.String("combined-inner"),
        "value": JSON.Number(200.0),
      },
    ),
    "nested": JSON.Object(
      dict{
        "label": JSON.String("combined-nested"),
        "count": JSON.Number(1.41),
      },
    ),
  }
  let sampleJson = sample->JSON.Object

  let sampleRecord: NestedModules.combined = {
    inner: {name: "combined-inner", value: 200},
    nested: {label: "combined-nested", count: 1.41},
  }

  let encoded = sampleRecord->NestedModules.combined_encode
  t->testEqual(`encode`, encoded, sampleJson)

  let decoded = sampleJson->NestedModules.combined_decode
  t->testEqual(`decode`, decoded, Ok(sampleRecord))
})
