open Zora

let testEqual = (t, name, lhs, rhs) =>
  t->test(name, async t => {
    t->equal(lhs, rhs, name)
  })

let deepEqualWithBigInt = %raw(`(a, b) => {
  if (typeof a === 'bigint' && typeof b === 'bigint') {
    return a === b;
  }
  if (typeof a !== typeof b) return false;
  if (typeof a === 'object' && a !== null && b !== null) {
    const keysA = Object.keys(a);
    const keysB = Object.keys(b);
    if (keysA.length !== keysB.length) return false;
    return keysA.every(key => deepEqualWithBigInt(a[key], b[key]));
  }
  return a === b;
}`)

zoraBlock("record with @spice.key", t => {
  let sample = dict{
    "spice-label": JSON.String("sample"),
    "spice-value": JSON.Number(1.0),
  }
  let sampleJson = sample->JSON.Object

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
  let sample = dict{
    "label": JSON.String("sample"),
    "value": JSON.Number(1.0),
  }
  let sampleJson = sample->JSON.Object

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
  let sample1 = dict{
    "label": JSON.String("sample"),
    "value": JSON.Number(1.0),
  }
  let sampleJson1 = sample1->JSON.Object

  let sampleRecord1: Records.tOp = {
    label: Some("sample"),
    value: 1,
  }

  let encoded = sampleRecord1->Records.tOp_encode
  t->testEqual(`encode`, encoded, sampleJson1)

  let decoded = sampleJson1->Records.tOp_decode
  t->testEqual(`decode`, decoded, Ok(sampleRecord1))

  let sample2 = dict{
    "label": JSON.String("sample"),
  }
  let sampleJson2 = sample2->JSON.Object

  let sampleRecord2: Records.tOp = {
    label: Some("sample"),
  }

  let encoded = sampleRecord2->Records.tOp_encode
  t->testEqual(`encode omit optional field`, encoded, sampleJson2)

  // FIXME: https://github.com/rescript-lang/rescript-compiler/issues/6485
  // let decoded = sampleJson2->Records.tOp_decode
  // t->testEqual(`decode omit optional field`, decoded, Ok(sampleRecord2))

  let sample3 = dict{}
  let sampleJson3 = sample3->JSON.Object

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
  let sample = dict{
    "n": JSON.Null,
    "n2": JSON.String("n2"),
  }
  let sampleJson = sample->JSON.Object

  let sampleRecord: Records.t2 = {
    o: None,
    n: Null.Null,
    n2: Null.Value("n2"),
  }

  let encoded = sampleRecord->Records.t2_encode
  t->testEqual(`encode`, encoded, sampleJson)

  let decoded = sampleJson->Records.t2_decode
  // ReScript omits option/optional fields in JS when constructing records directly,
  // but dynamically created records may have these fields as `undefined` at runtime.
  // This line ensures the test covers both cases.
  let _ = %raw(`sampleRecord["o"]= undefined`)
  let _ = %raw(`sampleRecord["on"]= undefined`)
  t->testEqual(`decode`, decoded, Ok(sampleRecord))
})

zoraBlock("record with spice.default", t => {
  let sample = dict{}
  let sampleJson = sample->JSON.Object

  let sample2 = dict{
    "value": JSON.Number(0.0),
    "value2": JSON.Number(1.0),
  }
  let sampleJson2 = sample2->JSON.Object

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
  let sample = dict{
    "a": JSON.Number(0.0),
    "b": JSON.Number(1.0),
  }
  let sampleJson = sample->JSON.Object

  let sampleRecord: Records.t4 = {
    a: 0n,
    b: 1n,
    c: None,
  }

  let encoded = sampleRecord->Records.t4_encode
  t->testEqual(`encode`, encoded, sampleJson)

  let decoded = sampleJson->Records.t4_decode
  // ReScript omits option/optional fields in JS when constructing records directly,
  // but dynamically created records may have these fields as `undefined` at runtime.
  // This line ensures the test covers both cases.
  let _ = %raw(`sampleRecord["c"] = undefined`)
  t->ok(deepEqualWithBigInt(decoded, Ok(sampleRecord)), "decode")
})

zoraBlock("nested record error path", t => {
  // Test that error paths correctly capture the full nested path
  // When inner.value fails to decode, the path should be ".one.value" not just ".value"
  let invalidJson = JSON.Object(
    dict{
      "one": JSON.Object(
        dict{
          "value": JSON.String("not an int"),
        },
      ),
    },
  )

  let decoded = invalidJson->Records.outer_decode
  t->test("error path includes full nested path", async t => {
    switch decoded {
    | Error({path}) => t->equal(path, ".one.value", "path should be .one.value")
    | Ok(_) => t->fail("expected decode to fail")
    }
  })
})

zoraBlock("deeply nested record error path", t => {
  // Test deeply nested paths: .level1.one.value
  let invalidJson = JSON.Object(
    dict{
      "level1": JSON.Object(
        dict{
          "one": JSON.Object(
            dict{
              "value": JSON.String("not an int"),
            },
          ),
        },
      ),
    },
  )

  let decoded = invalidJson->Records.deeplyNested_decode
  t->test("error path includes deeply nested path", async t => {
    switch decoded {
    | Error({path}) => t->equal(path, ".level1.one.value", "path should be .level1.one.value")
    | Ok(_) => t->fail("expected decode to fail")
    }
  })
})
