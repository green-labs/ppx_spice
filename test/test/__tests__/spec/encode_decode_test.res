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

@spice.decode
type inner = {value: int}

@spice.decode
type outer = {inner: inner}

@spice.decode
type arrayWrapper = {arr: array<int>}

zoraBlock("decode error path", t => {
  t->test("nested record path", async t => {
    let json =
      Js.Dict.fromArray([
        ("inner", Js.Dict.fromArray([("value", Js.Json.string("not an int"))])->Js.Json.object_),
      ])->Js.Json.object_

    let result = json->outer_decode

    switch result {
    | Error(e) => t->equal(e.path, ".inner.value", "path should be .inner.value")
    | Ok(_) => t->fail("should have failed")
    }
  })

  t->test("array index path", async t => {
    let json =
      Js.Dict.fromArray([
        ("arr", Js.Json.array([Js.Json.number(1.0), Js.Json.string("bad")])),
      ])->Js.Json.object_

    let result = json->arrayWrapper_decode

    switch result {
    | Error(e) => t->equal(e.path, ".arr[1]", "path should be .arr[1]")
    | Ok(_) => t->fail("should have failed")
    }
  })
})

@spice.decode
type v = A(int)

zoraBlock("variant error path", t => {
  t->test("variant arg path", async t => {
    let json = [JSON.String("A"), JSON.String("bad")]->JSON.Array
    let result = json->v_decode
    switch result {
    | Error(e) => t->equal(e.path, "[1]", "path should be [1]")
    | Ok(_) => t->fail("should have failed")
    }
  })
})

@spice.decode
type pv = [#A(int) | #B(int, string, float)]

zoraBlock("polyvariant error path", t => {
  t->test("polyvariant case path", async t => {
    let json = [JSON.String("C"), JSON.String("bad")]->JSON.Array
    let result = json->pv_decode
    switch result {
    | Error(e) => {
        t->equal(
          e.message,
          "Invalid polymorphic constructor",
          "message should be 'Invalid polymorphic constructor'",
        )
        t->equal(e.path, "[0]", "path should be [0]")
      }
    | Ok(_) => t->fail("should have failed")
    }
  })

  t->test("polyvariant arg path", async t => {
    let json = [JSON.String("A"), JSON.String("bad")]->JSON.Array
    let result = json->pv_decode
    switch result {
    | Error(e) => t->equal(e.path, "[1]", "path should be [1]")
    | Ok(_) => t->fail("should have failed")
    }
  })

  t->test("polyvariant multi-arg path", async t => {
    let json =
      [JSON.String("B"), JSON.Number(1.0), JSON.String("good"), JSON.String("bad")]->JSON.Array
    let result = json->pv_decode
    switch result {
    | Error(e) => t->equal(e.path, "[3]", "path should be [3]")
    | Ok(_) => t->fail("should have failed")
    }
  })
})

@spice.decode
type tupleWrapper = {t: (int, string, int, float)}

@spice.decode
type dictWrapper = {d: Js.Dict.t<int>}

zoraBlock("other types error path", t => {
  t->test("tuple path", async t => {
    let json = dict{
      "t": [JSON.Number(1.0), JSON.String("ok"), JSON.String("bad"), JSON.Number(4.0)]->JSON.Array,
    }->JSON.Object

    let result = json->tupleWrapper_decode

    switch result {
    | Error(e) => t->equal(e.path, ".t[2]", "path should be .t[2]")
    | Ok(_) => t->fail("should have failed")
    }
  })

  t->test("dict path", async t => {
    let json = dict{
      "d": dict{
        "a": JSON.String("bad"),
      }->JSON.Object,
    }->JSON.Object

    let result = json->dictWrapper_decode

    switch result {
    | Error(e) => t->equal(e.path, ".d.a", "path should be .d.a")
    | Ok(_) => t->fail("should have failed")
    }
  })
})
