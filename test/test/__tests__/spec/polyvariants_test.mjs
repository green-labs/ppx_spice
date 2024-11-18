// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Zora from "zora";
import * as Polyvariants from "../../../src/Polyvariants.mjs";

function testEqual(t, name, lhs, rhs) {
  t.test(name, async t => {
    t.equal(lhs, rhs, name);
  });
}

Zora.test("polymorphic variants with attribute", t => {
  let polyvariantEncoded = Polyvariants.t_encode("one");
  testEqual(t, "encode 하나", polyvariantEncoded, "하나");
  let polyvariantEncoded$1 = Polyvariants.t_encode("two");
  testEqual(t, "encode 둘", polyvariantEncoded$1, "둘");
  let polyvariantDecoded = Polyvariants.t_decode("하나");
  testEqual(t, "decode 하나", polyvariantDecoded, {
    TAG: "Ok",
    _0: "one"
  });
  let polyvariantDecoded$1 = Polyvariants.t_decode("둘");
  testEqual(t, "decode 둘", polyvariantDecoded$1, {
    TAG: "Ok",
    _0: "two"
  });
});

Zora.test("polymorphic variants", t => {
  let polyvariantEncoded = Polyvariants.t1_encode("one");
  testEqual(t, "encode #one", polyvariantEncoded, ["one"]);
  let polyvariantEncoded$1 = Polyvariants.t1_encode("two");
  testEqual(t, "encode #two", polyvariantEncoded$1, ["two"]);
  let polyvariantDecoded = Polyvariants.t1_decode(["one"]);
  testEqual(t, "decode one", polyvariantDecoded, {
    TAG: "Ok",
    _0: "one"
  });
  let polyvariantDecoded$1 = Polyvariants.t1_decode(["two"]);
  testEqual(t, "decode two", polyvariantDecoded$1, {
    TAG: "Ok",
    _0: "two"
  });
});

Zora.test("polymorphic variants with @spice.as number", t => {
  let polyvariantEncoded = Polyvariants.t2_encode("one");
  testEqual(t, "encode 1.0", polyvariantEncoded, 1.0);
  let polyvariantEncoded$1 = Polyvariants.t2_encode("two");
  testEqual(t, "encode 2.0", polyvariantEncoded$1, 2.0);
  let polyvariantDecoded = Polyvariants.t2_decode(1.0);
  testEqual(t, "decode 1.0", polyvariantDecoded, {
    TAG: "Ok",
    _0: "one"
  });
  let polyvariantDecoded$1 = Polyvariants.t2_decode(2.0);
  testEqual(t, "decode 2.0", polyvariantDecoded$1, {
    TAG: "Ok",
    _0: "two"
  });
});

export {
  testEqual,
}
/*  Not a pure module */
