// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Zora from "zora";
import * as Records from "../../../src/Records.mjs";

function testEqual(t, name, lhs, rhs) {
  t.test(name, (async function (t) {
          t.equal(lhs, rhs, name);
        }));
}

Zora.test("record with @spice.key", (function (t) {
        var sample = {};
        sample["spice-label"] = "sample";
        sample["spice-value"] = 1.0;
        var sampleRecord = {
          label: "sample",
          value: 1
        };
        var encoded = Records.t_encode(sampleRecord);
        testEqual(t, "encode", encoded, sample);
        var decoded = Records.t_decode(sample);
        testEqual(t, "decode", decoded, {
              TAG: "Ok",
              _0: sampleRecord
            });
      }));

Zora.test("record without @spice.key", (function (t) {
        var sample = {};
        sample["label"] = "sample";
        sample["value"] = 1.0;
        var sampleRecord = {
          label: "sample",
          value: 1
        };
        var encoded = Records.t1_encode(sampleRecord);
        testEqual(t, "encode", encoded, sample);
        var decoded = Records.t1_decode(sample);
        testEqual(t, "decode", decoded, {
              TAG: "Ok",
              _0: sampleRecord
            });
      }));

Zora.test("record with optional field", (function (t) {
        var sample1 = {};
        sample1["label"] = "sample";
        sample1["value"] = 1.0;
        var sampleRecord1 = {
          label: "sample",
          value: 1
        };
        var encoded = Records.tOp_encode(sampleRecord1);
        testEqual(t, "encode", encoded, sample1);
        var decoded = Records.tOp_decode(sample1);
        testEqual(t, "decode", decoded, {
              TAG: "Ok",
              _0: sampleRecord1
            });
        var sample2 = {};
        sample2["label"] = "sample";
        var encoded$1 = Records.tOp_encode({
              label: "sample"
            });
        testEqual(t, "encode omit optional field", encoded$1, sample2);
        var sample3 = {};
        var encoded$2 = Records.tOp_encode({});
        testEqual(t, "encode omit optional field with None field", encoded$2, sample3);
      }));

Zora.test("record with null", (function (t) {
        var sample = {};
        sample["n"] = null;
        sample["n2"] = "n2";
        var sampleRecord_n = null;
        var sampleRecord = {
          n: sampleRecord_n,
          n2: "n2"
        };
        var encoded = Records.t2_encode(sampleRecord);
        testEqual(t, "encode", encoded, sample);
        var decoded = Records.t2_decode(sample);
        testEqual(t, "decode", decoded, {
              TAG: "Ok",
              _0: sampleRecord
            });
      }));

export {
  testEqual ,
}
/*  Not a pure module */
