// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Jest = require("@glennsl/bs-jest/src/jest.js");
var Sample1 = require("../src/sample1.js");
var Sample2 = require("../src/sample2.js");

Jest.describe("ppx_spice test", (function (param) {
        Jest.test("polymorphic variant encode", (function (param) {
                var sample1Encoded = Sample1.t_encode("one");
                return Jest.Expect.toEqual(["one"], Jest.Expect.expect(sample1Encoded));
              }));
        Jest.test("polymorphic variant decode", (function (param) {
                var sample1Decoded = Sample1.t_decode(["two"]);
                return Jest.Expect.toEqual({
                            TAG: /* Ok */0,
                            _0: "two"
                          }, Jest.Expect.expect(sample1Decoded));
              }));
        Jest.test("variant encode", (function (param) {
                var sample2Encoded = Sample2.t_encode(/* One */0);
                return Jest.Expect.toEqual("first", Jest.Expect.expect(sample2Encoded));
              }));
        return Jest.test("variant decode", (function (param) {
                      var sample2Decoded = Sample2.t_decode("second");
                      return Jest.Expect.toEqual({
                                  TAG: /* Ok */0,
                                  _0: /* Two */1
                                }, Jest.Expect.expect(sample2Decoded));
                    }));
      }));

/*  Not a pure module */
