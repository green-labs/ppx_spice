// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Belt_Result = require("rescript/lib/js/belt_Result.js");
var Caml_option = require("rescript/lib/js/caml_option.js");

function falseableEncode(encoder, opt) {
  if (opt !== undefined) {
    return encoder(Caml_option.valFromOption(opt));
  } else {
    return false;
  }
}

function falseableDecode(decoder, json) {
  if (!(!Array.isArray(json) && (json === null || typeof json !== "object") && typeof json !== "number" && typeof json !== "string" && typeof json !== "boolean") && typeof json === "boolean" && !json) {
    return {
            TAG: "Ok",
            _0: undefined
          };
  }
  return Belt_Result.map(decoder(json), (function (v) {
                return Caml_option.some(v);
              }));
}

var falseable = [
  falseableEncode,
  falseableDecode
];

function magicDecode(j) {
  return {
          TAG: "Ok",
          _0: j
        };
}

function magic_0(prim) {
  return prim;
}

var magic = [
  magic_0,
  magicDecode
];

exports.falseableEncode = falseableEncode;
exports.falseableDecode = falseableDecode;
exports.falseable = falseable;
exports.magicDecode = magicDecode;
exports.magic = magic;
/* No side effect */
