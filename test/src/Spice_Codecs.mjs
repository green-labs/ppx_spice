// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Belt_Result from "rescript/lib/es6/Belt_Result.js";
import * as Primitive_option from "rescript/lib/es6/Primitive_option.js";

function falseableEncode(encoder, opt) {
  if (opt !== undefined) {
    return encoder(Primitive_option.valFromOption(opt));
  } else {
    return false;
  }
}

function falseableDecode(decoder, json) {
  if (json === false) {
    return {
      TAG: "Ok",
      _0: undefined
    };
  }
  return Belt_Result.map(decoder(json), v => Primitive_option.some(v));
}

let falseable = [
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

let magic = [
  magic_0,
  magicDecode
];

export {
  falseableEncode,
  falseableDecode,
  falseable,
  magicDecode,
  magic,
}
/* No side effect */
