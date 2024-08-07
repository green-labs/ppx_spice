// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Spice from "./Spice.mjs";
import * as Js_dict from "rescript/lib/es6/js_dict.js";
import * as Belt_Option from "rescript/lib/es6/belt_Option.js";

function t0_encode(v) {
  return Js_dict.fromArray(Spice.filterOptional([
                  [
                    "a",
                    Spice.intToJson(v.a)
                  ],
                  [
                    "b",
                    Spice.optionToJson(Spice.intToJson, v.b)
                  ]
                ]));
}

function t0_decode(v) {
  if (!Array.isArray(v) && (v === null || typeof v !== "object") && typeof v !== "number" && typeof v !== "string" && typeof v !== "boolean") {
    return Spice.error(undefined, "Not an object", v);
  }
  if (!(typeof v === "object" && !Array.isArray(v))) {
    return Spice.error(undefined, "Not an object", v);
  }
  var match = Belt_Option.map(Js_dict.get(v, "a"), Spice.intFromJson);
  if (match === undefined) {
    return Spice.error(undefined, "a missing", v);
  }
  if (match.TAG === "Ok") {
    var a = match._0;
    var match$1 = Belt_Option.map(Js_dict.get(v, "b"), (function (param) {
            return Spice.optionFromJson(Spice.intFromJson, param);
          }));
    if (match$1 === undefined) {
      return {
              TAG: "Ok",
              _0: {
                a: a
              }
            };
    }
    if (match$1.TAG === "Ok") {
      return {
              TAG: "Ok",
              _0: {
                a: a,
                b: match$1._0
              }
            };
    }
    var e = match$1._0;
    return {
            TAG: "Error",
            _0: {
              path: ".b" + e.path,
              message: e.message,
              value: e.value
            }
          };
  }
  var e$1 = match._0;
  return {
          TAG: "Error",
          _0: {
            path: ".a" + e$1.path,
            message: e$1.message,
            value: e$1.value
          }
        };
}

function t1_encode(v) {
  return Js_dict.fromArray(Spice.filterOptional([
                  [
                    "a",
                    Spice.intToJson(v.a)
                  ],
                  [
                    "bs",
                    Spice.optionToJson((function (param) {
                            return Spice.arrayToJson(Spice.intToJson, param);
                          }), v.bs)
                  ]
                ]));
}

function t1_decode(v) {
  if (!Array.isArray(v) && (v === null || typeof v !== "object") && typeof v !== "number" && typeof v !== "string" && typeof v !== "boolean") {
    return Spice.error(undefined, "Not an object", v);
  }
  if (!(typeof v === "object" && !Array.isArray(v))) {
    return Spice.error(undefined, "Not an object", v);
  }
  var match = Belt_Option.map(Js_dict.get(v, "a"), Spice.intFromJson);
  if (match === undefined) {
    return Spice.error(undefined, "a missing", v);
  }
  if (match.TAG === "Ok") {
    var a = match._0;
    var match$1 = Belt_Option.map(Js_dict.get(v, "bs"), (function (param) {
            return Spice.optionFromJson((function (param) {
                          return Spice.arrayFromJson(Spice.intFromJson, param);
                        }), param);
          }));
    if (match$1 === undefined) {
      return {
              TAG: "Ok",
              _0: {
                a: a
              }
            };
    }
    if (match$1.TAG === "Ok") {
      return {
              TAG: "Ok",
              _0: {
                a: a,
                bs: match$1._0
              }
            };
    }
    var e = match$1._0;
    return {
            TAG: "Error",
            _0: {
              path: ".bs" + e.path,
              message: e.message,
              value: e.value
            }
          };
  }
  var e$1 = match._0;
  return {
          TAG: "Error",
          _0: {
            path: ".a" + e$1.path,
            message: e$1.message,
            value: e$1.value
          }
        };
}

function b_encode(v) {
  switch (v) {
    case "B0" :
        return "B0";
    case "B1" :
        return "B1";
    case "B2" :
        return "B2";
    
  }
}

function b_decode(v) {
  if (!Array.isArray(v) && (v === null || typeof v !== "object") && typeof v !== "number" && typeof v !== "string" && typeof v !== "boolean") {
    return Spice.error(undefined, "Not a JSONString", v);
  }
  switch (typeof v) {
    case "string" :
        if ("B0" === v) {
          return {
                  TAG: "Ok",
                  _0: "B0"
                };
        } else if ("B1" === v) {
          return {
                  TAG: "Ok",
                  _0: "B1"
                };
        } else if ("B2" === v) {
          return {
                  TAG: "Ok",
                  _0: "B2"
                };
        } else {
          return Spice.error(undefined, "Not matched", v);
        }
    case "number" :
        return Spice.error(undefined, "Not matched", v);
    default:
      return Spice.error(undefined, "Not a JSONString", v);
  }
}

function t2_encode(v) {
  return Js_dict.fromArray(Spice.filterOptional([
                  [
                    "a",
                    Spice.intToJson(v.a)
                  ],
                  [
                    "bs",
                    Spice.optionToJson((function (param) {
                            return Spice.arrayToJson(b_encode, param);
                          }), v.bs)
                  ]
                ]));
}

function t2_decode(v) {
  if (!Array.isArray(v) && (v === null || typeof v !== "object") && typeof v !== "number" && typeof v !== "string" && typeof v !== "boolean") {
    return Spice.error(undefined, "Not an object", v);
  }
  if (!(typeof v === "object" && !Array.isArray(v))) {
    return Spice.error(undefined, "Not an object", v);
  }
  var match = Belt_Option.map(Js_dict.get(v, "a"), Spice.intFromJson);
  if (match === undefined) {
    return Spice.error(undefined, "a missing", v);
  }
  if (match.TAG === "Ok") {
    var a = match._0;
    var match$1 = Belt_Option.map(Js_dict.get(v, "bs"), (function (param) {
            return Spice.optionFromJson((function (param) {
                          return Spice.arrayFromJson(b_decode, param);
                        }), param);
          }));
    if (match$1 === undefined) {
      return {
              TAG: "Ok",
              _0: {
                a: a
              }
            };
    }
    if (match$1.TAG === "Ok") {
      return {
              TAG: "Ok",
              _0: {
                a: a,
                bs: match$1._0
              }
            };
    }
    var e = match$1._0;
    return {
            TAG: "Error",
            _0: {
              path: ".bs" + e.path,
              message: e.message,
              value: e.value
            }
          };
  }
  var e$1 = match._0;
  return {
          TAG: "Error",
          _0: {
            path: ".a" + e$1.path,
            message: e$1.message,
            value: e$1.value
          }
        };
}

export {
  t0_encode ,
  t0_decode ,
  t1_encode ,
  t1_decode ,
  b_encode ,
  b_decode ,
  t2_encode ,
  t2_decode ,
}
/* No side effect */
