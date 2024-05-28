// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Spice from "./Spice.mjs";
import * as Js_dict from "rescript/lib/es6/js_dict.js";
import * as Belt_Option from "rescript/lib/es6/belt_Option.js";

function t_encode(v) {
  return Js_dict.fromArray(Spice.filterOptional([
                  [
                    "spice-label",
                    false,
                    Spice.stringToJson(v.label)
                  ],
                  [
                    "spice-value",
                    false,
                    Spice.intToJson(v.value)
                  ]
                ]));
}

function t_decode(v) {
  if (!Array.isArray(v) && (v === null || typeof v !== "object") && typeof v !== "number" && typeof v !== "string" && typeof v !== "boolean") {
    return Spice.error(undefined, "Not an object", v);
  }
  if (!(typeof v === "object" && !Array.isArray(v))) {
    return Spice.error(undefined, "Not an object", v);
  }
  var label = Spice.stringFromJson(Belt_Option.getWithDefault(Js_dict.get(v, "spice-label"), null));
  if (label.TAG === "Ok") {
    var value = Spice.intFromJson(Belt_Option.getWithDefault(Js_dict.get(v, "spice-value"), null));
    if (value.TAG === "Ok") {
      return {
              TAG: "Ok",
              _0: {
                label: label._0,
                value: value._0
              }
            };
    }
    var e = value._0;
    return {
            TAG: "Error",
            _0: {
              path: "." + ("spice-value" + e.path),
              message: e.message,
              value: e.value
            }
          };
  }
  var e$1 = label._0;
  return {
          TAG: "Error",
          _0: {
            path: "." + ("spice-label" + e$1.path),
            message: e$1.message,
            value: e$1.value
          }
        };
}

function t1_encode(v) {
  return Js_dict.fromArray(Spice.filterOptional([
                  [
                    "label",
                    false,
                    Spice.stringToJson(v.label)
                  ],
                  [
                    "value",
                    false,
                    Spice.intToJson(v.value)
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
  var label = Spice.stringFromJson(Belt_Option.getWithDefault(Js_dict.get(v, "label"), null));
  if (label.TAG === "Ok") {
    var value = Spice.intFromJson(Belt_Option.getWithDefault(Js_dict.get(v, "value"), null));
    if (value.TAG === "Ok") {
      return {
              TAG: "Ok",
              _0: {
                label: label._0,
                value: value._0
              }
            };
    }
    var e = value._0;
    return {
            TAG: "Error",
            _0: {
              path: ".value" + e.path,
              message: e.message,
              value: e.value
            }
          };
  }
  var e$1 = label._0;
  return {
          TAG: "Error",
          _0: {
            path: ".label" + e$1.path,
            message: e$1.message,
            value: e$1.value
          }
        };
}

function tOp_encode(v) {
  return Js_dict.fromArray(Spice.filterOptional([
                  [
                    "label",
                    false,
                    (function (extra) {
                          return Spice.optionToJson(Spice.stringToJson, extra);
                        })(v.label)
                  ],
                  [
                    "value",
                    true,
                    (function (extra) {
                          return Spice.optionToJson(Spice.intToJson, extra);
                        })(v.value)
                  ]
                ]));
}

function tOp_decode(v) {
  if (!Array.isArray(v) && (v === null || typeof v !== "object") && typeof v !== "number" && typeof v !== "string" && typeof v !== "boolean") {
    return Spice.error(undefined, "Not an object", v);
  }
  if (!(typeof v === "object" && !Array.isArray(v))) {
    return Spice.error(undefined, "Not an object", v);
  }
  var label = (function (extra) {
        return Spice.optionFromJson(Spice.stringFromJson, extra);
      })(Belt_Option.getWithDefault(Js_dict.get(v, "label"), null));
  if (label.TAG === "Ok") {
    var value = (function (extra) {
          return Spice.optionFromJson(Spice.intFromJson, extra);
        })(Belt_Option.getWithDefault(Js_dict.get(v, "value"), null));
    if (value.TAG === "Ok") {
      return {
              TAG: "Ok",
              _0: {
                label: label._0,
                value: value._0
              }
            };
    }
    var e = value._0;
    return {
            TAG: "Error",
            _0: {
              path: ".value" + e.path,
              message: e.message,
              value: e.value
            }
          };
  }
  var e$1 = label._0;
  return {
          TAG: "Error",
          _0: {
            path: ".label" + e$1.path,
            message: e$1.message,
            value: e$1.value
          }
        };
}

function t2_encode(v) {
  return Js_dict.fromArray(Spice.filterOptional([[
                    "nullable",
                    true,
                    (function (extra) {
                          return Spice.optionToJson(Spice.stringToJson, extra);
                        })(v.nullable)
                  ]]));
}

function t2_decode(v) {
  if (!Array.isArray(v) && (v === null || typeof v !== "object") && typeof v !== "number" && typeof v !== "string" && typeof v !== "boolean") {
    return Spice.error(undefined, "Not an object", v);
  }
  if (!(typeof v === "object" && !Array.isArray(v))) {
    return Spice.error(undefined, "Not an object", v);
  }
  var nullable = (function (extra) {
        return Spice.optionFromJson(Spice.stringFromJson, extra);
      })(Belt_Option.getWithDefault(Js_dict.get(v, "nullable"), null));
  if (nullable.TAG === "Ok") {
    return {
            TAG: "Ok",
            _0: {
              nullable: nullable._0
            }
          };
  }
  var e = nullable._0;
  return {
          TAG: "Error",
          _0: {
            path: ".nullable" + e.path,
            message: e.message,
            value: e.value
          }
        };
}

export {
  t_encode ,
  t_decode ,
  t1_encode ,
  t1_decode ,
  tOp_encode ,
  tOp_decode ,
  t2_encode ,
  t2_decode ,
}
/* No side effect */
