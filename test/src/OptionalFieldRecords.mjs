// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Spice from "./Spice.mjs";
import * as Js_dict from "rescript/lib/es6/Js_dict.js";
import * as Belt_Option from "rescript/lib/es6/Belt_Option.js";

function t0_encode(v) {
  let extra = v.b;
  return Js_dict.fromArray(Spice.filterOptional([
    [
      "a",
      Spice.intToJson(v.a)
    ],
    [
      "b",
      Spice.optionToJson(Spice.intToJson, extra)
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
  let match = Belt_Option.map(Js_dict.get(v, "a"), Spice.intFromJson);
  if (match === undefined) {
    return Spice.error(undefined, "a" + " missing", v);
  }
  if (match.TAG === "Ok") {
    let a = match._0;
    let match$1 = Belt_Option.map(Js_dict.get(v, "b"), extra => Spice.optionFromJson(Spice.intFromJson, extra));
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
    let e = match$1._0;
    return {
      TAG: "Error",
      _0: {
        path: "." + ("b" + e.path),
        message: e.message,
        value: e.value
      }
    };
  }
  let e$1 = match._0;
  return {
    TAG: "Error",
    _0: {
      path: "." + ("a" + e$1.path),
      message: e$1.message,
      value: e$1.value
    }
  };
}

function t1_encode(v) {
  let extra = v.bs;
  return Js_dict.fromArray(Spice.filterOptional([
    [
      "a",
      Spice.intToJson(v.a)
    ],
    [
      "bs",
      Spice.optionToJson(extra => Spice.arrayToJson(Spice.intToJson, extra), extra)
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
  let match = Belt_Option.map(Js_dict.get(v, "a"), Spice.intFromJson);
  if (match === undefined) {
    return Spice.error(undefined, "a" + " missing", v);
  }
  if (match.TAG === "Ok") {
    let a = match._0;
    let match$1 = Belt_Option.map(Js_dict.get(v, "bs"), extra => Spice.optionFromJson(extra => Spice.arrayFromJson(Spice.intFromJson, extra), extra));
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
    let e = match$1._0;
    return {
      TAG: "Error",
      _0: {
        path: "." + ("bs" + e.path),
        message: e.message,
        value: e.value
      }
    };
  }
  let e$1 = match._0;
  return {
    TAG: "Error",
    _0: {
      path: "." + ("a" + e$1.path),
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
  let extra = v.bs;
  return Js_dict.fromArray(Spice.filterOptional([
    [
      "a",
      Spice.intToJson(v.a)
    ],
    [
      "bs",
      Spice.optionToJson(extra => Spice.arrayToJson(b_encode, extra), extra)
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
  let match = Belt_Option.map(Js_dict.get(v, "a"), Spice.intFromJson);
  if (match === undefined) {
    return Spice.error(undefined, "a" + " missing", v);
  }
  if (match.TAG === "Ok") {
    let a = match._0;
    let match$1 = Belt_Option.map(Js_dict.get(v, "bs"), extra => Spice.optionFromJson(extra => Spice.arrayFromJson(b_decode, extra), extra));
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
    let e = match$1._0;
    return {
      TAG: "Error",
      _0: {
        path: "." + ("bs" + e.path),
        message: e.message,
        value: e.value
      }
    };
  }
  let e$1 = match._0;
  return {
    TAG: "Error",
    _0: {
      path: "." + ("a" + e$1.path),
      message: e$1.message,
      value: e$1.value
    }
  };
}

export {
  t0_encode,
  t0_decode,
  t1_encode,
  t1_decode,
  b_encode,
  b_decode,
  t2_encode,
  t2_decode,
}
/* No side effect */
