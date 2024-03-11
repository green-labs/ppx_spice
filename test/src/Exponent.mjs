// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Spice from "./Spice.mjs";

function t_encode(v) {
  return "m³";
}

function t_decode(v) {
  if (!Array.isArray(v) && (v === null || typeof v !== "object") && typeof v !== "number" && typeof v !== "string" && typeof v !== "boolean") {
    return Spice.error(undefined, "Not a JSONString", v);
  }
  switch (typeof v) {
    case "string" :
        if ("m³" === v) {
          return {
                  TAG: "Ok",
                  _0: "M3"
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

function t_encode$1(v) {
  switch (v) {
    case "Create" :
        return "create";
    case "Update" :
        return "update";
    case "Delete" :
        return "delete";
    
  }
}

function t_decode$1(v) {
  if (!Array.isArray(v) && (v === null || typeof v !== "object") && typeof v !== "number" && typeof v !== "string" && typeof v !== "boolean") {
    return Spice.error(undefined, "Not a JSONString", v);
  }
  switch (typeof v) {
    case "string" :
        if ("create" === v) {
          return {
                  TAG: "Ok",
                  _0: "Create"
                };
        } else if ("update" === v) {
          return {
                  TAG: "Ok",
                  _0: "Update"
                };
        } else if ("delete" === v) {
          return {
                  TAG: "Ok",
                  _0: "Delete"
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

var Action = {
  t_encode: t_encode$1,
  t_decode: t_decode$1
};

export {
  t_encode ,
  t_decode ,
  Action ,
}
/* No side effect */