// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Spice from "./Spice.mjs";
import * as Js_dict from "rescript/lib/es6/js_dict.js";
import * as Belt_Option from "rescript/lib/es6/belt_Option.js";

function te_encode(v) {
  return Js_dict.fromArray(Spice.filterOptional([
                  [
                    "name",
                    false,
                    Spice.stringToJson(v.name)
                  ],
                  [
                    "nickname",
                    true,
                    (function (extra) {
                          return Spice.optionToJson(Spice.stringToJson, extra);
                        })(v.nickname)
                  ]
                ]));
}

function td_decode(v) {
  if (!Array.isArray(v) && (v === null || typeof v !== "object") && typeof v !== "number" && typeof v !== "string" && typeof v !== "boolean") {
    return Spice.error(undefined, "Not an object", v);
  }
  if (!(typeof v === "object" && !Array.isArray(v))) {
    return Spice.error(undefined, "Not an object", v);
  }
  var name = Spice.stringFromJson(Belt_Option.getWithDefault(Js_dict.get(v, "name"), null));
  if (name.TAG === "Ok") {
    var nickname = (function (extra) {
          return Spice.optionFromJson(Spice.stringFromJson, extra);
        })(Belt_Option.getWithDefault(Js_dict.get(v, "nickname"), null));
    if (nickname.TAG === "Ok") {
      return {
              TAG: "Ok",
              _0: {
                name: name._0,
                nickname: nickname._0
              }
            };
    }
    var e = nickname._0;
    return {
            TAG: "Error",
            _0: {
              path: ".nickname" + e.path,
              message: e.message,
              value: e.value
            }
          };
  }
  var e$1 = name._0;
  return {
          TAG: "Error",
          _0: {
            path: ".name" + e$1.path,
            message: e$1.message,
            value: e$1.value
          }
        };
}

export {
  te_encode ,
  td_decode ,
}
/* No side effect */
