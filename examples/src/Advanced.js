// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Spice = require("@greenlabs/ppx-spice/src/rescript/Spice.js");
var Js_dict = require("rescript/lib/js/js_dict.js");
var Belt_Option = require("rescript/lib/js/belt_Option.js");

function language_encode(v) {
  return "ReScript";
}

function language_decode(v) {
  if (!Array.isArray(v) && (v === null || typeof v !== "object") && typeof v !== "number" && typeof v !== "string" && typeof v !== "boolean") {
    return Spice.error(undefined, "Not a JSONString", v);
  }
  switch (typeof v) {
    case "string" :
        if ("ReScript" === v) {
          return {
                  TAG: "Ok",
                  _0: "ReScript"
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

function profile_encode(v) {
  return Js_dict.fromArray(Spice.filterOptional([[
                    "languages",
                    true,
                    (function (extra) {
                          return Spice.optionToJson((function (extra) {
                                        return Spice.arrayToJson(language_encode, extra);
                                      }), extra);
                        })(v.languages)
                  ]]));
}

function profile_decode(v) {
  if (!Array.isArray(v) && (v === null || typeof v !== "object") && typeof v !== "number" && typeof v !== "string" && typeof v !== "boolean") {
    return Spice.error(undefined, "Not an object", v);
  }
  if (!(typeof v === "object" && !Array.isArray(v))) {
    return Spice.error(undefined, "Not an object", v);
  }
  var languages = (function (extra) {
        return Spice.optionFromJson((function (extra) {
                      return Spice.arrayFromJson(language_decode, extra);
                    }), extra);
      })(Belt_Option.getWithDefault(Js_dict.get(v, "languages"), null));
  if (languages.TAG === "Ok") {
    return {
            TAG: "Ok",
            _0: {
              languages: languages._0
            }
          };
  }
  var e = languages._0;
  return {
          TAG: "Error",
          _0: {
            path: ".languages" + e.path,
            message: e.message,
            value: e.value
          }
        };
}

function user_encode(v) {
  return Js_dict.fromArray(Spice.filterOptional([[
                    "profile",
                    false,
                    profile_encode(v.profile)
                  ]]));
}

function user_decode(v) {
  if (!Array.isArray(v) && (v === null || typeof v !== "object") && typeof v !== "number" && typeof v !== "string" && typeof v !== "boolean") {
    return Spice.error(undefined, "Not an object", v);
  }
  if (!(typeof v === "object" && !Array.isArray(v))) {
    return Spice.error(undefined, "Not an object", v);
  }
  var profile = profile_decode(Belt_Option.getWithDefault(Js_dict.get(v, "profile"), null));
  if (profile.TAG === "Ok") {
    return {
            TAG: "Ok",
            _0: {
              profile: profile._0
            }
          };
  }
  var e = profile._0;
  return {
          TAG: "Error",
          _0: {
            path: ".profile" + e.path,
            message: e.message,
            value: e.value
          }
        };
}

exports.language_encode = language_encode;
exports.language_decode = language_decode;
exports.profile_encode = profile_encode;
exports.profile_decode = profile_decode;
exports.user_encode = user_encode;
exports.user_decode = user_decode;
/* No side effect */
