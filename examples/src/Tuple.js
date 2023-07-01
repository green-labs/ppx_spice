// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Spice = require("@greenlabs/ppx-spice/src/rescript/Spice.js");
var Js_dict = require("rescript/lib/js/js_dict.js");
var Belt_Option = require("rescript/lib/js/belt_Option.js");
var Belt_Result = require("rescript/lib/js/belt_Result.js");

function profile_encode(v) {
  return [
          Spice.stringToJson(v[0]),
          Spice.intToJson(v[1])
        ];
}

function profile_decode(v) {
  if (!Array.isArray(v) && (v === null || typeof v !== "object") && typeof v !== "number" && typeof v !== "string") {
    return Spice.error(undefined, "Not a tuple", v);
  }
  if (!Array.isArray(v)) {
    return Spice.error(undefined, "Not a tuple", v);
  }
  if (v.length !== 2) {
    return Spice.error(undefined, "Incorrect cardinality", v);
  }
  var v0 = v[0];
  var v1 = v[1];
  var match = Spice.stringFromJson(v0);
  var match$1 = Spice.intFromJson(v1);
  if (match.TAG === "Ok") {
    if (match$1.TAG === "Ok") {
      return {
              TAG: "Ok",
              _0: [
                match._0,
                match$1._0
              ]
            };
    }
    var e = match$1._0;
    return {
            TAG: "Error",
            _0: {
              path: "[1]" + e.path,
              message: e.message,
              value: e.value
            }
          };
  }
  var e$1 = match._0;
  return {
          TAG: "Error",
          _0: {
            path: "[0]" + e$1.path,
            message: e$1.message,
            value: e$1.value
          }
        };
}

function user_encode(v) {
  return Js_dict.fromArray(Spice.filterOptional([
                  [
                    "id",
                    false,
                    Spice.intToJson(v.id)
                  ],
                  [
                    "name",
                    false,
                    Spice.stringToJson(v.name)
                  ],
                  [
                    "profile",
                    false,
                    profile_encode(v.profile)
                  ]
                ]));
}

function user_decode(v) {
  if (!Array.isArray(v) && (v === null || typeof v !== "object") && typeof v !== "number" && typeof v !== "string") {
    return Spice.error(undefined, "Not an object", v);
  }
  if (!(typeof v === "object" && !Array.isArray(v))) {
    return Spice.error(undefined, "Not an object", v);
  }
  var id = Spice.intFromJson(Belt_Option.getWithDefault(Js_dict.get(v, "id"), null));
  if (id.TAG === "Ok") {
    var name = Spice.stringFromJson(Belt_Option.getWithDefault(Js_dict.get(v, "name"), null));
    if (name.TAG === "Ok") {
      var profile = profile_decode(Belt_Option.getWithDefault(Js_dict.get(v, "profile"), null));
      if (profile.TAG === "Ok") {
        return {
                TAG: "Ok",
                _0: {
                  id: id._0,
                  name: name._0,
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
  var e$2 = id._0;
  return {
          TAG: "Error",
          _0: {
            path: ".id" + e$2.path,
            message: e$2.message,
            value: e$2.value
          }
        };
}

var data = {
    "id": 1,
    "name": "woonki",
    "profile": ["mununki", 0]
  };

var user = user_decode(data);

var json = user_encode(Belt_Result.getExn(user));

exports.profile_encode = profile_encode;
exports.profile_decode = profile_decode;
exports.user_encode = user_encode;
exports.user_decode = user_decode;
exports.data = data;
exports.user = user;
exports.json = json;
/* user Not a pure module */
