// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Spice = require("@greenlabs/ppx-spice/src/rescript/Spice.js");
var Js_dict = require("rescript/lib/js/js_dict.js");
var Js_json = require("rescript/lib/js/js_json.js");
var Belt_Option = require("rescript/lib/js/belt_Option.js");
var Belt_Result = require("rescript/lib/js/belt_Result.js");

function address_encode(v) {
  return Js_dict.fromArray(Spice.filterOptional([
                  [
                    "street",
                    false,
                    Spice.stringToJson(v.street)
                  ],
                  [
                    "number",
                    false,
                    Spice.intToJson(v.number)
                  ]
                ]));
}

function address_decode(v) {
  if (!Array.isArray(v) && (v === null || typeof v !== "object") && typeof v !== "number" && typeof v !== "string" && typeof v !== "boolean") {
    return Spice.error(undefined, "Not an object", v);
  }
  if (!(typeof v === "object" && !Array.isArray(v))) {
    return Spice.error(undefined, "Not an object", v);
  }
  var street = Spice.stringFromJson(Belt_Option.getWithDefault(Js_dict.get(v, "street"), null));
  if (street.TAG === "Ok") {
    var number = Spice.intFromJson(Belt_Option.getWithDefault(Js_dict.get(v, "number"), null));
    if (number.TAG === "Ok") {
      return {
              TAG: "Ok",
              _0: {
                street: street._0,
                number: number._0
              }
            };
    }
    var e = number._0;
    return {
            TAG: "Error",
            _0: {
              path: ".number" + e.path,
              message: e.message,
              value: e.value
            }
          };
  }
  var e$1 = street._0;
  return {
          TAG: "Error",
          _0: {
            path: ".street" + e$1.path,
            message: e$1.message,
            value: e$1.value
          }
        };
}

function user_encode(v) {
  return Js_dict.fromArray(Spice.filterOptional([
                  [
                    "name",
                    false,
                    Spice.stringToJson(v.name)
                  ],
                  [
                    "address",
                    false,
                    address_encode(v.address)
                  ]
                ]));
}

function user_decode(v) {
  if (!Array.isArray(v) && (v === null || typeof v !== "object") && typeof v !== "number" && typeof v !== "string" && typeof v !== "boolean") {
    return Spice.error(undefined, "Not an object", v);
  }
  if (!(typeof v === "object" && !Array.isArray(v))) {
    return Spice.error(undefined, "Not an object", v);
  }
  var name = Spice.stringFromJson(Belt_Option.getWithDefault(Js_dict.get(v, "name"), null));
  if (name.TAG === "Ok") {
    var address = address_decode(Belt_Option.getWithDefault(Js_dict.get(v, "address"), null));
    if (address.TAG === "Ok") {
      return {
              TAG: "Ok",
              _0: {
                name: name._0,
                address: address._0
              }
            };
    }
    var e = address._0;
    return {
            TAG: "Error",
            _0: {
              path: ".address" + e.path,
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

function encoderUser(v) {
  var streetJson = v.address.street;
  var numberJson = v.address.number;
  var nameJson = v.name;
  return Js_dict.fromArray([
              [
                "name",
                nameJson
              ],
              [
                "address_street",
                streetJson
              ],
              [
                "address_number",
                numberJson
              ]
            ]);
}

function decoderUser(json) {
  var dict = Js_json.classify(json);
  if (typeof dict !== "object") {
    return {
            TAG: "Error",
            _0: {
              path: "",
              message: "Expected Json Object",
              value: json
            }
          };
  }
  if (dict.TAG !== "JSONObject") {
    return {
            TAG: "Error",
            _0: {
              path: "",
              message: "Expected Json Object",
              value: json
            }
          };
  }
  var dict$1 = dict._0;
  var name = Belt_Option.map(Js_dict.get(dict$1, "name"), Js_json.classify);
  var addressStreet = Belt_Option.map(Js_dict.get(dict$1, "address_street"), Js_json.classify);
  var addressNumber = Belt_Option.map(Js_dict.get(dict$1, "address_number"), Js_json.classify);
  if (name !== undefined && typeof name === "object" && name.TAG === "JSONString" && addressStreet !== undefined && typeof addressStreet === "object" && addressStreet.TAG === "JSONString" && addressNumber !== undefined && typeof addressNumber === "object" && addressNumber.TAG === "JSONNumber") {
    return {
            TAG: "Ok",
            _0: {
              name: name._0,
              address: {
                street: addressStreet._0,
                number: addressNumber._0 | 0
              }
            }
          };
  }
  return {
          TAG: "Error",
          _0: {
            path: "",
            message: "Expected name, street, number ",
            value: json
          }
        };
}

var codecUser = [
  encoderUser,
  decoderUser
];

function data_encode(v) {
  return Js_dict.fromArray(Spice.filterOptional([[
                    "user",
                    false,
                    encoderUser(v.user)
                  ]]));
}

function data_decode(v) {
  if (!Array.isArray(v) && (v === null || typeof v !== "object") && typeof v !== "number" && typeof v !== "string" && typeof v !== "boolean") {
    return Spice.error(undefined, "Not an object", v);
  }
  if (!(typeof v === "object" && !Array.isArray(v))) {
    return Spice.error(undefined, "Not an object", v);
  }
  var user = decoderUser(Belt_Option.getWithDefault(Js_dict.get(v, "user"), null));
  if (user.TAG === "Ok") {
    return {
            TAG: "Ok",
            _0: {
              user: user._0
            }
          };
  }
  var e = user._0;
  return {
          TAG: "Error",
          _0: {
            path: ".user" + e.path,
            message: e.message,
            value: e.value
          }
        };
}

var data = {
  "user" : {
    "name": "woonki",
    "address_street": "Munjung",
    "address_number": 8
  }
};

var data$1 = data_decode(data);

var json = data_encode(Belt_Result.getExn(data$1));

exports.address_encode = address_encode;
exports.address_decode = address_decode;
exports.user_encode = user_encode;
exports.user_decode = user_decode;
exports.encoderUser = encoderUser;
exports.decoderUser = decoderUser;
exports.codecUser = codecUser;
exports.data_encode = data_encode;
exports.data_decode = data_decode;
exports.data = data$1;
exports.json = json;
/* data Not a pure module */
