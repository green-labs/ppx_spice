// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Spice from "./Spice.mjs";
import * as Js_dict from "rescript/lib/es6/js_dict.js";
import * as Belt_Option from "rescript/lib/es6/belt_Option.js";

function t_encode(v) {
  return Js_dict.fromArray(Spice.filterOptional([
                  [
                    "spice-label",
                    Spice.stringToJson(v.label)
                  ],
                  [
                    "spice-value",
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
  var match = Belt_Option.map(Js_dict.get(v, "spice-label"), Spice.stringFromJson);
  if (match === undefined) {
    return Spice.error(undefined, "spice-label" + " missing", v);
  }
  if (match.TAG === "Ok") {
    var match$1 = Belt_Option.map(Js_dict.get(v, "spice-value"), Spice.intFromJson);
    if (match$1 === undefined) {
      return Spice.error(undefined, "spice-value" + " missing", v);
    }
    if (match$1.TAG === "Ok") {
      return {
              TAG: "Ok",
              _0: {
                label: match._0,
                value: match$1._0
              }
            };
    }
    var e = match$1._0;
    return {
            TAG: "Error",
            _0: {
              path: "." + ("spice-value" + e.path),
              message: e.message,
              value: e.value
            }
          };
  }
  var e$1 = match._0;
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
                    Spice.stringToJson(v.label)
                  ],
                  [
                    "value",
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
  var match = Belt_Option.map(Js_dict.get(v, "label"), Spice.stringFromJson);
  if (match === undefined) {
    return Spice.error(undefined, "label missing", v);
  }
  if (match.TAG === "Ok") {
    var match$1 = Belt_Option.map(Js_dict.get(v, "value"), Spice.intFromJson);
    if (match$1 === undefined) {
      return Spice.error(undefined, "value missing", v);
    }
    if (match$1.TAG === "Ok") {
      return {
              TAG: "Ok",
              _0: {
                label: match._0,
                value: match$1._0
              }
            };
    }
    var e = match$1._0;
    return {
            TAG: "Error",
            _0: {
              path: ".value" + e.path,
              message: e.message,
              value: e.value
            }
          };
  }
  var e$1 = match._0;
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
                    Spice.optionToJson(Spice.stringToJson, v.label)
                  ],
                  [
                    "value",
                    Spice.optionToJson(Spice.intToJson, v.value)
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
  var match = Belt_Option.map(Js_dict.get(v, "label"), (function (param) {
          return Spice.optionFromJson(Spice.stringFromJson, param);
        }));
  if (match !== undefined) {
    if (match.TAG === "Ok") {
      var label = match._0;
      var match$1 = Belt_Option.map(Js_dict.get(v, "value"), (function (param) {
              return Spice.optionFromJson(Spice.intFromJson, param);
            }));
      if (match$1 === undefined) {
        return {
                TAG: "Ok",
                _0: {
                  label: label
                }
              };
      }
      if (match$1.TAG === "Ok") {
        return {
                TAG: "Ok",
                _0: {
                  label: label,
                  value: match$1._0
                }
              };
      }
      var e = match$1._0;
      return {
              TAG: "Error",
              _0: {
                path: ".value" + e.path,
                message: e.message,
                value: e.value
              }
            };
    }
    var e$1 = match._0;
    return {
            TAG: "Error",
            _0: {
              path: ".label" + e$1.path,
              message: e$1.message,
              value: e$1.value
            }
          };
  }
  var match$2 = Belt_Option.map(Js_dict.get(v, "value"), (function (param) {
          return Spice.optionFromJson(Spice.intFromJson, param);
        }));
  if (match$2 === undefined) {
    return {
            TAG: "Ok",
            _0: {}
          };
  }
  if (match$2.TAG === "Ok") {
    return {
            TAG: "Ok",
            _0: {
              value: match$2._0
            }
          };
  }
  var e$2 = match$2._0;
  return {
          TAG: "Error",
          _0: {
            path: ".value" + e$2.path,
            message: e$2.message,
            value: e$2.value
          }
        };
}

function t2_encode(v) {
  return Js_dict.fromArray(Spice.filterOptional([
                  [
                    "o",
                    Spice.optionToJson(Spice.stringToJson, v.o)
                  ],
                  [
                    "n",
                    Spice.nullToJson(Spice.stringToJson, v.n)
                  ],
                  [
                    "on",
                    Spice.optionToJson((function (param) {
                            return Spice.nullToJson(Spice.stringToJson, param);
                          }), v.on)
                  ],
                  [
                    "n2",
                    Spice.nullToJson(Spice.stringToJson, v.n2)
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
  var match = Belt_Option.map(Js_dict.get(v, "o"), (function (param) {
          return Spice.optionFromJson(Spice.stringFromJson, param);
        }));
  if (match !== undefined) {
    if (match.TAG === "Ok") {
      var o = match._0;
      var match$1 = Belt_Option.map(Js_dict.get(v, "n"), (function (param) {
              return Spice.nullFromJson(Spice.stringFromJson, param);
            }));
      if (match$1 === undefined) {
        return Spice.error(undefined, "n missing", v);
      }
      if (match$1.TAG === "Ok") {
        var n = match$1._0;
        var match$2 = Belt_Option.map(Js_dict.get(v, "on"), (function (param) {
                return Spice.optionFromJson((function (param) {
                              return Spice.nullFromJson(Spice.stringFromJson, param);
                            }), param);
              }));
        if (match$2 !== undefined) {
          if (match$2.TAG === "Ok") {
            var match$3 = Belt_Option.map(Js_dict.get(v, "n2"), (function (param) {
                    return Spice.nullFromJson(Spice.stringFromJson, param);
                  }));
            if (match$3 === undefined) {
              return Spice.error(undefined, "n2 missing", v);
            }
            if (match$3.TAG === "Ok") {
              return {
                      TAG: "Ok",
                      _0: {
                        o: o,
                        n: n,
                        on: match$2._0,
                        n2: match$3._0
                      }
                    };
            }
            var e = match$3._0;
            return {
                    TAG: "Error",
                    _0: {
                      path: ".n2" + e.path,
                      message: e.message,
                      value: e.value
                    }
                  };
          }
          var e$1 = match$2._0;
          return {
                  TAG: "Error",
                  _0: {
                    path: ".on" + e$1.path,
                    message: e$1.message,
                    value: e$1.value
                  }
                };
        }
        var match$4 = Belt_Option.map(Js_dict.get(v, "n2"), (function (param) {
                return Spice.nullFromJson(Spice.stringFromJson, param);
              }));
        if (match$4 === undefined) {
          return Spice.error(undefined, "n2 missing", v);
        }
        if (match$4.TAG === "Ok") {
          return {
                  TAG: "Ok",
                  _0: {
                    o: o,
                    n: n,
                    n2: match$4._0
                  }
                };
        }
        var e$2 = match$4._0;
        return {
                TAG: "Error",
                _0: {
                  path: ".n2" + e$2.path,
                  message: e$2.message,
                  value: e$2.value
                }
              };
      }
      var e$3 = match$1._0;
      return {
              TAG: "Error",
              _0: {
                path: ".n" + e$3.path,
                message: e$3.message,
                value: e$3.value
              }
            };
    }
    var e$4 = match._0;
    return {
            TAG: "Error",
            _0: {
              path: ".o" + e$4.path,
              message: e$4.message,
              value: e$4.value
            }
          };
  }
  var match$5 = Belt_Option.map(Js_dict.get(v, "n"), (function (param) {
          return Spice.nullFromJson(Spice.stringFromJson, param);
        }));
  if (match$5 === undefined) {
    return Spice.error(undefined, "n missing", v);
  }
  if (match$5.TAG === "Ok") {
    var n$1 = match$5._0;
    var match$6 = Belt_Option.map(Js_dict.get(v, "on"), (function (param) {
            return Spice.optionFromJson((function (param) {
                          return Spice.nullFromJson(Spice.stringFromJson, param);
                        }), param);
          }));
    if (match$6 !== undefined) {
      if (match$6.TAG === "Ok") {
        var match$7 = Belt_Option.map(Js_dict.get(v, "n2"), (function (param) {
                return Spice.nullFromJson(Spice.stringFromJson, param);
              }));
        if (match$7 === undefined) {
          return Spice.error(undefined, "n2 missing", v);
        }
        if (match$7.TAG === "Ok") {
          return {
                  TAG: "Ok",
                  _0: {
                    n: n$1,
                    on: match$6._0,
                    n2: match$7._0
                  }
                };
        }
        var e$5 = match$7._0;
        return {
                TAG: "Error",
                _0: {
                  path: ".n2" + e$5.path,
                  message: e$5.message,
                  value: e$5.value
                }
              };
      }
      var e$6 = match$6._0;
      return {
              TAG: "Error",
              _0: {
                path: ".on" + e$6.path,
                message: e$6.message,
                value: e$6.value
              }
            };
    }
    var match$8 = Belt_Option.map(Js_dict.get(v, "n2"), (function (param) {
            return Spice.nullFromJson(Spice.stringFromJson, param);
          }));
    if (match$8 === undefined) {
      return Spice.error(undefined, "n2 missing", v);
    }
    if (match$8.TAG === "Ok") {
      return {
              TAG: "Ok",
              _0: {
                n: n$1,
                n2: match$8._0
              }
            };
    }
    var e$7 = match$8._0;
    return {
            TAG: "Error",
            _0: {
              path: ".n2" + e$7.path,
              message: e$7.message,
              value: e$7.value
            }
          };
  }
  var e$8 = match$5._0;
  return {
          TAG: "Error",
          _0: {
            path: ".n" + e$8.path,
            message: e$8.message,
            value: e$8.value
          }
        };
}

function t3_encode(v) {
  return Js_dict.fromArray(Spice.filterOptional([
                  [
                    "value",
                    Spice.intToJson(v.value)
                  ],
                  [
                    "value2",
                    Spice.optionToJson(Spice.intToJson, v.value2)
                  ]
                ]));
}

function t3_decode(v) {
  if (!Array.isArray(v) && (v === null || typeof v !== "object") && typeof v !== "number" && typeof v !== "string" && typeof v !== "boolean") {
    return Spice.error(undefined, "Not an object", v);
  }
  if (!(typeof v === "object" && !Array.isArray(v))) {
    return Spice.error(undefined, "Not an object", v);
  }
  var match = Belt_Option.getWithDefault(Belt_Option.map(Js_dict.get(v, "value"), Spice.intFromJson), {
        TAG: "Ok",
        _0: 0
      });
  if (match.TAG === "Ok") {
    var match$1 = Belt_Option.getWithDefault(Belt_Option.map(Js_dict.get(v, "value2"), (function (param) {
                return Spice.optionFromJson(Spice.intFromJson, param);
              })), {
          TAG: "Ok",
          _0: 1
        });
    if (match$1.TAG === "Ok") {
      return {
              TAG: "Ok",
              _0: {
                value: match._0,
                value2: match$1._0
              }
            };
    }
    var e = match$1._0;
    return {
            TAG: "Error",
            _0: {
              path: ".value2" + e.path,
              message: e.message,
              value: e.value
            }
          };
  }
  var e$1 = match._0;
  return {
          TAG: "Error",
          _0: {
            path: ".value" + e$1.path,
            message: e$1.message,
            value: e$1.value
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
  t3_encode ,
  t3_decode ,
}
/* No side effect */
