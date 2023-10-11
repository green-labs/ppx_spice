let falseableEncode = (encoder, opt) =>
  switch opt {
  | None => Js.Json.Boolean(false)
  | Some(v) => encoder(v)
  }
let falseableDecode = (decoder, json) =>
  switch json {
  | Js.Json.Boolean(false) => Belt.Result.Ok(None)
  | _ => Belt.Result.map(decoder(json), v => Some(v))
  }
let falseable = (falseableEncode, falseableDecode)

let magicDecode = j => Belt.Result.Ok(Obj.magic(j))
let magic = (Obj.magic, magicDecode)
