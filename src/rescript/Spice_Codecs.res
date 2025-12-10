let falseableEncode = (encoder, opt) =>
  switch opt {
  | None => JSON.Boolean(false)
  | Some(v) => encoder(v)
  }
let falseableDecode = (decoder, json) =>
  switch json {
  | JSON.Boolean(false) => Ok(None)
  | _ => Result.map(decoder(json), v => Some(v))
  }
let falseable = (falseableEncode, falseableDecode)

let magicDecode = j => Ok(Obj.magic(j))
let magic = (Obj.magic, magicDecode)
