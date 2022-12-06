@spice
type status = WAITING | PROCESSING | SUCCESS | FAIL

let encoderStatus = v =>
  switch v {
  | WAITING => "waiting"
  | PROCESSING => "processing"
  | SUCCESS => "success"
  | FAIL => "fail"
  }->Js.Json.string

let decoderStatus = json => {
  switch json |> Js.Json.classify {
  | Js.Json.JSONString(str) =>
    switch str {
    | "waiting" => WAITING->Ok
    | "processing" => PROCESSING->Ok
    | "success" => SUCCESS->Ok
    | "fail" => FAIL->Ok
    | _ => Error({Spice.path: "", message: "Expected JSONString", value: json})
    }
  | _ => Error({Spice.path: "", message: "Expected JSONString", value: json})
  }
}

let codecStatus: Spice.codec<status> = (encoderStatus, decoderStatus)

@spice
type data = {status: @spice.codec(codecStatus) status}

let data = %raw(`
  {
    "status": "success"
  }
`)

let data = data->data_decode

let json = data->Result.getExn->data_encode
