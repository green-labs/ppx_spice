@spice
type status = WAITING | PROCESSING | SUCCESS | FAIL

let encoderStatus = v =>
  switch v {
  | WAITING => "waiting"
  | PROCESSING => "processing"
  | SUCCESS => "success"
  | FAIL => "fail"
  }->JSON.String

let decoderStatus = json => {
  switch json {
  | JSON.String("waiting") => Ok(WAITING)
  | JSON.String("processing") => Ok(PROCESSING)
  | JSON.String("success") => Ok(SUCCESS)
  | JSON.String("fail") => Ok(FAIL)
  | JSON.String(str) =>
    Error({Spice.path: "", message: "Unexpected status value: " ++ str, value: json})
  | _ => Error({Spice.path: "", message: "Expected Json String", value: json})
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

let json = data->Result.getOrThrow->data_encode
