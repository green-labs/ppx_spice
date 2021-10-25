type decodeError = {
  path: string,
  message: string,
  value: string,
}

type result<'a> = Belt.Result.t<'a, decodeError>
type decoder<'a> = string => result<'a>
type encoder<'a> = 'a => string

let error = (~path=?, message, value) => {
  let path = switch path {
  | None => ""
  | Some(s) => s
  }
  Belt.Result.Error({path: path, message: message, value: value})
}
