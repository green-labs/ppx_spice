@spice
type language =
  | @spice.as("ReScript") ReScript
  | @spice.as("OCaml") OCaml
  | @spice.as("TypeScript") TypeScript
  | @spice.as("JavaScript") JavaScript

@spice
type user = {
  id: int,
  nickname?: string,
  language: language,
}

let data = %raw(`
  {
    "id": 1,
    "nickname": "bob",
    "language": "ReScript"
  }
`)

let user: result<user, Spice.decodeError> = data->user_decode

let json: Js.Json.t = user->Result.getExn->user_encode

let language = Js.Json.string("ReScript")->language_decode
