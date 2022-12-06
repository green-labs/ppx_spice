@spice
type language =
  | ReScript(string)
  | OCaml
  | TypeScript
  | JavaScript

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
    "language": ["ReScript", "awesome"]
  }
`)

let user: Belt.Result.t<user, Spice.decodeError> = data->user_decode

let json: Js.Json.t = user->Result.getExn->user_encode

let language = Js.Json.string("ReScript")->language_decode
