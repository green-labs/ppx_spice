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

let user: result<user, Spice.decodeError> = data->user_decode

let json: JSON.t = user->Result.getOrThrow->user_encode

let language = JSON.String("ReScript")->language_decode
