@spice
type language =
  | @spice.as("ReScript") ReScript
  | @spice.as("OCaml") OCaml
  | @spice.as("TypeScript") TypeScript
  | @spice.as("JavaScript") JavaScript

@spice
type profile = {
  name?: string,
  company?: string,
  languages?: array<language>,
}

@spice
type user = {
  id: int,
  nickname?: string,
  age?: int,
  profile: profile,
}

let data = %raw(`
  {
    "id": 1,
    "nickname": "bob",
    "profile": {
      "name": "Bob",
      "company": "Facebook",
      "languages": ["OCaml"]
    }
  }
`)

let user: Belt.Result.t<user, Spice.decodeError> = data->user_decode

let json: Js.Json.t = user->Result.getExn->user_encode
