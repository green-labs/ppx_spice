@spice
type profile = (string, int)

@spice
type user = {
  id: int,
  name: string,
  profile: profile,
}

let data = %raw(`
  {
    "id": 1,
    "name": "woonki",
    "profile": ["mununki", 0]
  }
`)

let user: Belt.Result.t<user, Spice.decodeError> = data->user_decode

let json: Js.Json.t = user->Result.getExn->user_encode
