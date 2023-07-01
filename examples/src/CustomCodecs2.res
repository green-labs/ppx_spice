@spice
type address = {
  street: string,
  number: int,
}

@spice
type user = {
  name: string,
  address: address,
}

let encoderUser = (v: user) => {
  let streetJson = Js.Json.string(v.address.street)
  let numberJson = Js.Json.number(v.address.number->Int.toFloat)
  let nameJson = Js.Json.string(v.name)
  [("name", nameJson), ("address_street", streetJson), ("address_number", numberJson)]
  ->Js.Dict.fromArray
  ->Js.Json.object_
}
let decoderUser = json =>
  switch json->Js.Json.classify {
  | JSONObject(dict) => {
      let name = dict->Js.Dict.get("name")->Option.map(Js.Json.classify)
      let addressStreet = dict->Js.Dict.get("address_street")->Option.map(Js.Json.classify)
      let addressNumber = dict->Js.Dict.get("address_number")->Option.map(Js.Json.classify)
      switch (name, addressStreet, addressNumber) {
      | (
          Some(Js.Json.JSONString(name)),
          Some(Js.Json.JSONString(street)),
          Some(Js.Json.JSONNumber(number)),
        ) =>
        Ok({name, address: {street, number: number->Int.fromFloat}})
      | _ => Error({Spice.path: "", message: "Expected name, street, number ", value: json})
      }
    }
  | _ => Error({Spice.path: "", message: "Expected Json Object", value: json})
  }

let codecUser: Spice.codec<user> = (encoderUser, decoderUser)

@spice
type data = {user: @spice.codec(codecUser) user}

let data = %raw(`
{
  "user" : {
    "name": "woonki",
    "address_street": "Munjung",
    "address_number": 8
  }
}
`)

let data = data->data_decode
let json = data->Result.getExn->data_encode
