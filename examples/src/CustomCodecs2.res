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
  JSON.Object(
    dict{
      "name": JSON.String(v.name),
      "address_street": JSON.String(v.address.street),
      "address_number": v.address.number->Int.toFloat->JSON.Number,
    },
  )
}
let decoderUser = json =>
  switch json {
  | JSON.Object(dict{
      "name": JSON.String(name),
      "address_street": String(street),
      "address_number": Number(number),
    }) =>
    Ok({name, address: {street, number: number->Int.fromFloat}})
  | Object(_) => Error({Spice.path: "", message: "Expected name, street, number ", value: json})

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
let json = data->Result.getOrThrow->data_encode
