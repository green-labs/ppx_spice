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

@spice
type userRaw = {
  name: string,
  address_street: string,
  address_number: int,
}

let data = %raw(`
{
  "name": "woonki",
  "address_street": "Munjung",
  "address_number": 8
}
`)

let data =
  data
  ->userRaw_decode
  ->Result.map(userRaw => {
    name: userRaw.name,
    address: {street: userRaw.address_street, number: userRaw.address_number},
  })

let json = data->Result.map(user =>
  {
    name: user.name,
    address_street: user.address.street,
    address_number: user.address.number,
  }->userRaw_encode
)
