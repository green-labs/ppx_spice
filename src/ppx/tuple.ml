let generate_encoder enc =
  if Configs.get_uncurried () then Tuple_uncurried.generate_encoder enc
  else Tuple_curried.generate_encoder enc

let generate_decoder dec =
  if Configs.get_uncurried () then Tuple_uncurried.generate_decoder dec
  else Tuple_curried.generate_decoder dec
