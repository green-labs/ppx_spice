let map_signature_item m sigi =
  if Configs.get_uncurried () then Signature_uncurried.map_signature_item m sigi
  else Signature_curried.map_signature_item m sigi
