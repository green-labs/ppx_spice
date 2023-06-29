let generate_codecs st cty =
  if Configs.get_uncurried () then Codecs_uncurried.generate_codecs st cty
  else Codecs_curried.generate_codecs st cty
