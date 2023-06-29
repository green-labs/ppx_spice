let generate_codecs st rfs b =
  if Configs.get_uncurried () then
    Polyvariants_uncurried.generate_codecs st rfs b
  else Polyvariants_curried.generate_codecs st rfs b
