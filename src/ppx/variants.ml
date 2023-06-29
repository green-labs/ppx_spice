let generate_codecs st cds b =
  if Configs.get_uncurried () then Variants_uncurried.generate_codecs st cds b
  else Variants_curried.generate_codecs st cds b
