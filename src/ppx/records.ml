let generate_codecs st lds b =
  if Configs.get_uncurried () then Records_uncurried.generate_codecs st lds b
  else Records_curried.generate_codecs st lds b
