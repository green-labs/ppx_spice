let map_structure_item m stri =
  if Configs.get_uncurried () then Structure_uncurried.map_structure_item m stri
  else Structure_curried.map_structure_item m stri
