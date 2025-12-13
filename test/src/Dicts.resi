@spice
type stringDict = dict<string>

@spice
type intDict = dict<int>

@spice
type inner = {
  name: string,
  value: int,
}

@spice
type recordDict = dict<inner>
