@spice
type stringArray = array<string>

@spice
type intArray = array<int>

@spice
type floatArray = array<float>

@spice
type boolArray = array<bool>

@spice
type recordItem = {
  id: int,
  name: string,
}

@spice
type recordArray = array<recordItem>

@spice
type nestedArray = array<array<int>>
