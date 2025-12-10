@spice
type inner = {
  name: string,
  value: int,
}

@spice
type simpleResult = result<int, string>

@spice
type recordResult = result<inner, string>

@spice
type nestedResult = result<result<int, string>, string>
