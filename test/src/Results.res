@spice
type intResult = result<int, string>

@spice
type inner = {
  id: int,
  name: string,
}

@spice
type innerResult = result<inner, string>

@spice
type nestedResult = result<result<int, string>, string>

// Result.t 형태 테스트
@spice
type resultT = Result.t<int, string>

// Belt.Result.t 형태 테스트
@spice
type beltResultT = Belt.Result.t<int, string>
