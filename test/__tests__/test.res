open Jest
open Expect

Sample1.t_decode->Js.Console.log

describe("init", _ => {
  test("actually running?", _ => {
    expect("a") |> toEqual("a")
  })
})
