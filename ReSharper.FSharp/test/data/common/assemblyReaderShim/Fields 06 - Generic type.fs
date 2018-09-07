module Module

let fObj = Foo()
fObj.FooField = obj() |> ignore

let fString = Foo<string>()
fString.FooField = "" |> ignore

let fStringInferred = Foo()
fStringInferred.FooField = "" |> ignore

let fStringError = Foo<string>()
fStringError.FooField = 123 |> ignore
