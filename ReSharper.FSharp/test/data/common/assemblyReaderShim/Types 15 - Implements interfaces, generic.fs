module Module

open System.Collections.Generic

let f = Foo<string>()
let ls = f :> IList<string>
let li = f :> IList<int>