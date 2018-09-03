module Module

let f = Foo()
let (s1: string) = f.Field
let (s2: string) = f.ReadonlyField

f.Field <- ""
f.ReadonlyField <- ""
