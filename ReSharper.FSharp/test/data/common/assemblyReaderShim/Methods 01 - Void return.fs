module Module

let f = Foo()
let (u1: unit) = f.Method()
let (u2: unit) = Foo.StaticMethod()

let (uError1: unit) = Foo.Method()
let (uError2: unit) = f.StaticMethod()
