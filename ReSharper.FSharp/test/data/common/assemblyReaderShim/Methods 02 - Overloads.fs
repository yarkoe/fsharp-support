module Module

open System.Text

let f = Foo()
f.Method()
f.Method("")
f.Method(StringBuilder())
f.Method(f)

Foo.StaticMethod()
Foo.StaticMethod("")
Foo.StaticMethod(StringBuilder())
Foo.StaticMethod(f)
