module Module

type Bar() as this =
    inherit Foo()

    do
        this.Method(123)
        this.Method("")


type Baz() as this =
    let f = Foo()
    do
        f.Method(123)
        f.Method("")
