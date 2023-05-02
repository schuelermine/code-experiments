(lambda p: lambda u: lambda v: lambda w: p(u)(p(v)(w)))(
    lambda f: lambda g: lambda x: f(g(x))
)(print)(
    (lambda h: lambda i: lambda j: lambda y: h(y)(i)(j))(
        lambda c: (lambda a: lambda b: a) if c else (lambda a: lambda b: b)
    )("Yes!")("No.")
)(
    callable
)(
    1
)
