from __future__ import annotations
from typing import Generic, NoReturn, TypeVar

T = TypeVar("T", covariant=True)
U = TypeVar("U")

class Foo(Generic[T]):
    pass

def mkBar() -> Bar:
    return Bar()
    
def mkFoo(x: U) -> Foo[U]:
    return mkBar()

class Bar(Foo[NoReturn]):
    pass

