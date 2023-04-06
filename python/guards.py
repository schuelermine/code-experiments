from typing import Callable


def foo(a: str | int) -> list[str]:
    x: list[str] = ["abc", "def"]
    if isinstance(a, int):
        x.insert(a, "ghi")
    elif isinstance(a, str):
        x.insert(0, a)
    return x


def bar(a: str | int) -> Callable[[list[str]], list[str]]:
    if isinstance(a, int):
        i_a: int = a

        def modify(x: list[str]) -> list[str]:
            x.insert(i_a, "ghi")
            return x

    elif isinstance(a, str):
        s_a: str = a

        def modify(x: list[str]) -> list[str]:
            x.insert(0, s_a)
            return x

    return modify
