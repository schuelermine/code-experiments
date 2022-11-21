from inspect import signature, Parameter
from typing import Any, Callable, ValuesView


def passable(fun: Callable[..., Any], arg: str | int) -> bool:
    if not callable(fun):
        raise TypeError("Argument fun to passable() must be callable")
    if isinstance(arg, str):
        return kwarg_passable(fun, arg)
    elif isinstance(arg, int):
        return arg_passable(fun, arg)
    else:
        raise TypeError("Argument arg to passable() must be int or str")


def kwarg_passable(fun: Callable[..., Any], arg_key: str) -> bool:
    assert callable(fun)
    assert isinstance(arg_key, str)
    params: ValuesView[Parameter] = signature(fun).parameters.values()
    return any(
        param.kind is Parameter.VAR_KEYWORD
        or (
            param.kind in [Parameter.KEYWORD_ONLY, Parameter.POSITIONAL_OR_KEYWORD]
            and param.name == arg_key
        )
        for param in params
    )


def arg_passable(fun: Callable[..., Any], arg_ix: int) -> bool:
    assert callable(fun)
    assert isinstance(arg_ix, int)
    params: ValuesView[Parameter] = signature(fun).parameters.values()
    return sum(
        1
        for param in params
        if param.kind in [Parameter.POSITIONAL_ONLY, Parameter.POSITIONAL_OR_KEYWORD]
    ) > arg_ix or any(param.kind is Parameter.VAR_POSITIONAL for param in params)
