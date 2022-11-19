from inspect import signature, Parameter
from typing import Any, Callable, ValuesView


def arg_passable(fun: Callable[..., Any], arg_ix: int) -> bool:
    params: ValuesView[Parameter] = signature(fun).parameters.values()
    return sum(
        1
        for param in params
        if param.kind in [Parameter.POSITIONAL_ONLY, Parameter.KEYWORD_ONLY]
    ) > arg_ix or any(param.kind == Parameter.VAR_POSITIONAL for param in params)


def kwarg_passable(fun: Callable[..., Any], arg_key: str) -> bool:
    params: ValuesView[Parameter] = signature(fun).parameters.values()
    return any(
        param.kind is Parameter.VAR_KEYWORD
        or (
            param.kind in [Parameter.KEYWORD_ONLY, Parameter.POSITIONAL_OR_KEYWORD]
            and param.name is arg_key
        )
        for param in params
    )
