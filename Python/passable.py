from inspect import signature, Parameter
from typing import Callable


def arg_passable(fun: Callable, arg_ix: int) -> bool:
    params = signature(fun).parameters.values()
    return len(
        [
            1
            for param in params
            if param.kind in [Parameter.POSITIONAL_ONLY, Parameter.KEYWORD_ONLY]
        ]
    ) > arg_ix or any(param.kind == Parameter.VAR_POSITIONAL for param in params)


def kwarg_passable(fun: Callable, arg_key: str) -> bool:
    params = signature(fun).parameters.values()
    return any(
        param.kind is Parameter.VAR_KEYWORD
        or (
            param.kind in [Parameter.KEYWORD_ONLY, Parameter.POSITIONAL_OR_KEYWORD]
            and param.name is arg_key
        )
        for param in params
    )
