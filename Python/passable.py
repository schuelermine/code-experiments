from inspect import signature, Parameter
from typing import Callable


def arg_passable(fun: Callable, arg_ix: int) -> bool:
    params = signature(fun).parameters.values()
    return len(params) >= arg_ix + 1 or any(
        param.kind == Parameter.VAR_POSITIONAL for param in params
    )


def kwarg_passable(fun: Callable, arg_key: str) -> bool:
    params = signature(fun).parameters.values()
    return any(
        param.kind == Parameter.VAR_KEYWORD
        or (
            param.kind in [Parameter.KEYWORD_ONLY, Parameter.POSITIONAL_OR_KEYWORD]
            and param.name == arg_key
        )
        for param in params
    )
