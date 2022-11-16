from inspect import signature, Parameter


def arg_passable(fun, arg_ix):
    params = signature(fun).parameters.values()
    return len(params) >= arg_ix + 1 or any(
        param.kind == Parameter.VAR_POSITIONAL for param in params
    )


def kwarg_passable(fun, arg_key):
    params = signature(fun).parameters.values()
    return any(
        param.kind == Parameter.VAR_KEYWORD
        or (
            param.kind in [Parameter.KEYWORD_ONLY, Parameter.POSITIONAL_OR_KEYWORD]
            and param.name == arg_key
        )
        for param in params
    )
