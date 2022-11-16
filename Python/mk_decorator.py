from inspect import signature, Parameter
from functools import wraps


def mk_decorator(*args):
    """
    Transform a plain function that takes multiple parameters into a decorator in one of its parameters.
    Call as @mk_decorator to take the value to be decorated as the first argument.
    Call as @mk_decorator(key) to specify the positional or keyword argument that the function is passed to.

    Example:
    > @mk_decorator('f')
    > def replace(x, *, f):
    >     return x
    >
    > @replace(None)
    > def foo(): pass
    >
    > foo is None  # True
    """

    @wraps(mk_decorator)
    def _mk_decorator(dec, key):
        params = signature(dec).parameters.values()
        if len(params) == 1:
            return dec
        if isinstance(key, int):

            def modify_args(args, kwargs, fun):
                args = list(args)
                args.insert(key, fun)
                return args, kwargs

            if not len(params) >= key + 1 and not any(
                param.kind == Parameter.VAR_POSITIONAL for param in params
            ):
                raise ValueError(
                    f"Argument to mk_decorator() does not take an argument {key}"
                )
        elif isinstance(key, str):

            def modify_args(args, kwargs, fun):
                kwargs[key] = fun
                return args, kwargs

            if not any(
                param.kind == Parameter.VAR_KEYWORD
                or (
                    param.kind
                    in [Parameter.KEYWORD_ONLY, Parameter.POSITIONAL_OR_KEYWORD]
                    and param.name == key
                )
                for param in params
            ):
                raise ValueError(
                    f"Argument to mk_decorator() does not take argument {key}"
                )
        else:
            raise TypeError(
                "Argument to mk_decorator() must be an instance of int or str"
            )

        @wraps(dec)
        def new_dec_outer(*args, **kwargs):
            @wraps(dec)
            def new_dec_inner(fun):
                args_, kwargs_ = modify_args(args, kwargs, fun)
                new_fun = dec(*args_, **kwargs_)
                try:
                    new_fun = wraps(fun)(new_fun)
                except AttributeError:
                    pass
                return new_fun

            return new_dec_inner

        return new_dec_outer

    if len(args) != 1:
        raise TypeError(
            f"mk_decorator() takes exactly one argument ({len(args)} given)"
        )
    x = args[0]
    if callable(x):
        return _mk_decorator(x, 0)
    else:

        @wraps(mk_decorator)
        def mk_decorator_(f):
            return _mk_decorator(f, x)

        return mk_decorator_
