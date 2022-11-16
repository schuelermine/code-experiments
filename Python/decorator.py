from inspect import signature, Parameter
from functools import wraps


def decorator(*args):
    """
Transform a plain function that takes multiple parameters into a decorator in one of its parameters.
Call as @decorator to take the value to be decorated as the first argument.
Call as @decorator(key) to specify the positional or keyword argument that the function is passed to.

Example:
> @decorator('f')
> def replace(x, *, f):
>     return x
>
> @replace(None)
> def foo(): pass
>
> foo is None  # True
    """

    @wraps(decorator)
    def _decorator(dec, key):
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
                    f"Argument to decorator() does not take an argument {key}"
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
                    f"Argument to decorator() does not take argument {key}"
                )
        else:
            raise TypeError("Argument to decorator() must be an instance of int or str")

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
        raise TypeError(f"decorator() takes exactly one argument ({len(args)} given)")
    x = args[0]
    if callable(x):
        return _decorator(x, 0)
    else:

        @wraps(decorator)
        def decorator_(f):
            return _decorator(f, x)

        return decorator_
