from inspect import signature, Parameter
from functools import wraps, update_wrapper
from typing import overload, Callable, Concatenate, Literal, TypeVar, Any

T = TypeVar("T")
D = TypeVar("D")


@overload
def decorator(
    x: str | int,
) -> Callable[[Callable[..., T]], Callable[..., Callable[[D], T]]]:
    pass


@overload
def decorator(x: Callable[[D], T]) -> Callable[[D], T]:
    pass


def decorator(
    x: str | int | Callable[[D], T]
) -> Callable[[D], T] | Callable[
    [Callable[..., T]], Callable[[D], T] | Callable[..., Callable[[D], T]]
]:
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

    if callable(x):
        return _decorator(x, 0)
    else:

        y: str | int = x

        @wraps(decorator)
        def decorator_(
            f: Callable[..., T]
        ) -> Callable[[D], T] | Callable[..., Callable[[Any], T]]:
            return _decorator(f, y)

        return decorator_


@wraps(decorator)
def _decorator(
    dec: Callable[..., T], key: str | int
) -> Callable[[D], T] | Callable[..., Callable[[D], T]]:
    params = signature(dec).parameters.values()
    if len(params) == 1:
        return dec
    if isinstance(key, int):

        if not (
            sum(
                1
                for param in params
                if param.kind in [Parameter.POSITIONAL_ONLY, Parameter.KEYWORD_ONLY]
            )
            > key
            or any(param.kind is Parameter.VAR_POSITIONAL for param in params)
        ):
            raise ValueError(f"Argument to decorator() does not take an argument {key}")

        i_key: int = key

        def modify_args(
            args: tuple[Any, ...], kwargs: dict[str, Any], val: D
        ) -> tuple[tuple[Any, ...], dict[str, Any]]:
            args_ = list(args)
            args_.insert(i_key, val)
            return tuple(args_), kwargs

    elif isinstance(key, str):

        if not any(
            param.kind is Parameter.VAR_KEYWORD
            or (
                param.kind in [Parameter.KEYWORD_ONLY, Parameter.POSITIONAL_OR_KEYWORD]
                and param.name == key
            )
            for param in params
        ):
            raise ValueError(f"Argument to decorator() does not take argument {key}")

        s_key: str = key

        def modify_args(
            args: tuple[Any, ...], kwargs: dict[str, Any], val: D
        ) -> tuple[tuple[Any, ...], dict[str, Any]]:
            kwargs[s_key] = val
            return args, kwargs

    else:
        raise TypeError("Argument to decorator() must be an instance of int or str")

    @wraps(dec)
    def new_dec_outer(*args: Any, **kwargs: Any) -> Callable[[Any], T]:
        @wraps(dec)
        def new_dec_inner(val: D) -> T:
            args_, kwargs_ = modify_args(args, kwargs, val)
            new_fun = dec(*args_, **kwargs_)
            try:
                new_fun = update_wrapper(new_fun, val)  # type: ignore
            except AttributeError:
                pass
            return new_fun

        return new_dec_inner

    return new_dec_outer
