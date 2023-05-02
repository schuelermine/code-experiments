from inspect import signature, Parameter
from functools import wraps, update_wrapper
from typing import overload, Callable, Concatenate, Literal, TypeVar

T = TypeVar("T")
D = TypeVar("D")


@overload
def decorator(
    x: str | int,
) -> Callable[[Callable[..., T]], Callable[..., Callable[[D], T]]]:
    pass  # overload


@overload
def decorator(x: Callable[[D], T]) -> Callable[[D], T]:
    pass  # overload


def decorator(
    x: str | int | Callable[[D], T]
) -> (
    Callable[[D], T]
    | Callable[[Callable[..., T]], Callable[[D], T] | Callable[..., Callable[[D], T]]]
):
    """
    Transform a plain function that takes multiple parameters into a parametrized decorator in one of its parameters.
    Call as @decorator to take the value to be decorated as the first argument.
    Call as @decorator(key) to specify the positional or keyword argument that the function is passed to.
    If the resultant function would have no arguments, this step is elided.

    Example:
    > @decorator('f')
    > def replace(x, *, f):
    >     return x
    >
    > @replace(None)
    > def foo(): pass
    >
    > foo is None  # True

    > @decorator
    > def foo(f):
    >     return "foo"
    >
    > @foo
    > def bar(): pass
    >
    > bar == "foo"  # True
    """

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
                    if param.kind
                    in [Parameter.POSITIONAL_ONLY, Parameter.POSITIONAL_OR_KEYWORD]
                )
                > key
                or any(param.kind is Parameter.VAR_POSITIONAL for param in params)
            ):
                raise ValueError(
                    f"Argument to decorator() does not take an argument {key}"
                )

            i_key: int = key

            def modify_args(
                args: tuple[object, ...], kwargs: dict[str, object], val: D
            ) -> tuple[tuple[object, ...], dict[str, object]]:
                args_ = list(args)
                args_.insert(i_key, val)
                return tuple(args_), kwargs

        elif isinstance(key, str):
            if not any(
                param.kind is Parameter.VAR_KEYWORD
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

            s_key: str = key

            def modify_args(
                args: tuple[object, ...], kwargs: dict[str, object], val: D
            ) -> tuple[tuple[object, ...], dict[str, object]]:
                kwargs[s_key] = val
                return args, kwargs

        else:
            raise TypeError("Argument to decorator() must be int or str or callable")

        @wraps(dec)
        def new_dec_outer(*args: object, **kwargs: object) -> Callable[[D], T]:
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

    if callable(x):
        return _decorator(x, 0)
    else:
        y: str | int = x

        @wraps(decorator)
        def decorator_(
            f: Callable[..., T]
        ) -> Callable[[D], T] | Callable[..., Callable[[D], T]]:
            return _decorator(f, y)

        return decorator_
