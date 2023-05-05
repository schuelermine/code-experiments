from __future__ import annotations
from typing import TypeVar, ParamSpec, Generic, Concatenate, overload, cast
from collections.abc import Callable
from functools import wraps

R = TypeVar("R", covariant=True)
P = ParamSpec("P")
Obj = TypeVar("Obj")


class hybridmethod(Generic[P, R, Obj]):
    __func__: Callable[Concatenate[type[Obj], Obj, P], R]

    def __init__(self, function: Callable[Concatenate[type[Obj], Obj, P], R], /):
        self.__func__ = function

    @overload
    def __get__(
        self, obj: None, objtype: type[Obj]
    ) -> Callable[Concatenate[Obj, P], R]:
        ...

    @overload
    def __get__(self, obj: Obj, objtype: type[Obj]) -> Callable[P, R]:
        ...

    def __get__(
        self, obj: Obj | None, objtype: type[Obj] | None = None
    ) -> Callable[Concatenate[Obj, P], R] | Callable[P, R]:
        if objtype is None and obj is not None:
            objtype = type(obj)

        assert objtype is not None, "descriptor called with not enough information"
        f: Callable[Concatenate[Obj, P], R] | Callable[P, R]
        if obj is None and isinstance(objtype, type):

            @wraps(self.__func__)
            def g(obj: Obj, *args: P.args, **kwargs: P.kwargs) -> R:
                return self.__func__(objtype, obj, *args, **kwargs)

            f = g
        else:
            assert obj is not None

            @wraps(self.__func__)
            def h(*args: P.args, **kwargs: P.kwargs) -> R:
                return self.__func__(objtype, obj, *args, **kwargs)

            f = h

        return f
