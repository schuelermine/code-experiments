from __future__ import annotations
from typing import Callable, Generic, Optional, TypeVar, cast, overload

T = TypeVar("T")
U = TypeVar("U")
C = TypeVar("C", bound="Maybe[object]")


class MissingValueError(ValueError):
    "Raised to indicate a potentially missing value was missing."
    pass


class Maybe(Generic[T]):
    sentinel = object()

    value: T | object

    @overload
    def __init__(self, /) -> None:
        ...

    @overload
    def __init__(self, value: T, /) -> None:
        ...

    def __init__(self, /, *args: T) -> None:
        if len(args) == 0:
            self.value = Maybe.sentinel
        else:
            self.value = args[0]

    @classmethod
    def Just(cls: type[C], value: T, /) -> C:
        return cls(value)

    @classmethod
    def Nothing(cls: type[C]) -> C:
        return cls()

    def assume_present(self) -> T:
        cls = type(self)
        if self.value is cls.sentinel:
            raise MissingValueError()
        return cast(T, self.value)

    def map(self: Maybe[T], f: Callable[[T], U], /) -> Maybe[U]:
        cls = type(self)
        if self.value is cls.sentinel:
            return cls()
        else:
            return cls(f(cast(T, self.value)))

    def replace(self: Maybe[T], value: U, /) -> Maybe[U]:
        cls = type(self)
        if self.value is cls.sentinel:
            return cls()
        else:
            return cls(value)

    def then(self: Maybe[T], maybe: Maybe[U], /) -> Maybe[U]:
        cls = type(self)
        if self.value is cls.sentinel:
            return cls()
        else:
            return maybe

    def bind(self: Maybe[T], f: Callable[[T], Maybe[U]], /) -> Maybe[U]:
        cls = type(self)
        if self.value is cls.sentinel:
            return cls()
        else:
            maybe2 = f(cast(T, self.value))
            if maybe2.value is cls.sentinel:
                return cls()
            else:
                return cls(cast(U, maybe2.value))

    def join(self: Maybe[Maybe[T]], /) -> Maybe[T]:
        cls = cast(type[Maybe[T]], type(self))
        if self.value is cls.sentinel:
            return cls()
        elif cast(Maybe[T], self.value).value is cls().sentinel:
            return cls()
        else:
            return cls(cast(T, cast(Maybe[T], self.value).value))

    @classmethod
    def from_optional(cls: type[C], value: Optional[T], /) -> C:
        if value is None:
            return cls()
        else:
            return cls(value)

    @classmethod
    def with_bool(cls: type[C], /, present: bool, value: T) -> C:
        if present:
            return cls(value)
        else:
            return cls()
