from __future__ import annotations
from typing import Callable, Generic, Optional, TypeVar, cast, overload, Any

T = TypeVar("T")
U = TypeVar("U")
C = TypeVar("C", bound="Maybe[Any]")


class Maybe(Generic[T]):
    present: bool
    value: Optional[T]

    @overload
    def __init__(self, /) -> None:
        ...

    @overload
    def __init__(self, value: T, /) -> None:
        ...

    def __init__(self, /, *args: T) -> None:
        if len(args) == 0:
            self.present = False
            self.value = None
        else:
            self.present = True
            self.value = args[0]

    @classmethod
    def Just(cls: type[C], value: T) -> C:
        return cls(value)

    @classmethod
    def Nothing(cls: type[C]) -> C:
        return cls()

    def assumePresent(self) -> T:
        assert self.present
        return cast(T, self.value)

    def map(self: Maybe[T], f: Callable[[T], U], /) -> Maybe[U]:
        if not self.present:
            return type(self)()
        else:
            return type(self)(f(cast(T, self.value)))

    def flatMap(self: Maybe[T], f: Callable[[T], Maybe[U]], /) -> Maybe[U]:
        if not self.present:
            return type(self)()
        else:
            value2 = f(cast(T, self.value))
            if not value2.present:
                return type(self)()
            else:
                return type(self)(cast(U, value2.value))

    def join(self: Maybe[Maybe[T]], /) -> Maybe[T]:
        cls = cast(type[Maybe[T]], type(self))
        if not self.present:
            return cls()
        elif not cast(Maybe[T], self.value).present:
            return cls()
        else:
            return cls(cast(T, cast(Maybe[T], self.value).value))

    @classmethod
    def fromOptional(cls: type[C], value: Optional[T], /) -> C:
        if value is None:
            return cls()
        else:
            return cls(value)

    @classmethod
    def withBool(cls: type[C], /, present: bool, value: T) -> C:
        if present:
            return cls(value)
        else:
            return cls()
