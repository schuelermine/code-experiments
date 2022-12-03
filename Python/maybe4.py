from __future__ import annotations


from typing import Callable, Optional, TypeVar, cast, Protocol


T = TypeVar("T", covariant=True)
G = TypeVar("G")
U = TypeVar("U")


class Maybe(Protocol[T]):
    def assumePresent(self) -> T:
        ...

    def map(self: Maybe[G], f: Callable[[G], U], /) -> Maybe[U]:
        ...

    def flatMap(self: Maybe[G], f: Callable[[G], Maybe[U]], /) -> Maybe[U]:
        ...

    def join(self: Maybe[Maybe[G]]) -> Maybe[G]:
        ...


class Just(Maybe[T]):
    value: T

    def __init__(self: Just[T], value: T) -> None:
        self.value = value

    def assumePresent(self: Just[G]) -> G:
        return self.value

    def map(self: Just[G], f: Callable[[G], U], /) -> Just[U]:
        return Just[U](f(self.value))

    def flatMap(self: Just[G], f: Callable[[G], Maybe[U]], /) -> Maybe[U]:
        return f(self.value)

    def join(self: Just[Maybe[G]]) -> Maybe[G]:
        return self.value

    __match_args__ = ("value",)


class MissingValueError(ValueError):
    "Raised to indicate a potentially missing value was missing."
    pass


class Nothing(Maybe[T]):
    def __init__(self: Nothing[T]) -> None:
        self.present = False
        self.value = None

    def assumePresent(self: Nothing[G]) -> G:
        raise MissingValueError()

    def map(self: Nothing[G], f: Callable[[G], U], /) -> Nothing[U]:
        return Nothing[U]()

    def flatMap(self: Nothing[G], f: Callable[[G], Maybe[U]], /) -> Nothing[U]:
        return Nothing[U]()

    def join(self: Nothing[Maybe[G]]) -> Nothing[G]:
        return Nothing[G]()

    __match_args__ = ()
