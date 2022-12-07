from __future__ import annotations
from typing import Callable, Optional, TypeVar, Protocol

T = TypeVar("T", covariant=True)
G = TypeVar("G")
U = TypeVar("U")


class Maybe(Protocol[T]):
    def assume_present(self) -> T:
        ...

    def map(self: Maybe[G], f: Callable[[G], U], /) -> Maybe[U]:
        ...

    def replace(self: Maybe[object], value: U, /) -> Maybe[U]:
        ...

    def and_then(self: Maybe[object], maybe: Maybe[U], /) -> Maybe[U]:
        ...

    def flatmap(self: Maybe[G], f: Callable[[G], Maybe[U]], /) -> Maybe[U]:
        ...

    def join(self: Maybe[Maybe[G]]) -> Maybe[G]:
        ...


class Just(Maybe[T]):
    value: T

    def __init__(self: Just[T], value: T, /) -> None:
        self.value = value

    def assume_present(self: Just[G]) -> G:
        return self.value

    def map(self: Just[G], f: Callable[[G], U], /) -> Just[U]:
        return Just[U](f(self.value))

    def replace(self: Just[object], value: U, /) -> Just[U]:
        return Just[U](value)

    def and_then(self: Just[object], maybe: Maybe[U], /) -> Maybe[U]:
        return maybe

    def flatmap(self: Just[G], f: Callable[[G], Maybe[U]], /) -> Maybe[U]:
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

    def assume_present(self: Nothing[G]) -> G:
        raise MissingValueError()

    def map(self: Nothing[G], f: Callable[[G], U], /) -> Nothing[U]:
        return Nothing[U]()

    def replace(self: Nothing[object], value: U, /) -> Nothing[U]:
        return Nothing[U]()

    def and_then(self: Nothing[object], maybe: Maybe[U], /) -> Nothing[U]:
        return Nothing[U]()

    def flatmap(self: Nothing[G], f: Callable[[G], Maybe[U]], /) -> Nothing[U]:
        return Nothing[U]()

    def join(self: Nothing[Maybe[G]]) -> Nothing[G]:
        return Nothing[G]()

    __match_args__ = ()


def maybe_from_optional(value: Optional[G], /) -> Maybe[G]:
    if value is None:
        return Nothing[G]()
    else:
        return Just[G](value)


def maybe_with_bool(present: bool, value: G) -> Maybe[G]:
    if present:
        return Just[G](value)
    else:
        return Nothing[G]()
