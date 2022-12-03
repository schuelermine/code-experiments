from __future__ import annotations
from abc import ABC, abstractmethod
from typing import Callable, Generic, Optional, TypeVar, cast

T = TypeVar("T", covariant=True)
G = TypeVar("G")
U = TypeVar("U")


class Maybe(ABC, Generic[T]):
    present: bool
    value: Optional[T]

    @abstractmethod
    def assumePresent(self) -> T:
        pass

    @abstractmethod
    def map(self: Maybe[G], f: Callable[[G], U], /) -> Maybe[U]:
        pass

    @abstractmethod
    def flatMap(self: Maybe[G], f: Callable[[G], Maybe[U]], /) -> Maybe[U]:
        pass

    @abstractmethod
    def join(self: Maybe[Maybe[G]]) -> Maybe[G]:
        pass


class Just(Maybe[T]):
    def __init__(self: Just[T], value: T) -> None:
        self.present = True
        self.value = value

    def assumePresent(self: Just[G]) -> G:
        return cast(G, self.value)

    def map(self: Just[G], f: Callable[[G], U], /) -> Just[U]:
        return Just[U](f(cast(G, self.value)))

    def flatMap(self: Just[G], f: Callable[[G], Maybe[U]], /) -> Maybe[U]:
        return f(cast(G, self.value))

    def join(self: Just[Maybe[G]]) -> Maybe[G]:
        return cast(Maybe[G], self.value)

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
