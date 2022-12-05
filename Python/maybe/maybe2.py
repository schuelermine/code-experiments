from __future__ import annotations
from abc import ABC, abstractmethod
from typing import Callable, Generic, Optional, TypeVar, cast, Any

T = TypeVar("T", covariant=True)
G = TypeVar("G")
U = TypeVar("U")


class Maybe(ABC, Generic[T]):
    present: bool
    value: Optional[T]

    @abstractmethod
    def assume_present(self) -> T:
        pass

    @abstractmethod
    def map(self: Maybe[G], f: Callable[[G], U], /) -> Maybe[U]:
        pass

    @abstractmethod
    def replace(self: Maybe[Any], value: U, /) -> Maybe[U]:
        pass

    @abstractmethod
    def and_then(self: Maybe[Any], maybe: Maybe[U], /) -> Maybe[U]:
        pass

    @abstractmethod
    def flatmap(self: Maybe[G], f: Callable[[G], Maybe[U]], /) -> Maybe[U]:
        pass

    @abstractmethod
    def join(self: Maybe[Maybe[G]]) -> Maybe[G]:
        pass

    @staticmethod
    def from_optional(value: Optional[G], /) -> Maybe[G]:
        if value is None:
            return Nothing[G]()
        else:
            return Just[G](value)

    @staticmethod
    def with_bool(present: bool, value: G) -> Maybe[G]:
        if present:
            return Just[G](value)
        else:
            return Nothing[G]()


class Just(Maybe[T]):
    def __init__(self: Just[T], value: T, /) -> None:
        self.present = True
        self.value = value

    def assume_present(self: Just[G]) -> G:
        return cast(G, self.value)

    def map(self: Just[G], f: Callable[[G], U], /) -> Just[U]:
        return Just[U](f(cast(G, self.value)))

    def replace(self: Just[Any], value: U, /) -> Just[U]:
        return Just[U](value)

    def and_then(self: Just[Any], maybe: Maybe[U]) -> Maybe[U]:
        return maybe

    def flatmap(self: Just[G], f: Callable[[G], Maybe[U]], /) -> Maybe[U]:
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

    def assume_present(self: Nothing[G]) -> G:
        raise MissingValueError()

    def map(self: Nothing[G], f: Callable[[G], U], /) -> Nothing[U]:
        return Nothing[U]()

    def replace(self: Nothing[Any], value: U, /) -> Nothing[U]:
        return Nothing[U]()

    def and_then(self: Nothing[Any], maybe: Maybe[U], /) -> Nothing[U]:
        return Nothing[U]()

    def flatmap(self: Nothing[G], f: Callable[[G], Maybe[U]], /) -> Nothing[U]:
        return Nothing[U]()

    def join(self: Nothing[Maybe[G]]) -> Nothing[G]:
        return Nothing[G]()

    __match_args__ = ()
