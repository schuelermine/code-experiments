from dataclasses import dataclass
from typing import Callable, TypeAlias, TypeVar, Generic, Optional, Any

T = TypeVar("T", covariant=True)
G = TypeVar("G")
U = TypeVar("U")


@dataclass
class Nothing(Generic[T]):
    pass


@dataclass
class Just(Generic[T]):
    value: T


Maybe: TypeAlias = Nothing | Just[T]


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


class MissingValueError(ValueError):
    "Raised to indicate a potentially missing value was missing."
    pass


def assume_present(maybe: Maybe[G], /) -> G:
    if isinstance(maybe, Nothing):
        raise MissingValueError
    return maybe.value


def map_maybe(f: Callable[[G], U], maybe: Maybe[G], /) -> Maybe[U]:
    if isinstance(maybe, Nothing):
        return Nothing[U]()
    else:
        return Just[U](f(maybe.value))


def replace_maybe(maybe: Maybe[Any], value: U, /) -> Maybe[U]:
    if isinstance(maybe, Nothing):
        return Nothing[U]()
    else:
        return Just[U](value)


def and_then_maybe(maybe1: Maybe[Any], maybe2: Maybe[U], /) -> Maybe[U]:
    if isinstance(maybe1, Nothing):
        return Nothing[U]()
    else:
        return maybe2


def flatmap_maybe(f: Callable[[G], Maybe[U]], maybe: Maybe[G], /) -> Maybe[U]:
    if isinstance(maybe, Nothing):
        return Nothing[U]()
    else:
        return f(maybe.value)


def join_maybe(maybe: Maybe[Maybe[G]]) -> Maybe[G]:
    if isinstance(maybe, Nothing):
        return Nothing[G]()
    else:
        return maybe.value
