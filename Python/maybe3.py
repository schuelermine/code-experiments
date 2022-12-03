from dataclasses import dataclass
from typing import TypeAlias, TypeVar, Generic

T = TypeVar("T")


@dataclass
class Nothing(Generic[T]):
    pass


@dataclass
class Just(Generic[T]):
    value: T


Maybe: TypeAlias = Nothing | Just[T]
