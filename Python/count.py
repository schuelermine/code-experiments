from types import NoneType
from typing import Callable, Iterable, TypeVar

T = TypeVar("T")


def count1(iterable: Iterable[T], condition: None | Callable[[T], bool] = None) -> int:
    if condition is None:
        return sum(1 for _ in iterable)

    return sum(1 for value in iterable if condition(value))


def count2(iterable: Iterable[T], condition: None | Callable[[T], bool] = None) -> int:
    if condition is None:
        return len([() for _ in iterable])

    return len([() for value in iterable if condition(value)])


def count3(iterable: Iterable[T], condition: None | Callable[[T], bool] = None) -> int:
    if condition is None:
        return sum(1 for _ in iterable)

    return sum(condition(value) for value in iterable)


def count4(iterable: Iterable[object]) -> int:
    return len(list(iterable))
