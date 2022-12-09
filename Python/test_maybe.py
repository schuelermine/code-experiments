from maybe import Maybe, Nothing, Just
from typing import TypeVar
from hypothesis import assume, given
from hypothesis.strategies import (
    integers,
    SearchStrategy,
    just,
    one_of,
)


T = TypeVar("T")


def justs(values: SearchStrategy[T]) -> SearchStrategy[Just[T]]:
    return values.map(Just)


nothing: SearchStrategy[Nothing[object]] = just(Nothing())


def maybes(values: SearchStrategy[T]) -> SearchStrategy[Maybe[T]]:
    return one_of(nothing, justs(values))


@given(maybes(integers()))
def test_eq_reflexivity(maybe: Maybe[object]) -> None:
    assert maybe == maybe


@given(maybes(integers()), maybes(integers()))
def test_ne(maybe1: Maybe[int], maybe2: Maybe[int]) -> None:
    assume(not maybe1 == maybe2)
    assert maybe1 != maybe2
