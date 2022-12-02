from passable import passable
from hypothesis import assume, given
from hypothesis.strategies import (
    composite,
    DrawFn,
    integers,
    characters,
    SearchStrategy,
    booleans,
    one_of,
    just,
    lists,
    none,
    data,
    DataObject,
)
from typing import Optional, TypeVar, Generic, TypeAlias
from dataclasses import dataclass
from inspect import Parameter, _ParameterKind, Signature
from unicodedata import normalize
from keyword import iskeyword
from pytest import raises


id_start_categories = ["Lu", "Ll", "Lt", "Lm", "Lo", "Nl"]
id_start_extras = (
    list(map(chr, range(0x1885, 0x1886)))
    + ["\u2118", "\u212e"]
    + list(map(chr, range(0x309B, 0x309C)))
)
id_continue_categories = id_start_categories + ["Mn", "Mc", "Nd", "Pc"]
id_continue_extras = (
    id_start_extras
    + ["\u00b7", "\u0387"]
    + list(map(chr, range(0x1369, 0x1372)))
    + ["\u19da"]
)


@composite
def identifiers(
    draw: DrawFn, max_length: Optional[int] = None, min_length: int = 1
) -> str:
    assert min_length < max_length if max_length is not None else True
    assert min_length >= 1
    length = draw(integers(min_value=min_length, max_value=max_length))
    string = ""
    string += draw(
        characters(
            whitelist_categories=id_start_categories,
            whitelist_characters=id_start_extras,
        )
    )
    for _ in range(length - 1):
        string += draw(
            characters(
                whitelist_categories=id_continue_categories,
                whitelist_characters=id_continue_extras,
            )
        )
    assume(is_valid_identifier(string))
    return string


def is_valid_identifier(name: str) -> bool:
    return (not iskeyword(name)) and name.isidentifier()


@given(identifiers(max_length=10))
def test_identifiers(name: str) -> None:
    assert is_valid_identifier(name)


parameter_kinds: SearchStrategy[_ParameterKind] = one_of(
    just(Parameter.POSITIONAL_OR_KEYWORD),
    just(Parameter.POSITIONAL_ONLY),
    just(Parameter.KEYWORD_ONLY),
    just(Parameter.VAR_KEYWORD),
    just(Parameter.VAR_POSITIONAL),
)


def prepare_paramkinds(kinds: list[_ParameterKind]) -> list[_ParameterKind]:
    kinds.sort()
    seen_var_positional = False
    seen_var_keyword = False
    indices_to_delete = []
    for i, kind in enumerate(kinds):
        if kind is Parameter.VAR_POSITIONAL:
            if seen_var_positional:
                indices_to_delete.append(i)
            else:
                seen_var_positional = True
        elif kind is Parameter.VAR_KEYWORD:
            if seen_var_keyword:
                indices_to_delete.append(i)
            else:
                seen_var_keyword = True
    return [kind for i, kind in enumerate(kinds) if i not in indices_to_delete]


sentinel = object()


@composite
def signatures(
    draw: DrawFn,
    /,
    defaults: SearchStrategy[object],
    *,
    max_param_count: Optional[int] = None,
    min_param_count: int = 0,
    max_identifier_length: Optional[int] = None,
    min_identifier_length: int = 1,
) -> Signature:
    assert min_param_count < max_param_count if max_param_count is not None else True
    assert min_param_count >= 0
    assert (
        min_identifier_length < max_identifier_length
        if max_identifier_length is not None
        else True
    )
    assert min_identifier_length >= 1
    param_kinds = draw(
        lists(parameter_kinds, max_size=max_param_count, min_size=min_param_count).map(
            prepare_paramkinds
        )
    )
    param_count = len(param_kinds)
    param_defaults = draw(
        lists(
            one_of(just(sentinel), defaults),
            min_size=param_count,
            max_size=param_count,
        ).map(lambda x: sorted(x, key=lambda y: 0 if y is sentinel else 1))
    )
    param_names = draw(
        lists(
            identifiers(
                max_length=max_identifier_length, min_length=min_identifier_length
            ),
            min_size=param_count,
            max_size=param_count,
            unique=True,
        )
    )
    return Signature(
        [
            Parameter(
                name,
                kind,
                default=default,
            )
            if kind not in [Parameter.VAR_POSITIONAL, Parameter.VAR_KEYWORD]
            and default is not sentinel
            else Parameter(name, kind)
            for name, kind, default in zip(param_names, param_kinds, param_defaults)
        ]
    )


class DummyFunction:
    def __init__(self, sig: Signature) -> None:
        self.__signature__ = sig

    def __call__(self, *args: object, **kwargs: object) -> None:
        self.__signature__.bind(*args, **kwargs)

    __signature__: Signature


@given(
    signatures(
        defaults=one_of(none(), integers(min_value=-333, max_value=333)),
        max_identifier_length=3,
        max_param_count=10000,
    ),
    one_of(identifiers(max_length=3), integers(min_value=0, max_value=100)),
)
def test_passable(sig: Signature, key: str | int) -> None:
    dummy = DummyFunction(sig)

    args: list[None]
    kwargs: dict[str, None]

    if isinstance(key, str):
        args = []
        kwargs = {}
        kwargs[key] = None
    else:
        args = [None] * (key + 1)
        kwargs = {}

    if not passable(dummy, key):
        with raises(TypeError):
            dummy(*args, **kwargs)


@given(
    signatures(
        defaults=one_of(none(), integers(min_value=-333, max_value=333)),
        max_identifier_length=3,
        max_param_count=10000,
    )
)
def test_signatures(sig: Signature) -> None:
    print(sig)
