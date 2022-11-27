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
    DataObject
)
from typing import Optional, TypeVar, Generic
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
def identifiers(draw: DrawFn, max_length: Optional[int] = None) -> str:
    length = draw(integers(min_value=1, max_value=max_length))
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
    assume(not iskeyword(string))
    return string


@given(identifiers(max_length=10))
def test_identifiers(name: str) -> None:
    assert is_valid_identifier(name)


def is_valid_identifier(name: str) -> bool:
    return (not iskeyword(name)) and name.isidentifier()


@composite
def parameters(
    draw: DrawFn,
    default: Optional[SearchStrategy[object]] = None,
    have_default: bool = False,
    kind: Optional[_ParameterKind] = None,
    max_length: Optional[int] = None,
) -> Parameter:
    if kind is None:
        kind = draw(parameter_kinds)
    assert kind is not None
    assert (not have_default) or default is not None
    if have_default and draw(booleans()) and kind not in [Parameter.VAR_POSITIONAL, Parameter.VAR_KEYWORD]:
        assert default is not None  # Satisfies the type checker. This assert should never fail.
        return Parameter(draw(identifiers(max_length=max_length)), kind, default=draw(default))
    else:
        return Parameter(draw(identifiers(max_length=max_length)), kind)


parameter_kinds: SearchStrategy[_ParameterKind] = one_of(
    just(Parameter.POSITIONAL_OR_KEYWORD),
    just(Parameter.POSITIONAL_ONLY),
    just(Parameter.KEYWORD_ONLY),
    just(Parameter.VAR_KEYWORD),
    just(Parameter.VAR_POSITIONAL),
)


T = TypeVar('T')


class ParamProto(Generic[T]):
    def __init__(self) -> None:
        self.has_kind = False
        self.kind = None
        self.has_name = False
        self.name = None
        self.has_default = False
        self.default = None

    def set_kind(self, kind: _ParameterKind) -> None:
        self.has_kind = True
        self.kind = kind

    def set_name(self, name: str) -> None:
        self.has_name = True
        self.name = name
    
    def set_default(self, default: T) -> None:
        if self.kind in [Parameter.VAR_POSITIONAL, Parameter.VAR_KEYWORD]:
            return
        self.has_default = True
        self.default = default
    
    def unset_default(self) -> None:
        self.has_default = False
        self.default = None

    @property
    def is_complete(self) -> bool:
        return self.has_kind and self.has_name
    
    @property
    def is_consistent(self) -> bool:
        return (self.kind is None or self.has_kind) and (self.name is None or self.has_name) and (self.name is None or self.has_default)
    
    def reify(self) -> Parameter:
        assert self.is_consistent
        assert self.is_complete
        assert self.kind is not None
        assert self.name is not None
        if self.has_default:
            return Parameter(self.name, self.kind, default=self.default)
        else:
            return Parameter(self.name, self.kind)

    has_kind: bool
    kind: Optional[_ParameterKind]
    has_name: bool
    name: Optional[str]
    has_default: bool
    default: Optional[T]


@composite
def signatures(draw: DrawFn, default: SearchStrategy[T], *, max_params: Optional[int] = None, max_identifier_length: Optional[int] = None) -> Signature:
    param_count = draw(integers(min_value=0, max_value=None))
    if param_count == 0:
        return Signature()
    params: list[ParamProto[T]] = [ParamProto() for _ in range(param_count)]
    current_count: int = 0
    def fill_kind(kind: _ParameterKind, max_count: int, min_count: int = 0) -> None:
        nonlocal current_count, params
        count = draw(integers(min_value=0, max_value=max_count))
        for i in range(count):
            params[current_count + i].set_kind(kind)
        current_count += count
    fill_kind(Parameter.POSITIONAL_ONLY, param_count - current_count)
    fill_kind(Parameter.POSITIONAL_OR_KEYWORD, param_count - current_count)
    fill_kind(Parameter.VAR_POSITIONAL, 1)
    fill_kind(Parameter.KEYWORD_ONLY, param_count - current_count, max(0, param_count - current_count - 1))
    if current_count != param_count:
        fill_kind(Parameter.VAR_KEYWORD, 1)
    current_count = draw(integers(min_value=0, max_value=param_count))
    for i in range(param_count - current_count):
        params[current_count + i].set_default(draw(default))
    return Signature([param.reify() for param in params])
    

class DummyFunction:
    def __init__(self, sig: Signature) -> None:
        self.__signature__ = sig
    def __call__(self, *args: object, **kwargs: object) -> None:
        self.__signature__.bind(*args, **kwargs)
    __signature__: Signature


@given(signatures(default=none(), max_identifier_length=3), one_of(identifiers(), integers()))
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
    else:
        dummy(*args, **kwargs)
