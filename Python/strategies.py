from typing import Optional, Type, Any
from hypothesis import assume
from hypothesis.strategies import (
    composite,
    text,
    integers,
    characters,
    DrawFn,
    SearchStrategy,
    one_of,
    just,
    booleans,
    recursive,
    dictionaries,
)
from keyword import iskeyword
from unicodedata import normalize
from inspect import Parameter, _ParameterKind


def nested_dictionaries(
    keys: SearchStrategy[Any], max_leaves: int = 100
) -> SearchStrategy[Any]:
    return recursive(just({}), lambda x: dictionaries(keys, x), max_leaves=max_leaves)


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
    length = draw(integers(min_value=0, max_value=max_length))
    string = ""
    if length != 0:
        string += draw(
            characters(
                whitelist_categories=id_start_categories,
                whitelist_characters=id_start_extras,
            )
        )
        for i in range(length - 1):
            string += draw(
                characters(
                    whitelist_categories=id_continue_categories,
                    whitelist_characters=id_continue_extras,
                )
            )
    assume(not iskeyword(string))
    return normalize("NFKC", string)


@composite
def parameters(
    draw: DrawFn,
    default: SearchStrategy[Any],
    kind: Optional[_ParameterKind] = None,
    max_length: Optional[int] = None,
) -> Parameter:
    if kind is None:
        kind = draw(parameter_kinds)
    assert kind is not None
    if draw(booleans()):
        return Parameter(draw(identifiers()), kind, default=draw(default))
    else:
        return Parameter(draw(identifiers()), kind)


parameter_kinds: SearchStrategy[_ParameterKind] = one_of(
    just(Parameter.POSITIONAL_OR_KEYWORD),
    just(Parameter.POSITIONAL_ONLY),
    just(Parameter.KEYWORD_ONLY),
    just(Parameter.VAR_KEYWORD),
    just(Parameter.VAR_POSITIONAL),
)
