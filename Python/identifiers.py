from typing import Optional
from hypothesis import assume
from hypothesis.strategies import composite, text, integers, characters, DrawFn
from keyword import iskeyword


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
    return string
