from dataclasses import dataclass
from typing import Mapping


@dataclass
class DummyCall:
    args: tuple[object, ...]
    kwargs: dict[str, object]


def _dummy_function(*args: object, **kwargs: object) -> DummyCall:
    return DummyCall(args, kwargs)


class _dummy_dict(dict[str, object]):
    def __setitem__(self, key: str, value: object) -> None:
        if callable(value):
            super().__setitem__(key, _dummy_function)
        else:
            super().__setitem__(key, value)


class dummy(type):
    @classmethod
    def __prepare__(
        cls: object, __name: str, __bases: tuple[type, ...], **kwds: object
    ) -> Mapping[str, object]:
        return _dummy_dict()
