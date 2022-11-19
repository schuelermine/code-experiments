from dataclasses import dataclass
from typing import Any, Mapping


@dataclass
class DummyCall:
    args: tuple[Any, ...]
    kwargs: dict[str, Any]


def _dummy_function(*args: Any, **kwargs: Any) -> DummyCall:
    return DummyCall(args, kwargs)


class _dummy_dict(dict[str, Any]):
    def __setitem__(self, key: str, value: Any) -> None:
        if callable(value):
            super().__setitem__(key, _dummy_function)
        else:
            super().__setitem__(key, value)


class dummy(type):
    @classmethod
    def __prepare__(
        metacls: Any, __name: str, __bases: tuple[type, ...], **kwds: Any
    ) -> Mapping[str, object]:
        return _dummy_dict()
