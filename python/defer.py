from __future__ import annotations
from collections.abc import Callable
from functools import partial
from typing import final, TypeVar, Generic


T = TypeVar("T")


@final
class DeferMut(Generic[T]):
    obj: T
    ops: list[Callable[[], None]]

    __slots__ = ("obj", "ops")

    def __init__(self, obj: T):
        self.obj = obj
        self.ops = []

    def run(self):
        for op in self.ops:
            op()

        self.ops.clear()

    def __getattribute__(self, name: str) -> Callable[..., None]:
        if name in ["obj", "ops", "run"]:
            return object.__getattribute__(self, name)

        val = getattr(self.obj, name)
        if not callable(val):
            return val

        def g(*args, **kwargs):

            def h():
                val(*args, **kwargs)

            self.ops.append(h)

        return g

    def __repr__(self):
        return f"<DeferMut {self.obj}, queued: {len(self.ops)}>"


@final
class DeferCon(Generic[T]):
    obj: T
    ops: list[Callable[[], T]]

    __slots__ = ("obj", "ops")

    def __init__(self, obj: T, ops: list[Callable[[], T]] | None = None):
        self.obj = obj
        self.ops = [] if ops is None else ops

    def run(self) -> object:
        obj = self.obj
        for op in self.ops:
            obj = op()

        return obj

    def __getattribute__(self, name: str) -> Callable[..., DeferCon]:
        if name in ["obj", "ops", "run"]:
            return object.__getattribute__(self, name)

        val = getattr(self.obj, name)
        if not callable(val):
            return val

        def g(*args, **kwargs):

            def h():
                return val(*args, **kwargs)

            return DeferCon(self.obj, self.ops + [h])

        return g

    def __repr__(self):
        return f"<DeferCon {self.obj}, queued: {len(self.ops)}>"
