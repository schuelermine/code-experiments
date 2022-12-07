from __future__ import annotations
from abc import ABCMeta, abstractmethod
from typing import Callable, Generic, NoReturn, Optional, TypeVar, cast

T = TypeVar("T", covariant=True)
G = TypeVar("G")
U = TypeVar("U")


class CallableABCMetaDict(
    dict[str, object],
):
    class_call: object | None

    def __init__(self, *args: object, **kwargs: object) -> None:
        self.class_call = None
        super().__init__(self, *args, **kwargs)

    def __setitem__(self, key: str, value: object, /) -> None:
        if key == "__class_call__":
            self.class_call = value
        else:
            super().__setitem__(key, value)


class CallableABCMeta(ABCMeta):
    class_call: object | None

    @classmethod
    def __prepare__(
        cls: object, __name: str, __bases: tuple[type, ...], **kwds: object
    ) -> CallableABCMetaDict:
        return CallableABCMetaDict()

    def __init__(
        self, name: str, bases: tuple[type, ...], namespace: CallableABCMetaDict
    ) -> None:
        self.class_call = namespace.class_call
        super().__init__(name, bases, namespace)

    def __call__(self, *args: object, **kwds: object) -> object:
        if self.class_call is not None and callable(self.class_call):
            return self.class_call(*args, **kwds)
        return super().__call__(*args, **kwds)


class CallableABC(metaclass=CallableABCMeta):
    pass


class Example(CallableABC):
    @classmethod
    def __class_call__(cls, a: object) -> tuple[()]:
        return ()


class Maybe(CallableABC, Generic[T]):
    present: bool
    value: Optional[T]

    @classmethod
    def __class_call__(cls, *args: G, **kwargs: NoReturn) -> Maybe[G]:
        if kwargs != {}:
            raise TypeError("Maybe() takes no keyword arguments")
        argc = len(args)
        if argc > 1:
            raise TypeError(
                f"Maybe() takes up to one positional argument, but {argc} were given"
            )
        if argc == 0:
            return Nothing()
        return Just(args[0])

    @abstractmethod
    def assume_present(self) -> T:
        pass

    @abstractmethod
    def map(self: Maybe[G], f: Callable[[G], U], /) -> Maybe[U]:
        pass

    @abstractmethod
    def replace(self: Maybe[object], value: U, /) -> Maybe[U]:
        pass

    @abstractmethod
    def and_then(self: Maybe[object], maybe: Maybe[U], /) -> Maybe[U]:
        pass

    @abstractmethod
    def flatmap(self: Maybe[G], f: Callable[[G], Maybe[U]], /) -> Maybe[U]:
        pass

    @abstractmethod
    def join(self: Maybe[Maybe[G]]) -> Maybe[G]:
        pass

    @staticmethod
    def from_optional(value: Optional[G], /) -> Maybe[G]:
        if value is None:
            return Nothing[G]()
        else:
            return Just[G](value)

    @staticmethod
    def with_bool(present: bool, value: G) -> Maybe[G]:
        if present:
            return Just[G](value)
        else:
            return Nothing[G]()


class Just(Maybe[T]):
    def __init__(self: Just[T], value: T, /) -> None:
        self.present = True
        self.value = value

    def assume_present(self: Just[G]) -> G:
        return cast(G, self.value)

    def map(self: Just[G], f: Callable[[G], U], /) -> Just[U]:
        return Just[U](f(cast(G, self.value)))

    def replace(self: Just[object], value: U, /) -> Just[U]:
        return Just[U](value)

    def and_then(self: Just[object], maybe: Maybe[U]) -> Maybe[U]:
        return maybe

    def flatmap(self: Just[G], f: Callable[[G], Maybe[U]], /) -> Maybe[U]:
        return f(cast(G, self.value))

    def join(self: Just[Maybe[G]]) -> Maybe[G]:
        return cast(Maybe[G], self.value)

    __match_args__ = ("value",)


class MissingValueError(ValueError):
    "Raised to indicate a potentially missing value was missing."
    pass


class Nothing(Maybe[T]):
    def __init__(self: Nothing[T]) -> None:
        self.present = False
        self.value = None

    def assume_present(self: Nothing[G]) -> G:
        raise MissingValueError()

    def map(self: Nothing[G], f: Callable[[G], U], /) -> Nothing[U]:
        return Nothing[U]()

    def replace(self: Nothing[object], value: U, /) -> Nothing[U]:
        return Nothing[U]()

    def and_then(self: Nothing[object], maybe: Maybe[U], /) -> Nothing[U]:
        return Nothing[U]()

    def flatmap(self: Nothing[G], f: Callable[[G], Maybe[U]], /) -> Nothing[U]:
        return Nothing[U]()

    def join(self: Nothing[Maybe[G]]) -> Nothing[G]:
        return Nothing[G]()

    __match_args__ = ()
