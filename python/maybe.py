from __future__ import annotations
from abc import ABCMeta, abstractmethod
from collections.abc import Callable, Iterator
from inspect import signature
from typing import Any, Generic, NoReturn, Optional, TypeVar, cast, overload

__all__ = ("Maybe", "Just", "Nothing", "MissingValueError")

T = TypeVar("T", covariant=True)
G = TypeVar("G")
U = TypeVar("U")
V = TypeVar("V")


class CallableABCMetaDict(
    dict[str, object],
):
    _class_call: object | None

    __slots__ = ("_class_call",)

    def __init__(self, *args: object, **kwargs: object) -> None:
        self._class_call = None
        super().__init__(self, *args, **kwargs)

    def __setitem__(self, key: str, value: object, /) -> None:
        if key == "_class_call":
            self._class_call = value
        super().__setitem__(key, value)


class CallableABCMeta(ABCMeta):
    _class_call: object | None

    @classmethod
    def __prepare__(
        cls, __name: str, __bases: tuple[type, ...], **kwds: object
    ) -> CallableABCMetaDict:
        return CallableABCMetaDict()

    def __init__(
        self, name: str, bases: tuple[type, ...], namespace: CallableABCMetaDict
    ) -> None:
        self._class_call = namespace._class_call
        super().__init__(name, bases, namespace)

    def __call__(self, *args: object, **kwds: object) -> object:
        _class_call = self._class_call
        if _class_call is not None and callable(_class_call):
            try:
                # Try to catch incorrect args early to hide __class_call__
                sig = signature(_class_call)
                sig.bind(*args, **kwds)
            except ValueError:  # Signature does not exist
                pass
            return _class_call(*args, **kwds)
        return super().__call__(*args, **kwds)


class CallableABC(metaclass=CallableABCMeta):
    __slots__ = ()


class Maybe(CallableABC, Generic[T]):
    """
    Callable abstract base class for Just and Nothing.
    A Maybe value represents a value that may or may not exist.
    To construct a Just, call Maybe with one argument.
    To construct a Nothing, call Maybe with no arguments.
    Calling Maybe does not construct a direct instance of Maybe.
    Property Maybe.present indicates if a value is present.
    Property Maybe.value is the value if it is present.
    To override the constructors used for the static methods when inheriting, redefine `_class_call` in your class to match the behavior of Maybe().
    Just and Nothing support pattern matching.
    See also: Just, Nothing
    """

    __slots__ = ("present", "value")

    present: bool
    value: Optional[T]

    @overload
    @classmethod
    def _class_call(cls, arg: G, /) -> Just[G]:
        ...

    @overload
    @classmethod
    def _class_call(cls) -> Nothing[NoReturn]:
        ...

    @classmethod
    def _class_call(cls, *args: G) -> Maybe[G]:
        argc = len(args)
        if argc > 1:
            raise TypeError(
                f"Maybe() takes up to one positional argument, but {argc} were given"
            )
        if argc == 0:
            return Nothing[G]()
        return Just[G](args[0])

    @abstractmethod
    def assume_present(self) -> T:
        """
        Return the value, assuming it exists.
        Raises MissingValueError if value is missing.
        """
        pass

    @abstractmethod
    def get(self: Maybe[G], /, default: G) -> G:
        """
        Return the value if it is present, otherwise, return default.
        """
        pass

    @abstractmethod
    def map(self: Maybe[G], f: Callable[[G], U], /) -> Maybe[U]:
        """
        Apply a function on the value, if it exists.
        Returns a new Maybe value containing the transformed value, if a value was present.
        """
        pass

    @abstractmethod
    def replace(self: Maybe[object], value: U, /) -> Maybe[U]:
        """
        Replace the value with a new value, if it exists.
        Returns a new Maybe value containing the new value, if a value was present.
        maybe.replace(value) is equivalent to maybe.map(lambda _: value).
        """
        pass

    @abstractmethod
    def then(self: Maybe[object], maybe: Maybe[U], /) -> Maybe[U]:
        """
        Replace a Maybe value with another Maybe value, if the first value is present.
        Returns the passed second Maybe value, unless the first value is missing.
        maybe1.then(maybe2) is equivalent to maybe1.bind(lambda _: maybe2).
        """
        pass

    @abstractmethod
    def alternatively(self: Maybe[G], maybe: Maybe[G], /) -> Maybe[G]:
        """
        Replace a Maybe value with another Maybe value, if the first value is not present.
        Returns the passed second Maybe value, unless the first value is present.
        """
        pass

    @abstractmethod
    def bind(self: Maybe[G], f: Callable[[G], Maybe[U]], /) -> Maybe[U]:
        """
        Construct a new Maybe value with the value in the first Maybe value, if it exists.
        Calls the passed function on the value, returning the result, if the value is present.
        maybe.bind(f) is equivalent to maybe.map(f).join().
        """
        pass

    @abstractmethod
    def join(self: Maybe[Maybe[G]]) -> Maybe[G]:
        """
        Flatten a Maybe value potentially containing another Maybe value.
        Returns the value, if it exists, and a missing value otherwise.
        maybe.join() is equivalent to maybe.bind(lambda x: x).
        """
        pass

    @abstractmethod
    def ap(self: Maybe[Callable[[G], U]], maybe: Maybe[G], /) -> Maybe[U]:
        """
        Apply a function from inside a Maybe value onto the value in another Maybe value, if both exist.
        Returns a missing value if any of the Maybe operands is missing.
        maybe1.ap(f, maybe2) is equivalent to maybe1.flatmap(lambda f: maybe2.flatmap(lambda x: f(x)))
        """
        pass

    @classmethod
    def lift2(
        cls, f: Callable[[G, U], V], maybe1: Maybe[G], maybe2: Maybe[U]
    ) -> Maybe[V]:
        """
        Apply a function over two Maybe values, returning a missing value if any inputs were missing.
        Maybe.lift2(f, maybe1, maybe2) is equivalent to maybe1.map(lambda x: lambda y: f(x, y)).ap(maybe2)
        """
        return maybe1.map(lambda x: lambda y: f(x, y)).ap(maybe2)

    @classmethod
    def lift(cls: Any, f: Callable[..., G], *args: Maybe[object]) -> Maybe[G]:
        """
        Apply a function that takes multiple arguments over multiple Maybe values, returning a missing value if any inputs were missing.
        This is a general version of Maybe.lift2.
        """
        values: list[object] = []
        is_missing = False
        for maybe in args:
            if maybe.present:
                values.append(maybe.value)
            else:
                is_missing = True
                break
        if is_missing:
            return cls[G]()
        else:
            return cls[G](f(*values))

    @classmethod
    def from_optional(cls: Any, value: Optional[G], /) -> Maybe[G]:
        """
        Create a Maybe value from a value that is potentially None.
        Returns a present value if value is not None, else returns a missing value.
        """
        if value is None:
            return cls[G]()
        else:
            return cls[G](value)

    @classmethod
    def with_bool(cls: Any, present: bool, value: G) -> Maybe[G]:
        """
        Create a Maybe value from a value and a boolean.
        Returns a missing value if present is False, else returns a present value.
        """
        if present:
            return cls[G](value)
        else:
            return cls[G]()

    def __or__(self: Maybe[G], other: Maybe[G]) -> Maybe[G]:
        return self.alternatively(other)

    def __rshift__(self: Maybe[object], other: Maybe[U]) -> Maybe[U]:
        return self.then(other)


class Just(Maybe[T]):
    """
    Subclass of Maybe that indicates a value is present.
    Construct with one value: Just(value)
    Property Just.present is always True.
    Property Just.value is the value.
    Just(value) is supported in pattern matching.
    See also: Maybe, Nothing
    """

    __slots__ = ("present", "value")

    def __init__(self: Just[T], value: T, /) -> None:
        self.present = True
        self.value = value

    def assume_present(self: Just[G]) -> G:
        return cast(G, self.value)

    def get(self: Maybe[G], /, default: G) -> G:
        return cast(G, self.value)

    def map(self: Just[G], f: Callable[[G], U], /) -> Just[U]:
        return Just[U](f(cast(G, self.value)))

    def replace(self: Just[object], value: U, /) -> Just[U]:
        return Just[U](value)

    def then(self: Just[object], maybe: Maybe[U]) -> Maybe[U]:
        return maybe

    def alternatively(self: Just[G], maybe: Maybe[object], /) -> Maybe[G]:
        return self

    def bind(self: Just[G], f: Callable[[G], Maybe[U]], /) -> Maybe[U]:
        return f(cast(G, self.value))

    def join(self: Just[Maybe[G]]) -> Maybe[G]:
        return cast(Maybe[G], self.value)

    def ap(self: Just[Callable[[G], U]], maybe: Maybe[G]) -> Maybe[U]:
        return maybe.map(cast(Callable[[G], U], self.value))

    def __repr__(self: Just[object]) -> str:
        return f"Just({self.value!r})"

    def __eq__(self: Just[object], other: object) -> bool:
        if isinstance(other, Just):
            return self.value == other.value
        elif isinstance(other, Nothing):
            return False
        else:
            return NotImplemented

    def __hash__(self: Just[object]) -> int:
        return hash((self.value, self.present))

    def __bool__(self: Just[object]) -> bool:
        return True

    def __len__(self: Just[object]) -> int:
        return 1

    def __iter__(self: Just[G]) -> Iterator[G]:
        return iter((cast(G, self.value),))

    def __contains__(self, item: object) -> bool:
        return self.value == item

    __match_args__ = ("value",)


class MissingValueError(ValueError):
    "Raised to indicate a potentially missing value was missing."
    pass


class Nothing(Maybe[T]):
    """
    Subclass of Maybe that indicates a value is missing.
    Construct with no value: Nothing()
    Property Nothing.present is always False.
    Property Nothing.value is always None.
    Nothing() is supported in pattern matching.
    See also: Maybe, Just
    """

    __slots__ = ("present", "value")

    def __init__(self: Nothing[T]) -> None:
        self.present = False
        self.value = None

    def assume_present(self: Nothing[G]) -> G:
        raise MissingValueError()

    def get(self: Nothing[object], /, default: G) -> G:
        return default

    def map(self: Nothing[G], f: Callable[[G], U], /) -> Nothing[U]:
        return Nothing[U]()

    def replace(self: Nothing[object], value: U, /) -> Nothing[U]:
        return Nothing[U]()

    def then(self: Nothing[object], maybe: Maybe[U], /) -> Nothing[U]:
        return Nothing[U]()

    def alternatively(self: Nothing[G], maybe: Maybe[G], /) -> Maybe[G]:
        return maybe

    def bind(self: Nothing[G], f: Callable[[G], Maybe[U]], /) -> Nothing[U]:
        return Nothing[U]()

    def join(self: Nothing[Maybe[G]]) -> Nothing[G]:
        return Nothing[G]()

    def ap(self: Nothing[Callable[[G], U]], maybe: Maybe[G]) -> Nothing[U]:
        return Nothing[U]()

    def __repr__(self: Nothing[object]) -> str:
        return "Nothing()"

    def __eq__(self: Nothing[object], other: object) -> bool:
        if isinstance(other, Nothing):
            return True
        elif isinstance(other, Just):
            return False
        else:
            return NotImplemented

    def __hash__(self: Nothing[object]) -> int:
        return hash(())

    def __bool__(self: Nothing[object]) -> bool:
        return False

    def __len__(self: Nothing[object]) -> int:
        return 0

    def __iter__(self: Nothing[G]) -> Iterator[G]:
        return iter(())

    def __contains__(self, item: object) -> bool:
        return False

    __match_args__ = ()
