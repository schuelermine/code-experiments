from __future__ import annotations
from abc import ABCMeta, abstractmethod
from inspect import signature
from typing import Callable, Generic, NoReturn, Optional, TypeVar, cast, overload

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
        cls, __name: str, __bases: tuple[type, ...], **kwds: object
    ) -> CallableABCMetaDict:
        return CallableABCMetaDict()

    def __init__(
        self, name: str, bases: tuple[type, ...], namespace: CallableABCMetaDict
    ) -> None:
        self.class_call = namespace.class_call
        super().__init__(name, bases, namespace)

    def __call__(self, *args: object, **kwds: object) -> object:
        class_call = self.class_call
        if class_call is not None and callable(class_call):
            try:
                # Try to catch incorrect args early to hide __class_call__
                sig = signature(class_call)
                sig.bind(*args, **kwds)
            except ValueError:  # Signature does not exist
                pass
            return class_call(*args, **kwds)
        return super().__call__(*args, **kwds)


class CallableABC(metaclass=CallableABCMeta):
    pass


class Maybe(CallableABC, Generic[T]):
    """
    Callable abstract base class for Just and Nothing.
    A Maybe value represents a value that may or may not exist.
    To construct a Just, call Maybe with one argument.
    To construct a Nothing, call Maybe with no arguments.
    Calling Maybe does not construct a direct instance of Maybe.
    Property Maybe.present indicates if a value is present.
    Property Maybe.value is the value if it is present.
    Just and Nothing support pattern matching.
    See also: Just, Nothing
    """

    present: bool
    value: Optional[T]

    __slots__ = ("present", "value")

    @overload
    @classmethod
    def __class_call__(cls, arg: G, /) -> Just[G]:
        ...

    @overload
    @classmethod
    def __class_call__(cls) -> Nothing[NoReturn]:
        ...

    @classmethod
    def __class_call__(cls, *args: G) -> Maybe[G]:
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
        """
        Return the value, assuming it exists.
        Raises MissingValueError if value is missing.
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
        Returns the value, if it exists, and Nothing() otherwise.
        maybe.join() is equivalent to maybe.bind(lambda x: x).
        """
        pass
    
    @abstractmethod
    def get(self: Maybe[G], /, default: G,) -> G:
        """
        Return the value if it is present, otherwise, return default.
        """
        pass

    @staticmethod
    def from_optional(value: Optional[G], /) -> Maybe[G]:
        """
        Create a Maybe value from a value that is potentially None.
        Returns Just(value) if value is not None, else returns Nothing().
        """
        if value is None:
            return Nothing[G]()
        else:
            return Just[G](value)

    @staticmethod
    def with_bool(present: bool, value: G) -> Maybe[G]:
        """
        Create a Maybe value from a value and a boolean.
        Return Nothing() if present is False, else returns Just(value).
        """
        if present:
            return Just[G](value)
        else:
            return Nothing[G]()


class Just(Maybe[T]):
    """
    Subclass of Maybe that indicates a value is present.
    Construct with one value: Just(value)
    Property Just.present is always True.
    Property Just.value is the value.
    Just(value) is supported in pattern matching.
    See also: Maybe, Nothing
    """

    def __init__(self: Just[T], value: T, /) -> None:
        self.present = True
        self.value = value

    def assume_present(self: Just[G]) -> G:
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

    def get(self: Maybe[G], /, default: G) -> G:
        return cast(G, self.value)
    
    def __repr__(self) -> str:
        return f"Just({self.value!r})"

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

    def __init__(self: Nothing[T]) -> None:
        self.present = False
        self.value = None

    def assume_present(self: Nothing[G]) -> G:
        raise MissingValueError()

    def map(self: Nothing[G], f: Callable[[G], U], /) -> Nothing[U]:
        return Nothing[U]()

    def replace(self: Nothing[object], value: U, /) -> Nothing[U]:
        return Nothing[U]()

    def then(self: Nothing[object], maybe: Maybe[U], /) -> Nothing[U]:
        return Nothing[U]()

    def alternatively(self: Nothing[object], maybe: Maybe[G], /) -> Maybe[G]:
        return maybe

    def bind(self: Nothing[G], f: Callable[[G], Maybe[U]], /) -> Nothing[U]:
        return Nothing[U]()

    def join(self: Nothing[Maybe[G]]) -> Nothing[G]:
        return Nothing[G]()

    def get(self: Nothing[object], /, default: G) -> G:
        return default
    
    def __repr__(self) -> str:
        return "Nothing()"

    __match_args__ = ()
