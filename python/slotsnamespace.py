class SlotsNamespaceMeta(type):
    def __call__(self, name, *args) -> Any:

        def __init__(self, **kwargs) -> None:
            for arg in args:
                missing_key = False
                try:
                    setattr(self, arg, kwargs[arg])
                except KeyError:
                    missing_key = True

                if missing_key:
                    raise TypeError(f"{arg!r} is a required keyword argument for {name}()")

                del kwargs[arg]

            if kwargs:
                raise TypeError(f"{next(iter(kwargs))!r} is an invalid keyword argument for {name}()")

        __init__.__qualname__ = f"{name}.__init__"

        def __repr__(self) -> str:
            return f"{name}(" + ", ".join(f"{arg}={getattr(self, arg)!r}" for arg in args) + ")"

        __repr__.__qualname__ = f"{name}.__repr__"
        namespace = {"__slots__": tuple(args), "__init__": __init__, "__repr__": __repr__}
        return super().__call__(name, (), namespace)

class SlotsNamespace(type, metaclass=SlotsNamespaceMeta):
    def __repr__(self) -> str:
        return f"<slots namespace {self.__name__!r}>"

__all__ = ("SlotsNamespace",)
