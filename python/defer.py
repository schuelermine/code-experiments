class DeferMut:
    __slots__ = ("obj", "ops")

    def __init__(self, obj, ops=None):
        self.obj = obj
        self.ops = [] if ops is None else ops

    def run(self):
        for op in self.ops:
            op()

        self.ops.clear()

    def __getattribute__(self, name):
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
        return f"<DeferMut {self.obj!r}, queued: {len(self.ops)}>"


class DeferCon:
    __slots__ = ("obj", "ops")

    def __init__(self, obj, ops=None):
        self.obj = obj
        self.ops = [] if ops is None else ops

    def run(self):
        obj = self.obj
        for op in self.ops:
            obj = op()

        return obj

    def __getattribute__(self, name: str):
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
        return f"<DeferCon {self.obj!r}, queued: {len(self.ops)}>"
