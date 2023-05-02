from functools import wraps

class hybridmethod:
    def __init__(self, function, /):
        self.__func__ = function

    def __get__(self, obj, objtype=None):
        if obj is None and isinstance(objtype, type):
            @wraps(self.__func__)
            def f(*args, **kwargs):
                return self.__func__(objtype, *args, **kwargs)

        else:
            @wraps(self.__func__)
            def f(*args, **kwargs):
                return self.__func__(objtype, obj, *args, **kwargs)

        return f

class Echoer:
    def __init__(self, text):
        self.text = text
    
    @hybridmethod
    def echo(cls, self):
        if not isinstance(self, cls):
            self = cls(self)
        
        return self.text
