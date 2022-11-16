from inspect import signature


def decorator(x, /):
    def _decorator(dec, key):
        if isinstance(key, int):
            def new_dec_outer(*args, **kwargs):
                args_ = list(args)
                def new_dec_inner(fun):
                    args_.insert(key, fun)
                    return dec(*args_, **kwargs)
                return new_dec_inner
        elif isinstance(key, str):
            def new_dec_outer(*args, **kwargs):
                def new_dec_inner(fun):
                    kwargs[key] = fun
                    return dec(*args, **kwargs)
                return new_dec_inner
        else:
            raise TypeError(
                "Argument to decorator() must be an instance of int or str")

        return new_dec_outer

    if callable(x):
        if len(signature(x).parameters) == 0:
            return x
        else:
            return _decorator(x, 0)
    else:
        return lambda f: _decorator(f, x)
