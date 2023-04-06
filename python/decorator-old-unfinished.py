from inspect import signature, Parameter

def decorator(fun):
    if not callable(func):
        raise TypeError("Parameter to decorator() must be callable")
    
    fun_params = signature(fun).parameters
    
    def dec(*args, **kwargs):
        if len(args) == 1:
