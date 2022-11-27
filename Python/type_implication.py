from typing import Optional

def foo(num: int, val: Optional[str] = None) -> list[str]:
    assert num == 0 or val is not None
    if num == 0:
        return []
    else:
        assert val is not None
        return [val] * num

