from hypothesis.strategies import recursive, dictionaries, just, SearchStrategy

def nested_dictionaries(
    keys: SearchStrategy[object], max_leaves: int = 100
) -> SearchStrategy[object]:
    return recursive(just({}), lambda x: dictionaries(keys, x), max_leaves=max_leaves)
