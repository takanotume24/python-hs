"""Minimal itertools shim for python-hs.

python-hs evaluates generator functions eagerly, so the helpers here return
finite lists instead of lazy iterators. Infinite helpers are capped at a
deterministic bound so that loops which break early keep working.
"""

_INFINITE_LIMIT = 1000


def chain(*iterables):
    result = []
    for iterable in iterables:
        for item in iterable:
            result = result + [item]
    return result


def repeat(object, times=None):
    if times is None:
        times = _INFINITE_LIMIT
    result = []
    i = 0
    while i < times:
        result = result + [object]
        i = i + 1
    return result


def count(start=0, step=1):
    result = []
    value = start
    i = 0
    while i < _INFINITE_LIMIT:
        result = result + [value]
        value = value + step
        i = i + 1
    return result


def cycle(iterable):
    items = list(iterable)
    result = []
    if len(items) == 0:
        return result
    i = 0
    while i < _INFINITE_LIMIT:
        result = result + [items[i % len(items)]]
        i = i + 1
    return result


def islice(iterable, *args):
    items = list(iterable)
    if len(args) == 1:
        start = 0
        stop = args[0]
        step = 1
    elif len(args) == 2:
        start = args[0]
        stop = args[1]
        step = 1
    else:
        start = args[0]
        stop = args[1]
        step = args[2]
    result = []
    index = start
    while index < stop and index < len(items):
        result = result + [items[index]]
        index = index + step
    return result


def product(*args, **kwargs):
    repeat_count = kwargs.get("repeat", 1)
    pools = []
    for pool in args:
        items = list(pool)
        i = 0
        while i < repeat_count:
            pools = pools + [items]
            i = i + 1
    result = [[]]
    for pool in pools:
        next_result = []
        for prefix in result:
            for item in pool:
                next_result = next_result + [prefix + [item]]
        result = next_result
    tuples = []
    for item in result:
        tuples = tuples + [tuple(item)]
    return tuples


def combinations(iterable, r):
    items = list(iterable)
    result = []
    indices = []
    i = 0
    while i < r:
        indices = indices + [i]
        i = i + 1
    if r > len(items):
        return result
    while True:
        chosen = []
        for index in indices:
            chosen = chosen + [items[index]]
        result = result + [tuple(chosen)]
        i = r - 1
        while i >= 0 and indices[i] == i + len(items) - r:
            i = i - 1
        if i < 0:
            return result
        indices[i] = indices[i] + 1
        j = i + 1
        while j < r:
            indices[j] = indices[j - 1] + 1
            j = j + 1


def permutations(iterable, r=None):
    items = list(iterable)
    if r is None:
        r = len(items)
    result = []
    used = []
    i = 0
    while i < len(items):
        used = used + [False]
        i = i + 1

    def build(current):
        if len(current) == r:
            result.append(tuple(current))
            return None
        index = 0
        while index < len(items):
            if used[index] == False:
                used[index] = True
                build(current + [items[index]])
                used[index] = False
            index = index + 1
        return None

    build([])
    return result


def zip_longest(*args, **kwargs):
    fillvalue = kwargs.get("fillvalue", None)
    lists = []
    width = 0
    for arg in args:
        items = list(arg)
        lists = lists + [items]
        if len(items) > width:
            width = len(items)
    result = []
    index = 0
    while index < width:
        row = []
        for items in lists:
            if index < len(items):
                row = row + [items[index]]
            else:
                row = row + [fillvalue]
        result = result + [tuple(row)]
        index = index + 1
    return result


def tee(iterable, n=2):
    items = list(iterable)
    copies = []
    i = 0
    while i < n:
        copies = copies + [list(items)]
        i = i + 1
    return tuple(copies)


def accumulate(iterable, initial=None):
    items = list(iterable)
    result = []
    total = initial
    for item in items:
        if total is None:
            total = item
        else:
            total = total + item
        result = result + [total]
    return result


def takewhile(predicate, iterable):
    result = []
    for item in iterable:
        if predicate(item):
            result = result + [item]
        else:
            return result
    return result


def dropwhile(predicate, iterable):
    items = list(iterable)
    index = 0
    while index < len(items) and predicate(items[index]):
        index = index + 1
    return items[index:]


def filterfalse(predicate, iterable):
    result = []
    for item in iterable:
        if not predicate(item):
            result = result + [item]
    return result


def starmap(function, iterable):
    result = []
    for item in iterable:
        result = result + [function(*item)]
    return result
