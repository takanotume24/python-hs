"""Minimal array shim for python-hs.

python-hs has no typed buffer protocol, so array.array is represented as a
plain list of values. Callers that only construct, index, iterate and take
the length of an array can run unchanged.
"""


def array(typecode, initializer=None):
    if initializer is None:
        return []
    return list(initializer)
