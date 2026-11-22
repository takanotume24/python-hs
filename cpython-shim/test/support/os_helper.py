"""Minimal os_helper shim for python-hs.

The real CPython test.support.os_helper covers filesystem helpers that the
interpreter does not implement yet. Only a placeholder module is needed so
that importing it succeeds.
"""


def unlink(path):
    return None


def rmtree(path):
    return None
