"""Minimal sys shim for python-hs.

Only widely used marker values and helpers are provided. The goal is to let
CPython tests import sys and read stable information, not to emulate the full
runtime introspection API.
"""

platform = "linux"
byteorder = "little"
maxsize = 9223372036854775807
maxunicode = 1114111
version = "3.12.16 (python-hs)"
version_info = (3, 12, 16, "final", 0)
hexversion = 50791152
copyright = "python-hs"
executable = "python-hs"
prefix = "/usr/local"
base_prefix = "/usr/local"
dont_write_bytecode = True

argv = ["python-hs"]
path = []


class _Flags:
    optimize = 0
    debug = 0
    verbose = 0
    dev_mode = False


flags = _Flags()


def getrecursionlimit():
    return 1000


def setrecursionlimit(limit):
    return None


def getrefcount(value):
    return 1


def getsizeof(value):
    return 0


def intern(value):
    return value


def exit(code=0):
    raise SystemExit(code)
