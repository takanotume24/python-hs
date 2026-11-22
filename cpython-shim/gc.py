"""Minimal gc shim for python-hs.

python-hs has no garbage collector to expose, so the shim reports stable
no-op values. Tests that only need to call gc.collect() or check whether an
object is tracked can run unchanged.
"""

garbage = []
callbacks = []

DEBUG_START = 1
DEBUG_STOP = 2
DEBUG_SAVEALL = 4
DEBUG_LEAK = 8


def collect(generation=2):
    return 0


def enable():
    return None


def disable():
    return None


def isenabled():
    return True


def is_tracked(obj):
    return True


def set_threshold(threshold0, threshold1=10, threshold2=10):
    return None


def get_threshold():
    return (700, 10, 10)


def get_count():
    return (0, 0, 0)


def get_objects(generation=None):
    return []


def get_referrers(obj=None):
    return []


def get_referents(obj=None):
    return []


def get_stats(generation=None):
    return []


def freeze():
    return None


def unfreeze():
    return None
