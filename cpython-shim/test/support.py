"""Minimal test.support shim for python-hs.

Only the helpers needed by allowlisted CPython tests are defined here. The
real CPython test.support module depends on modules that python-hs does not
implement yet, so this file shadows it when running through run-cpython-test.
"""


def import_module(name):
    return name


def requires(resource, msg=None):
    return None


def requires_resource(resource):
    return requires


def requires_freebsd_version(*args):
    return requires


def requires_linux_version(*args):
    return requires


def requires_mac_version(*args):
    return requires


def requires_IEEE_754(test):
    return test


def skip(reason):
    return None


def skipIf(condition, reason):
    return None


def skipUnless(condition, reason):
    return None


def cpython_only(test):
    return test


def check_syntax_error(testcase, statement, errtext=None):
    try:
        compile(statement, "<test>", "exec")
    except SyntaxError:
        return None
    raise AssertionError("expected SyntaxError for: " + statement)


class _ALWAYS_EQ:
    """Object that is equal to anything."""

    def __eq__(self, other):
        return True

    def __ne__(self, other):
        return False


class _NEVER_EQ:
    """Object that is not equal to anything."""

    def __eq__(self, other):
        return False

    def __ne__(self, other):
        return True


ALWAYS_EQ = _ALWAYS_EQ()
NEVER_EQ = _NEVER_EQ()
