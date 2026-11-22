"""Minimal test.support shim for python-hs.

Only the helpers needed by allowlisted CPython tests are defined here. The
real CPython test.support module depends on modules that python-hs does not
implement yet, so this file shadows it when running through run-cpython-test.
"""


def requires(resource, msg=None):
    return None


def requires_resource(resource):
    return requires


def skip(reason):
    return None


def skipIf(condition, reason):
    return None


def skipUnless(condition, reason):
    return None


def check_syntax_error(testcase, statement, errtext=None):
    return None


def import_module(name):
    return name
