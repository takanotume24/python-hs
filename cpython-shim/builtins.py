"""Minimal builtins shim for python-hs.

CPython tests sometimes import the builtins module to inspect its dict or to
store a marker. python-hs resolves builtin functions and exception classes as
language builtins, so this shim only provides the module-level names that are
not builtins themselves.
"""

__dict__ = {}
here = None
