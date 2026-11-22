"""Minimal keyword module shim for python-hs."""

kwlist = ['False', 'None', 'True', 'and', 'as', 'assert', 'async', 'await', 'break', 'class', 'continue', 'def', 'del', 'elif', 'else', 'except', 'finally', 'for', 'from', 'global', 'if', 'import', 'in', 'is', 'lambda', 'nonlocal', 'not', 'or', 'pass', 'raise', 'return', 'try', 'while', 'with', 'yield']

softkwlist = ['_', 'case', 'match', 'type']


def iskeyword(word):
    return word in kwlist


def issoftkeyword(word):
    return word in softkwlist
