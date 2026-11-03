# python-hs vs CPython：未実装機能一覧（P87 時点）

このドキュメントは、現在の `python-hs` と CPython 3.x を比較した際の主な未実装・未対応機能をまとめたものです。

## 1. 構文・字句解析

| 機能 | 状態 | 備考 |
|------|------|------|
| f-string (`f"..."`) | ❌ | 文字列補間・式埋め込み未対応 |
| raw string (`r"..."`) | ❌ | エスケープ無効化未対応 |
| triple-quoted string | ❌ | 複数行文字列未対応 |
| bytes リテラル (`b"..."`) | ❌ | bytes 型自体未対応 |
| 複素数リテラル (`1j`) | ❌ | complex 型未対応 |
| 巨大数の `_` 区切り (`1_000`) | ❌ | |
| 指数表記 (`1e3`) | ✅ | |
| `**` 累乗演算子 | ✅ | `*` `/` `%` `//` は対応。`**=` は未対応 |
| ビット演算 (`& \| ^ ~ << >>`) | ❌ | |
| Walrus operator (`:=`) | ✅ | |
| `match/case` | 一部 | 値/OR/シーケンス/マッピング/ガード/`as`/`**rest`対応。class パターン等は未対応の可能性あり |

## 2. 組み込み型・データ構造

| 機能 | 状態 | 備考 |
|------|------|------|
| `str` 型（メソッド付き） | 一部 | リテラル・連結・truthiness はあるが、`str.split`/`join` 等のメソッド未対応 |
| `bytes`/`bytearray` | ❌ | |
| `set`/`frozenset` | ❌ | |
| `complex` | ❌ | |
| `bool` 組み込み関数 | ✅ | |
| `int()`/`float()`/`str()` 等の型変換 | 一部 | 組み込みとして `bool`/`len`/`range` 等はあるが、汎用コンストラクタ未対応 |
| 辞書内包表記 | ❌ | リスト内包表記は対応 |
| 集合内包表記 | ❌ | |
| ジェネレータ式 | ❌ | `yield`/`yield from` は対応 |
| tuple / list / dict リテラル | ✅ | |
| slice 代入 (`l[1:3] = [4,5]`) | ❌ | slice 読み取りは対応 |
| `del` on リスト/辞書要素 | ❌ | 変数・属性の `del` は対応 |

## 3. 関数・スコープ

| 機能 | 状態 | 備考 |
|------|------|------|
| `nonlocal` | ❌ | `global` は対応 |
| keyword-only 引数 (`def f(*, x):`) | ❌ | `*args`/`**kwargs` は対応 |
| positional-only 引数 (`def f(x, /):`) | ❌ | PEP 570 |
| 関数アノテーションの実行時評価 | 一部 | 構文は解析されるが、評価器で完全には活用されていない |
| `lambda` | ✅ | |
| デコレータ | ✅ | 関数・クラス両方 |
| 可変長引数のデフォルト引数評価順 | ✅ | |
| `return` in `finally` の微妙な挙動 | 一部 | 基本的な置き換えは動作 |

## 4. クラス・OOP

| 機能 | 状態 | 備考 |
|------|------|------|
| 多重継承 | ❌ | 単一継承のみ対応 |
| `super()` | ❌ | |
| `property` | ❌ | descriptor protocol 未対応 |
| `__get__` / `__set__` / `__delete__` | ❌ | descriptor protocol 未対応 |
| `__new__` | ❌ | `__init__` は対応 |
| `__str__` / `__repr__` / `__eq__` | 一部 | dataclass 自動生成のみ。一般クラスの特殊メソッド呼び出しは未対応 |
| `__lt__` 等のリッチ比較 | 一部 | dataclass `order` 対応 |
| `__iter__` / `__next__` / `__contains__` | ❌ | `for` 反復は組み込み型のみ |
| `__len__` / `__bool__` 等の特殊メソッド | ❌ | 組み込み関数 `len`/`bool` は特殊メソッドを呼ばずハードコード |
| メタクラス | ❌ | |
| `__slots__` | ❌ | |

## 5. 例外処理

| 機能 | 状態 | 備考 |
|------|------|------|
| `raise ... from ...` | ❌ | 例外連鎖・原因保持未対応 |
| `ExceptionGroup` / `BaseExceptionGroup` | ❌ | Python 3.11 |
| traceback オブジェクト | ❌ | `__traceback__` 等未対応 |
| `sys.exc_info()` | ❌ | |
| `assert message` | 一部 | メッセージ付き assert は未対応の可能性あり |
| `StopIteration` / `GeneratorExit` | ❌ | |

## 6. モジュール・インポート

| 機能 | 状態 | 備考 |
|------|------|------|
| `__future__` import | ❌ | |
| namespace package | ❌ | |
| `__name__ == "__main__"` | ❌ | `__name__` 未実装 |
| `__all__` | 一部 | `from pkg import *` で利用されるが、網羅性不明 |
| `importlib` / `reload` | ❌ | |
| 相対 import | 一部 | package 配下では利用可能 |
| `from .pkg import x` 等の相対パターン | 一部 | 基本的な相対 import は対応 |
| 標準ライブラリの大半 | ❌ | `math`, `json`, `pathlib`, `os`, `dataclasses` の一部のみ |

## 7. 標準ライブラリ・組み込み関数

| 機能 | 状態 |
|------|------|
| ファイル I/O (`open`, `read`, `write`) | ❌ |
| `print()` 関数 | ❌（`print` 文として実装） |
| `repr()`, `str()`, `format()` | ❌ |
| `type()`, `isinstance()`, `issubclass()` | ❌ |
| `hasattr()`, `getattr()`, `setattr()`, `delattr()` | ❌ |
| `enumerate()`, `zip()`, `map()`, `filter()` | ❌ |
| `any()`, `all()`, `sum()`, `min()`, `max()` | ❌ |
| `abs()`, `round()`, `pow()`, `divmod()` | ❌ |
| `chr()`, `ord()`, `hex()`, `oct()`, `bin()` | ❌ |
| `input()` | ❌ |
| `iter()`, `next()` | ❌ |
| `sorted()`, `reversed()` | ❌ |
| `callable()` | ❌ |
| `vars()`, `dir()`, `locals()`, `globals()` | ❌ |
| `exec()`, `eval()` | ❌ |

## 8. 非同期・並行処理

| 機能 | 状態 |
|------|------|
| `async` / `await` | ❌ |
| `async for` / `async with` | ❌ |
| `asyncio` | ❌ |
| `coroutine` / `Task` | ❌ |

## 9. イテレータ・ジェネレータ

| 機能 | 状態 | 備考 |
|------|------|------|
| `yield` / `yield from` | ✅ | |
| ジェネレータ式 | ❌ | |
| イテレータプロトコル (`__iter__`/`__next__`) | ❌ | 組み込み型の反復はハードコード |
| `next()` 組み込み関数 | ❌ | |

## 10. 制御構文

| 機能 | 状態 | 備考 |
|------|------|------|
| `if/elif/else` | ✅ | |
| `while` / `for` | ✅ | |
| `while ... else` / `for ... else` | ❌ | |
| `break` / `continue` | ✅ | |
| `with` | ✅ | Context Manager対応 |
| `try/except/else/finally` | ✅ | `except*` 除く |
| `pass` | ✅ | |

## 11. その他の CPython 機能

| 機能 | 状態 |
|------|------|
| f-string | ❌ |
| 型ヒントの検証 (`typing`) | ❌ |
| `dataclasses` の高度な機能（`KW_ONLY`, `InitVar` 等） | ❌ / 一部 |
| `enum` | ❌ |
| `contextlib` | ❌ |
| `functools` | ❌ |
| `itertools` | ❌ |
| `collections` | ❌ |
| `re`（正規表現） | ❌ |
| `datetime` | ❌ |
| `http` / `urllib` | ❌ |

## 12. 互換性の限界

- **属性解決・メソッド解決**: descriptor protocol / MRO / `super()` が未対応なため、CPython と完全には一致しない。
- **特殊メソッド**: `__init__` 以外の `__xxx__` メソッドは、dataclass 自動生成の一部と `__enter__`/`__exit__` を除き基本的に呼ばれない。
- **エラー型・メッセージ**: 独自の簡易エラー体系。`raise ... from` や traceback は未対応。
- **標準ライブラリ**: 最小限の `math`/`json`/`pathlib`/`os`/`dataclasses` のみ。
- **I/O**: 標準出力 `print` 文のみ。ファイル入出力なし。

## 13. 直近で優先度が高そうな未実装機能

1. **`nonlocal`** — ネスト関数で外側のローカル変数を変更するための基本機能
2. **`f-string`** — 文字列操作の頻出パターン
3. **`**` 累乗演算子・ビット演算** — 基本的な演算子の欠落
4. **`print()` 関数化** — Python 3 では文ではなく関数
5. **`open` / ファイル I/O** — 実用性を大きく広げる
6. **`isinstance` / `type` / `hasattr` 等の組み込み関数** — 反射型プログラミングの基盤
7. **`super()` / 多重継承** — OOP の完成度向上
8. **`raise ... from` / traceback** — 例外処理の完成度向上
