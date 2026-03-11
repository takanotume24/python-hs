# python-hs

Haskellで実装した、Python 3 インタプリタです。  
TDD（`hspec`）で機能を拡張し、**可能な限り CPython 互換**を目標に `Lexer -> Parser -> Evaluator -> Runner/CLI` を継続的に改善しています。

このプログラムは、GitHub Copilotによって生成されたコード、及びtakanotume24が手動で修正したコードを含みます。

## 現在の実装状況（2026-03-02）

### 実行基盤
- [x] ソース文字列実行（`runSource`）
- [x] ファイル実行（`python-hs <file>`）
- [x] REPL（状態保持・複数行ブロック対応）

### 字句解析（Lexer）
- [x] 整数・識別子・改行
- [x] 浮動小数リテラル（`1.23`, `1.`, `.5`, 指数表記 `1e3`, `1.2e-3`）
- [x] 文字列リテラル（`"..."`）
- [x] 真偽/Noneリテラル（`True`, `False`, `None`）
- [x] 演算子（`+ - * / % //`, 比較, `and/or/not`）
- [x] 複合代入（`+= -= *= /= %= //=`）
- [x] 制御構文キーワード（`if/elif/else`, `while`, `for/in`, `break`, `continue`）
- [x] 関数関連（`def`, `return`, `global`, `pass`）
- [x] `import` キーワード（`import x`, `import x as y`, `from x import y`, `from x import y as z`）
- [x] 例外構文キーワード（`try`, `except`, `finally`, `raise`）
- [x] パターンマッチ構文キーワード（`match`, `case`）
- [x] `INDENT`/`DEDENT` トークン生成（複文ブロック）

### 構文解析（Parser）
- [x] 文: `print`, 代入, 複合代入, `return`, `global`, `pass`, `import`
- [x] 文: `try/except`（複数 `except` 節対応）、`try/except/finally`、`raise`
- [x] 文: `match/case`（値/OR/シーケンス/マッピング/ガード対応）
- [x] 文: `class`（単一継承ヘッダ + クラスボディ対応）
- [x] 文: class フィールド型注釈（`name: type` / `name: type = expr`）対応
- [x] 制御構文: `if/elif/else`, `while`, `for`
- [x] 関数定義: `def name(args): ...`
- [x] 式: 四則演算（`+ - * / % //`）, 比較, `not`, 関数呼び出し
- [x] 式: tuple（literal, index/slice）
- [x] 組み込みのメソッド呼び出し構文（例: `x.append(3)` を関数呼び出しへデシュガー）
- [x] リテラル: int/float/string/list/dict/None
- [x] インラインsuiteとインデントsuiteの両対応

### 評価器（Evaluator）
- [x] 変数束縛と式評価
- [x] 文字列連結（`"a" + "b"`）
- [x] 数値演算の暗黙昇格（int/float 混在）
- [x] `/` 実数除算 / `//` 床除算 / `%` 数値同士対応
- [x] 整数は任意精度（`Integer`）
- [x] 条件分岐・ループ実行
- [x] `break` / `continue` の制御伝播
- [x] 関数呼び出し（再帰を含む）
- [x] 関数スコープ（引数優先・グローバル参照可）
- [x] `global` は関数内代入をグローバルへ反映、`pass` は no-op として評価
- [x] truthiness（`None`、空文字、空リスト、空辞書）
- [x] truthiness（tuple を含む）
- [x] 反復回数ガード（2000超で `Value error`）
- [x] 位置情報つきエラー報告（Name/Type/Value/Argument count など）

### 組み込み関数（実装済み）
- [x] `len(x)`（string/list）
- [x] `len(x)`（string/list/tuple）
- [x] `bool(x)`（int/float/None/string/list/dictのtruthiness）
- [x] `range(stop)` / `range(start, stop)` / `range(start, stop, step)`
- [x] `append(list, value)`
- [x] `remove(list, value)`
- [x] `sort(list)`（数値要素: int/float、混在可）
- [x] `reverse(list)`
- [x] `insert(list, index, value)`
- [x] `pop(list)` / `pop(dict, key)` / `pop(dict, key, default)`
- [x] `clear(x)`（list/dict）
- [x] `keys(dict)` / `values(dict)` / `items(dict)`（挿入順維持）
- [x] `get(dict, key)` / `get(dict, key, default)`
- [x] `update(dict, key, value)` / `update(dict, otherDict)`
- [x] `setdefault(dict, key)` / `setdefault(dict, key, default)`
- [x] VM math 導線: `import math` + `math.sqrt/sin/cos/tan/log/exp/pi/e`
- [x] VM dataclass 導線: `from dataclasses import dataclass, field` + `@dataclass(order=..., frozen=...)`
- [x] VM tuple 導線: tuple literal / 比較 / index・slice / unpack / match sequence / for反復

補足:
- `math` は現時点で VM 実行経路を対象に互換性向上を継続中です。
- `import` は現時点で VM 実行経路を対象に、`math` / `dataclasses` / ローカル `module.py` / `package/__init__.py` / package submodule（`pkg.sub`, `from pkg import sub` など）を受理します。
- `from . import x` などの relative import は、package 配下モジュール内では利用可能です（エントリスクリプト直下では parent package がないためエラー）。
- `from pkg import *` はローカル package/module に対して利用可能です（先頭 `_` の名前は除外）。
- `import os` / `import json` / `import pathlib` は VM で利用可能です（現状: `os.getcwd`, `json.dumps/loads`, `pathlib.Path`）。
- `math.pi` / `math.e` は現仕様では関数形式（`math.pi()` / `math.e()`）で利用します。
- 例外処理（`try/except/finally`, `raise`）は現時点で VM 実行経路を中心に互換性向上を継続中です。
- `match/case` は現時点で VM 実行経路を中心に互換性向上を継続中です。
- `class`（`__init__` / メソッド呼び出し / 単一継承）は現時点で VM 実行経路を中心に互換性向上を継続中です。
- `dataclass`（`__init__` / `__repr__` / `__eq__` / `order` / `frozen` / `field(default_factory=list)`）は現時点で VM 実行経路を中心に互換性向上を継続中です。

## 互換性ギャップ（継続改善中）
- [ ] Python完全互換（継続して差分を縮小）
- [ ] Pythonの属性解決/メソッド解決の完全互換（descriptor/protocol を含む）
- [ ] `match/case` / import / stdlib の CPython 細部互換の拡張

## 開発環境（Nix Flakes）

Nix が使える環境では、以下で開発シェルに入れます。

```bash
nix develop
```

初回は依存のインデックス更新後にテストを実行してください。

```bash
cabal update
cabal test
cabal run check-structure
```

Flake check でも同等の検証（テスト + 構造チェック + runner case coverage）を実行できます。

```bash
nix flake check path:.
```

テストや構造チェックのログを表示したい場合は `-L` を付けてください。

```bash
nix flake check -L path:.
```

## 使い方

### 1. テスト
```bash
cabal test
```

### 2. 構造チェック
```bash
cabal run check-structure
```

### 3. ファイル実行
```bash
cabal run python-hs -- examples/sample.pyhs
```

### 4. REPL起動
```bash
cabal run python-hs
```

### 5. VMエンジンで起動
`cabal run` から実行ファイルへ引数を渡すときは `--` 区切りが必要です。

```bash
# VMでREPL起動
cabal run python-hs -- --engine vm

# VMでファイル実行
cabal run python-hs -- --engine vm examples/sample.pyhs
```

環境変数でも同じ指定ができます。

```bash
PYTHON_HS_RUNNER_ENGINE=vm cabal run python-hs
PYTHON_HS_RUNNER_ENGINE=vm cabal run python-hs -- examples/sample.pyhs
```

## サンプル

```python
x = 1
if x:
  print 10
else:
  print 20

while x < 3:
  x += 1

def id(v):
  return v

print id(x)
```

期待出力:

```text
10
3
```

## 品質ゲート
- `cabal test` が成功
- `cabal run check-structure` が成功
- コンパイラ警告 0
