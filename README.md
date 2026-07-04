# python-hs

Haskellで実装した、Python 3 インタプリタです。  
TDD（`hspec`）で機能を拡張し、**可能な限り CPython 互換**を目標に `Lexer -> Parser -> Evaluator -> VM/Runner/CLI` を継続的に改善しています。

**基本方針: 可読性最優先。** すべての設計判断・命名・構造化は、コードが人間にとって読みやすく理解しやすいかを第一の基準とします。

このプログラムは、GitHub Copilotによって生成されたコード、及びtakanotume24が手動で修正したコードを含みます。

## ライセンス

MIT License — 詳細は [LICENSE](LICENSE) を参照してください。

## 現在の実装状況（2026-07-05）

### 実行基盤
- [x] ソース文字列実行（`runSource` — AST評価器経路）
- [x] ソース文字列実行（`runSourceVm` — VMコンパイル/実行経路）
- [x] ファイル実行（`python-hs <file>`、AST/VMエンジン切り替え可能）
- [x] REPL（状態保持・複数行ブロック対応・AST/VM両対応）
- [x] エンジンスイッチ（`--engine ast|vm` または `PYTHON_HS_RUNNER_ENGINE`）

### 字句解析（Lexer）
- [x] 整数・識別子・改行
- [x] 浮動小数リテラル（`1.23`, `1.`, `.5`, 指数表記 `1e3`, `1.2e-3`）
- [x] 文字列リテラル（`"..."`）
- [x] 真偽/Noneリテラル（`True`, `False`, `None`）
- [x] 演算子（`+ - * / % //`, 比較, `and/or/not`）
- [x] 複合代入（`+= -= *= /= %= //=`）
- [x] 制御構文キーワード（`if/elif/else`, `while`, `for/in`, `break`, `continue`）
- [x] 関数関連（`def`, `return`, `global`, `pass`, `lambda`, `yield`, `yield from`）
- [x] `import` / `from ... import`（alias, dotted path, relative, star import）
- [x] 例外構文キーワード（`try`, `except`, `finally`, `raise`）
- [x] パターンマッチ構文キーワード（`match`, `case`）
- [x] `with` 文キーワード
- [x] `INDENT`/`DEDENT` トークン生成（複文ブロック）
- [x] デコレータ（`@`）
- [x] `class` キーワード

### 構文解析（Parser）
- [x] 文: `print`, 代入, 複合代入, `return`, `global`, `pass`
- [x] 文: `import` / `from ... import`（alias, dotted, relative, star, 括弧付き）
- [x] 文: `try/except`（複数節・型指定・alias対応）、`try/except/finally`、`raise`
- [x] 文: `match/case`（値/OR/シーケンス/マッピング/ガード/`as`/`**rest`対応）
- [x] 文: `class`（単一継承・デコレータ対応）
- [x] 文: class フィールド型注釈（`name: type` / `name: type = expr`）対応
- [x] 文: `with`（Context Manager・複数節・変数束縛対応）
- [x] 文: `yield` / `yield from`
- [x] 制御構文: `if/elif/else`, `while`, `for`
- [x] 関数定義: `def name(args): ...`（デフォルト引数・` *args`/` **kwargs`・型注釈）
- [x] 式: 四則演算（`+ - * / % //`）, 比較, `not`, 関数呼び出し, `lambda`
- [x] 式: tuple（literal, index/slice, unpack, 比較, for反復）
- [x] 式: list comprehension（for/if/nested/chained/if・walrus・unpack target）
- [x] 式: walrus operator（`:=`）
- [x] 組み込みのメソッド呼び出し構文（例: `x.append(3)` を関数呼び出しへデシュガー）
- [x] リテラル: int/float/string/list/dict/None/tuple
- [x] インラインsuiteとインデントsuiteの両対応

### 評価器（Evaluator — AST経路）
- [x] 変数束縛と式評価
- [x] 文字列連結（`"a" + "b"`）
- [x] 数値演算の暗黙昇格（int/float 混在）
- [x] `/` 実数除算 / `//` 床除算 / `%` 数値同士対応
- [x] 整数は任意精度（`Integer`）
- [x] 条件分岐・ループ実行
- [x] `break` / `continue` の制御伝播
- [x] 関数呼び出し（再帰・デフォルト引数・keyword引数を含む）
- [x] 関数スコープ（引数優先・グローバル参照可・`global`文対応）
- [x] truthiness（`None`、空文字、空リスト、空辞書、空tuple）
- [x] 反復回数ガード（2000超で `Value error`）
- [x] 位置情報つきエラー報告（Name/Type/Value/Argument count など）
- [x] keyword引数エラー（duplicate/unexpected/multiple-values/builtin keyword拒否）
- [x] default引数評価順・競合優先順位

### VM実行（コンパイル+実行経路）
- [x] AST → VM指令へのコンパイル
- [x] スレッド型VM（命令フェッチ→ディスパッチ）
- [x] 関数フレーム・ローカル・グローバル環境
- [x] 制御構文（if/while/for）のジャンプ命令
- [x] `break`/`continue` のループ制御
- [x] 関数呼び出し（ユーザー定義・組み込み）
- [x] keyword引数・デフォルト引数
- [x] `global`文・複合代入
- [x] 組み込み関数（`len`, `bool`, `range`, list/dict系）
- [x] `import`解決（math/dataclasses/local package/submodule/relative/star）
- [x] 例外処理（`try/except/finally`, `raise`）
- [x] `match/case`（パターンマッチ実行）
- [x] `class`（インスタンス生成・メソッド呼び出し・継承・属性解決）
- [x] `dataclass`（`__init__`/`__repr__`/`__eq__`/`order`/`frozen`自動生成）
- [x] `with`（Context Manager実行）
- [x] `yield`/`yield from`（generator）
- [x] tuple（リテラル・比較・index・slice・unpack・match・for反復）
- [x] 任意精度整数算術

### 組み込み関数（実装済み）
- [x] `len(x)`（string/list/tuple）
- [x] `bool(x)`（int/float/None/string/list/dict/tupleのtruthiness）
- [x] `range(stop)` / `range(start, stop)` / `range(start, stop, step)`
- [x] `append(list, value)` / `remove(list, value)` / `sort(list)` / `reverse(list)` / `insert(list, index, value)` / `pop(list)` / `clear(list)`
- [x] `keys(dict)` / `values(dict)` / `items(dict)`（挿入順維持）
- [x] `get(dict, key)` / `get(dict, key, default)`
- [x] `update(dict, key, value)` / `update(dict, otherDict)`
- [x] `setdefault(dict, key)` / `setdefault(dict, key, default)`
- [x] `pop(dict, key)` / `pop(dict, key, default)`
- [x] `math`: `sqrt`, `sin`, `cos`, `tan`, `log`, `exp`, `pi`, `e`
- [x] `json`: `dumps`, `loads`
- [x] `pathlib`: `Path`
- [x] `os`: `getcwd`
- [x] `dataclass`（デコレータ・`field(default_factory=...)`）

補足:
- `math.pi` / `math.e` は現仕様では関数形式（`math.pi()` / `math.e()`）で利用します。
- `import pkg.sub` / `import pkg.sub.deep` のような dotted import では、非 alias 時のトップレベル束縛は root module（例: `pkg`）のみです。`sub` / `deep` を直接参照したい場合は `from ... import ...` または `import ... as ...` を使います。
- `from . import x` などの relative import は、package 配下モジュール内では利用可能です（エントリスクリプト直下では parent package がないためエラー）。
- `from pkg import *` はローカル package/module に対して利用可能です（先頭 `_` の名前は除外、`__all__` 優先）。

## 互換性ギャップ（継続改善中）
- [ ] Python完全互換（継続して差分を縮小）
- [ ] Pythonの属性解決/メソッド解決の完全互換（descriptor/protocol を含む）
- [ ] f-string
- [ ] `async`/`await`
- [ ] `nonlocal`
- [ ] ファイルI/O（`open`, `read`, `write`）
- [ ] その他標準ライブラリ

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

### 3. 品質ゲート（一括実行）
```bash
cabal run quality-gate
```

これは以下を順番に実行します:
- `cabal test`
- `cabal run check-structure`
- `ormolu --mode check`（コードフォーマットチェック）
- `hlint src app`（静的解析）
- `cabal run detect-positional-args -- src`（レコード構文コンプライアンス）

### 4. 位置引数検出（レコード構文コンプライアンス）
```bash
# ディレクトリ全体をスキャン
cabal run detect-positional-args -- src

# 単一ファイルをスキャン
cabal run detect-positional-args -- src/PythonHS/Structure/FooBar.hs
```

検出対象:
- `positional_record_con` — `data`/`newtype` の非レコードコンストラクタ（レコード構文必須）
- `function_declaration` — 2+引数の関数定義（引数をレコード型にまとめる）
- `tuple` — 2+要素のタプル（専用レコード型に置き換える）

デフォルトはプレーンテキスト、`--json` で JSON 出力に切り替え可能です。

```bash
# プレーンテキスト（デフォルト）
cabal run detect-positional-args -- src

# JSON
cabal run detect-positional-args -- --json src

# 特定パスの除外（複数指定可）
cabal run detect-positional-args -- --exclude test/ --exclude app/ src
```

### 5. ファイル実行
```bash
cabal run python-hs -- examples/sample.pyhs
```

### 6. REPL起動
```bash
cabal run python-hs
```

### 7. VMエンジンで起動
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

## 品質基盤
- `cabal test` が成功（856 examples）
- `cabal run check-structure` が成功（1ファイル1関数・1型・200行制限）
- `ormolu --mode check` が成功（自動フォーマット）
- `hlint src app` が成功（静的解析）
- `cabal run detect-positional-args -- src` で `positional_record_con` / `function_declaration` / `tuple` を監視
- コンパイラ警告 0
- 可読性最優先の設計判断
