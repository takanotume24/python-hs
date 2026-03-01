# python-hs

Haskellで実装した、Python 3 サブセットのインタプリタです。  
TDD（`hspec`）で機能を拡張し、`Lexer -> Parser -> Evaluator -> Runner/CLI` を段階的に実装しています。

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
- [x] `import` キーワード（最小: `import math`）
- [x] `INDENT`/`DEDENT` トークン生成（複文ブロック）

### 構文解析（Parser）
- [x] 文: `print`, 代入, 複合代入, `return`, `global`, `pass`, `import`
- [x] 制御構文: `if/elif/else`, `while`, `for`
- [x] 関数定義: `def name(args): ...`
- [x] 式: 四則演算（`+ - * / % //`）, 比較, `not`, 関数呼び出し
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
- [x] 反復回数ガード（2000超で `Value error`）
- [x] 位置情報つきエラー報告（Name/Type/Value/Argument count など）

### 組み込み関数（実装済み）
- [x] `len(x)`（string/list）
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
- [x] VM最小 math 導線: `import math` + `math.sqrt/sin/cos/tan/log/exp/pi/e`

補足:
- `math` は現時点で VM 実行経路を対象にした MVP 実装です。
- `import` は現時点で `math` のみを受理し、`import os` など他モジュールは `Import error` になります。
- `math.pi` / `math.e` は現仕様では関数形式（`math.pi()` / `math.e()`）で利用します。

## MVP外・未対応（明示）
- [ ] Python完全互換（あくまでサブセット）
- [ ] 任意オブジェクトの一般メソッド解決（現状は組み込み相当の構文糖衣のみ）
- [ ] 例外処理、クラス、汎用import（任意モジュール読み込み）など高度機能

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
