# python-hs

Haskellで実装した、Python 3 サブセットのインタプリタです。  
TDD（`hspec`）で機能を拡張し、`Lexer -> Parser -> Evaluator -> Runner/CLI` を段階的に実装しています。

このプログラムは、GitHub Copilotによって生成されたコード、及びtakanotume24が手動で修正したコードを含みます。

## 現在の実装状況（2026-02-24）

### 実行基盤
- [x] ソース文字列実行（`runSource`）
- [x] ファイル実行（`python-hs <file>`）
- [x] REPL（状態保持・複数行ブロック対応）

### 字句解析（Lexer）
- [x] 整数・識別子・改行
- [x] 文字列リテラル（`"..."`）
- [x] 真偽/Noneリテラル（`True`, `False`, `None`）
- [x] 演算子（`+ - * / % //`, 比較, `and/or/not`）
- [x] 複合代入（`+= -= *= /= %= //=`）
- [x] 制御構文キーワード（`if/elif/else`, `while`, `for/in`, `break`, `continue`）
- [x] 関数関連（`def`, `return`, `global`, `pass`）
- [x] `INDENT`/`DEDENT` トークン生成（複文ブロック）

### 構文解析（Parser）
- [x] 文: `print`, 代入, 複合代入, `return`, `global`, `pass`
- [x] 制御構文: `if/elif/else`, `while`, `for`
- [x] 関数定義: `def name(args): ...`
- [x] 式: 四則演算（`+ - * / % //`）, 比較, `not`, 関数呼び出し
- [x] リテラル: int/string/list/dict/None
- [x] インラインsuiteとインデントsuiteの両対応

### 評価器（Evaluator）
- [x] 変数束縛と式評価
- [x] 文字列連結（`"a" + "b"`）
- [x] 条件分岐・ループ実行
- [x] `break` / `continue` の制御伝播
- [x] 関数呼び出し（再帰を含む）
- [x] 関数スコープ（引数優先・グローバル参照可）
- [x] `global` / `pass` は no-op として評価
- [x] truthiness（`None`、空文字、空リスト、空辞書）
- [x] 反復回数ガード（10000超で `Value error`）
- [x] 位置情報つきエラー報告（Name/Type/Value/Argument count など）

### 組み込み関数（実装済み）
- [x] `len(x)`（string/list）
- [x] `bool(x)`（int/None/string/list/dictのtruthiness）
- [x] `range(stop)` / `range(start, stop)` / `range(start, stop, step)`
- [x] `append(list, value)`
- [x] `remove(list, value)`
- [x] `sort(list)`（int要素のみ対応）
- [x] `insert(list, index, value)`
- [x] `pop(list)` / `pop(dict, key)` / `pop(dict, key, default)`
- [x] `clear(x)`（list/dict）
- [x] `keys(dict)` / `values(dict)` / `items(dict)`（挿入順維持）
- [x] `get(dict, key)` / `get(dict, key, default)`
- [x] `update(dict, key, value)`
- [x] `setdefault(dict, key, default)`

## MVP外・未対応（明示）
- [ ] Python完全互換（あくまでサブセット）
- [ ] メソッド呼び出し構文（例: `x.append(3)`）
- [ ] 一部組み込みは未実装（例: `reverse`）
- [ ] 例外処理、クラス、import など高度機能

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
