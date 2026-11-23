# haskell-structure-lint

Haskell プロジェクトの「ファイル構成」と「位置引数」を検査する小さな linter です。
もともと [python-hs](https://github.com/takanotume24/python-hs) の品質ゲート用に育った
チェックを、他のプロジェクトでも使えるように切り出したものです。

## 検査内容

### 構成チェック (`check`)

- 1 ファイル 1 トップレベル関数
- 1 ファイル 1 型定義（`data` / `newtype` / `class` / `type`）
- ファイル名とトップレベル宣言名の一致（`fooBar` → `FooBar.hs`）
- モジュール名とパスの一致
- 関数名は camelCase、型名は PascalCase
- `test/` 配下のファイル名は `Spec` で終わる
- 行数上限（既定 200 行）

### 位置引数チェック (`positional`)

- レコード構文を使っていない `data` / `newtype` のコンストラクタ
- 4 つ以上の位置引数を持つトップレベル関数
  - 単純な再帰パターンは除外
  - `skipFunctionPaths` に一致するパスは除外

## インストール

```bash
cabal build
cabal install --installdir="$HOME/.local/bin"
```

## 使い方

```bash
# 統合 CLI
structure-lint check --config structure-lint.json
structure-lint positional --config structure-lint.json --json src

# 互換 CLI（python-hs から移行しやすい名前）
check-structure --config structure-lint.json
detect-positional-args --config structure-lint.json src
```

`--config` を省略すると、カレントディレクトリの `structure-lint.json` を読みます。
それも無ければ既定値で動作します。

### 主なオプション

```
structure-lint check [--config FILE] [--root DIR] [--max-lines N]
structure-lint positional [--config FILE] [--json] [--exclude PATTERN]... [PATH]
```

- `--root DIR`: 構成チェックの起点ディレクトリ（既定 `.`）
- `--max-lines N`: 行数上限の上書き
- `--json`: 位置引数の検査結果を JSON で出力
- `--exclude PATTERN`: パスに含まれる文字列でファイルを除外（複数指定可）

## 設定ファイル

```json
{
  "structure": {
    "root": ".",
    "roots": ["src", "app", "test"],
    "maxLines": 200,
    "lineExemptions": [],
    "testDirPrefixes": ["test/"],
    "appDirPrefixes": ["app/"],
    "basenameModulePrefixes": ["app/"],
    "discoverDriver": "test/Spec.hs",
    "testSuffix": "Spec",
    "moduleRootPrefixes": ["src", "app", "test"]
  },
  "positionalArgs": {
    "excludes": [],
    "skipFunctionPaths": ["/VM/"]
  }
}
```

| フィールド | 意味 |
| --- | --- |
| `structure.root` | 検査対象のルートディレクトリ |
| `structure.roots` | ルートからの相対で走査するディレクトリ |
| `structure.maxLines` | 1 ファイルの行数上限 |
| `structure.lineExemptions` | 行数上限を免除するファイル（警告として表示） |
| `structure.testDirPrefixes` | テストファイルとみなすパス接頭辞 |
| `structure.appDirPrefixes` | アプリケーションコードとみなすパス接頭辞 |
| `structure.basenameModulePrefixes` | モジュール名がファイル名と一致してよいパス接頭辞 |
| `structure.discoverDriver` | モジュール宣言を免除する hspec-discover ドライバ |
| `structure.testSuffix` | テストファイル名の必須サフィックス |
| `structure.moduleRootPrefixes` | モジュール名から取り除く先頭ディレクトリ |
| `positionalArgs.excludes` | 検査しないパス（部分一致） |
| `positionalArgs.skipFunctionPaths` | 関数宣言チェックをしないパス（部分一致） |

## 制限

- 構成チェックは AST ではなく行ベースの簡易判定です。複雑なレイアウトや
  マルチライン宣言を完全には理解しません。
- 位置引数チェックは `haskell-src-exts` で GHC2024 相当の拡張を有効にして解析します。
  将来の GHC 構文追加には追従が必要です。

## 開発

```bash
cabal test
ormolu --mode check $(git ls-files '*.hs')
hlint src app
```
