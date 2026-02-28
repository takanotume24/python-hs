# 実装計画

## 現在の到達点
- [x] マイルストーン 0: プロジェクト初期設定
- [x] マイルストーン 1: レキサー
- [x] マイルストーン 2: AST + パーサ
- [x] マイルストーン 3: 評価器（MVP）
- [x] マイルストーン 4: REPL + ランナー（MVP）
- [x] マイルストーン 5: 受け入れ（MVP）

## 現在のスコープ（P1: 型拡張の開始）
### 文字列サポート
- [x] レキサー: `"..."` を `StringToken` として読み取り
- [x] パーサ: `StringToken` を `StringExpr` としてパース
- [x] 評価器: `print "hello"` を実行可能化
- [x] 評価器: `x = "hello"; print x` を実行可能化
- [x] 評価器: 文字列演算（`"a" + "b"`）
- [x] 評価器: 文字列比較（`==`, `!=`）

### 次の候補（P1 継続）
- [x] 組み込み関数（`len`）
- [x] 反復制御拡張（`for`, `break`, `continue`）
- [x] プリミティブ型拡張（リスト、辞書）
  - [x] リストの最小導入（`[1, 2]` のトークン/パーサ/print）
  - [x] 辞書の最小導入（`{1: 2}` のトークン/パーサ/print）

---

## 短期TODO（次スプリント）
1. [x] 仕様明文化: `range` / `items` / `values` の順序保証（現状: 入力順, テストで固定済み）
2. [x] 仕様明文化: リスト・辞書組み込みのMVP境界（`pop` はMVP外, テストで固定済み）
3. [x] 仕様明文化: 呼び出し形式の方針（当初は関数形式のみ、後続で組み込みメソッド構文を導入）
4. [x] 回帰防止: 上記仕様を受け入れテスト（Runner）へ反映

---

## 中期ロードマップ（P2）
- [x] 式の網羅性を拡張（`*` / `/` / `%`、単項演算子の型規則統一）
- [x] コレクション操作を整理（`append` 返り値方針、`get` のデフォルト値仕様）
- [x] 反復モデルを整理（`for in range` と `for in list/dict` の整合、辞書反復はキー反復で定義）
- [x] エラーメッセージ規約を統一（Type/Value/Name エラーの語彙と位置表記）

---

## 長期ロードマップ（P3+）
- Python3サブセット拡張（`elif`、複合代入、真偽値/None など）
- 関数機能拡張（ローカル/グローバル解決規則、引数仕様の拡張）
- データ構造拡張（辞書更新API、将来的なミュータブル操作の整備）
- 品質基盤強化（受け入れシナリオ増強、エラー互換テスト、性能回帰監視）

### P3 次アクション（着手中）
- [x] 複合代入の第1段階（`+=`）を導入（lex/parse/eval/runner）
- [x] `+=` の型規則を既存 `+` 規則に整合（int+int, string+string のみ）
- [x] `+=` の未定義識別子エラーを `Name error` 規約に整合
- [x] 回帰防止テスト（Lexer/Parser/Runtime/Runner）を追加
- [x] 複合代入の第2段階（`-=`）を導入（lex/parse/eval/runner）
- [x] 複合代入の第3段階（`*=`）を導入（lex/parse/eval/runner）
- [x] 複合代入の第4段階（`/=`）を導入（lex/parse/eval/runner）
- [x] 複合代入の第5段階（`%=`）を導入（lex/parse/eval/runner）
- [x] 複合代入の第6段階（`//=`）を導入（lex/parse/eval/runner）
- [x] `True` / `False` / `None` リテラルの最小導入（lex/parse/eval/runner）
- [x] `None` の truthiness を制御構文へ反映（if/while/not）
- [x] `None` 利用時のエラー互換を固定（len/算術の型エラー文言）
- [x] `bool(...)` 組み込みを最小導入（int/None 対応、型/引数エラー整備）
- [x] 関数本体からグローバル識別子を参照可能にする（ローカル優先で解決）
- [x] `bool(...)` の truthiness 対応を拡張（string/list/dict の空判定）
- [x] 制御構文/論理演算の truthiness を拡張（if/while/not/and/or で string/list/dict を許可）
- [x] 関数スコープ互換をテストで固定（引数シャドーイングと未定義識別子位置）
- [x] `global` 文を最小導入（lexer/parser/eval no-op）
- [x] `pass` 文を導入（lexer/parser/eval no-op）
- [x] `pass` と制御構文の相互作用テストを追加（if/while/for）
- [x] 反復上限ガードを導入（while/for の暴走を停止）
- [x] 複文ブロック対応の第1段階: `INDENT` / `DEDENT` を lexer に導入
- [x] 複文ブロック対応の第2段階: parser の suite 境界処理を修正（DEDENT 後の文継続を安定化）
- [x] 複文ブロック対応の第3段階: `if/elif/else` 複文後の後続文継続をテストで固定
- [x] 複文ブロック対応の第4段階: `elif` 内の入れ子条件分岐と後続文継続をテストで固定
- [x] 複文ブロック対応の第5段階: `while/for` 内の入れ子条件分岐と後続文継続をテストで固定
- [x] 複文ブロック対応の第6段階: インデント不整合（dedent不一致）の負系をテストで固定
- [x] 複文ブロック対応の第7段階: ブロックヘッダ直後にボディ欠落時の parse エラーをテストで固定
- [x] 複文ブロック対応の第8段階: `while/for` ヘッダ直後にボディ欠落時の parse エラーをテストで固定
- [x] 複文ブロック対応の第9段階: トップレベル不正 `elif/else` ヘッダの parse エラーをテストで固定
- [x] 複文ブロック対応の第10段階: 先頭インデント不正入力の parse エラーをテストで固定
- [x] 複文ブロック対応の第11段階: ヘッダ後に空行のみでボディ欠落する parse エラーをテストで固定
- [x] 複文ブロック対応の第12段階: 先頭タブ入力を空白として受理する現行仕様をテストで固定
- [x] 複文ブロック対応の第13段階: 行頭以外のタブ区切りを空白として受理する現行仕様をテストで固定
- [x] 複文ブロック対応の第14段階: 多段 `DEDENT` 終了後のトップレベル文継続を統合テストで固定
- [x] 複文ブロック対応の第15段階: ブロックヘッダのコロン欠落時 parse エラー位置をテストで固定
- [x] データ構造拡張の第1段階: `update(dict, key, value)` を導入（eval/runner）
- [x] データ構造拡張の第2段階: `pop(list)` を導入（末尾要素返却、空リストは Value error）
- [x] データ構造拡張の第3段階: `clear(list|dict)` を導入（eval/runner）
- [x] データ構造拡張の第4段階: `setdefault(dict, key, default)` を導入（eval/runner）
- [x] データ構造拡張の第5段階: `pop(dict, key[, default])` を導入（eval/runner）
- [x] データ構造拡張の第6段階: `remove(list, value)` を導入（eval/runner）
- [x] データ構造拡張の第7段階: `update(dict, otherDict)` を導入（辞書マージ, eval/runner）
- [x] データ構造拡張の第8段階: `setdefault(dict, key)` を導入（default省略時は `None`, eval/runner）
- [x] 呼び出し構文拡張の第1段階: 組み込みメソッド呼び出し（`x.append(3)`）を導入（lex/parse/runner）

### ブロック構文 仕様メモ（2026-02-24 時点）
- lexer は行頭スペースで `INDENT` / `DEDENT` を生成し、EOF 時に必要な `DEDENT` を flush する。
- `if / while / for / def` の suite は「インライン1文」または「改行 + インデント複文」を受理する。
- 複文終了時（`DEDENT`）は外側の文解析へ継続できる（後続トップレベル文を正しく解析する）。
- 不正ケース（ボディ欠落、トップレベル `elif/else`、不整合 dedent、先頭インデント不正）は parse/lexer エラーとして固定済み。
- タブは現行実装で空白として扱う（先頭タブ、トークン間タブとも受理）ため、その挙動をテストで固定済み。


## 品質ゲート
- 必須: `cabal test`
- 必須: `cabal run check-structure`
- 必須: コンパイラ警告 0

---

## メンテナンス記録（要約）
- 2026-02-28
  - [x] REPL改善: `exit()` 入力で対話REPLを終了できるよう `startRepl` を更新（通常評価せず終了）
  - [x] REPL改善: `replEvalLines` でも `exit()` で入力列評価を打ち切る挙動を追加
  - [x] 回帰防止: `CLISpec` に `exit()` 終了テスト（`replEvalLines` / 対話実行）を追加
  - [x] 品質ゲート再確認: `cabal test`（339 examples）/ `cabal run check-structure` 成功
- 2026-02-26
  - [x] P3継続: `setdefault(dict, key)` を追加（default省略時に `None` を補完、関数/メソッド形式を eval/runner で固定）
  - [x] P3継続: `update(dict, otherDict)` を追加（既存キー上書き+新規キー追加の辞書マージを eval/runner で固定）
  - [x] CI調整: GitHub Actions に `libncurses-dev` インストール手順を追加し、`haskeline` の `-ltinfo` リンク依存を満たすよう修正
  - [x] 非Nix再確認: `cabal test`（332 examples）/ `cabal run check-structure` 成功（`libncurses-dev` 導入後）
  - [x] REPL拡張: 入力処理を `haskeline` に移行し、矢印キーでの編集・履歴操作を有効化
  - [x] 依存調整: `python-hs.cabal` の library 依存に `haskeline` を追加
  - [x] 環境調整: `cabal.project` に `constraints: haskeline -terminfo` を追加（非Nix環境でのリンク回避を試行）
  - [x] Nix調整: `flake.nix` の dev shell に `ncurses` を追加し、REPL依存ライブラリを提供
  - [x] 品質ゲート（Nix）: `nix develop -c cabal test`（332 examples）/ `nix develop -c cabal run check-structure` 成功
  - [x] REPL調査: 初回/エラー後にプロンプトが出ない問題を修正（`isEOF` 先行判定を廃止し、入力待ちを `getLine` のEOF処理へ変更）
  - [x] REPL調査: 対話実行でのプロンプト回帰テストを追加（先頭 `>>>` 表示とエラー後の再表示を固定）
  - [x] REPL調査: EOF時に未送信バッファを評価して終了するよう `startRepl` を修正（ブロック入力終端の取りこぼしを解消）
  - [x] REPL調査: `CLISpec` を拡張（空行扱い、EOF送信、実行時エラー後継続、失敗後の環境維持、対話実行のEOFフラッシュ）
  - [x] 品質ゲート再確認: `cabal test`（331 examples）/ `cabal run check-structure` 成功
  - [x] P3継続: 辞書メソッド構文の回帰面を補完（`values/items/get(default)/pop(default)` と代表負系を固定）
  - [x] P3継続: 組み込みメソッド呼び出しを拡張（`remove` / `insert` / `sort` / `reverse` を受け入れテストで固定）
  - [x] P3継続: メソッド呼び出し時の型エラー位置互換を固定（`print 1.append(2)` で `at 1:9`）
  - [x] P3継続: 組み込みメソッド呼び出しを拡張（`update` / `pop` / `clear` の list/dict ケースを受け入れテストで固定）
  - [x] P3継続: 組み込みメソッド呼び出しの適用範囲を拡張（連鎖呼び出し・辞書組み込みの受け入れテストを追加）
  - [x] P3継続: 組み込みメソッド呼び出しの最小導入（`x.append(3)` を `append(x, 3)` と等価に評価）
  - [x] P3継続: `remove(list, value)` を追加（先頭一致要素のみ削除、未発見は Value error を eval/runner で固定）
  - [x] 開発環境の再現性向上のため `flake.nix` を追加（`nix develop` で GHC/Cabal/HLS/整形・Lint ツールを提供）
  - [x] `flake.nix` に `checks` を追加し、`nix flake check` でテストと `check-structure` を実行可能化
  - [x] `checks.cabal-test` を `runCommand` ラッパー化し、`nix flake check -L` 時にテスト実行済みログを明示
  - [x] `README` に `nix flake check -L path:.`（ログ表示あり）手順を追記
- 2026-02-24
  - [x] READMEを新規作成し、実装済みPythonサブセット機能とMVP境界（未対応範囲）を明文化
- 2026-02-19
  - [x] `PythonHS.CLI` を機能別モジュールへ分割
  - [x] 構造チェックを Haskell 実装へ移行
  - [x] 命名規約・ファイル粒度ルールへの準拠を完了
- 2026-02-20
  - [x] パーサ: `if/else` 改行ケース対応
  - [x] 評価器: `if` / `while` / 関数ランタイムのテスト強化
  - [x] エラー報告: `ParserErrorSpec` / `RuntimeErrorSpec` 追加
  - [x] 統合: `RunnerSpec` / `CLISpec` / `MvpScenarioSpec` 拡充
  - [x] P1開始: 文字列のレキサー・パーサ対応
  - [x] P1継続: 文字列の `print` と文字列変数の `print` 対応
  - [x] P1継続: 文字列連結 `"a" + "b"` 対応（混在型は型エラー維持）
  - [x] P1継続: 組み込み `len` を追加（成功/型エラー/引数個数エラーをテスト化）
  - [x] P1継続: 文字列比較 `==` / `!=` の評価器・統合テストを追加
  - [x] P1継続: `for` / `break` / `continue` と `range`（1引数）を追加
    - ループ内 `break` / `continue` の制御伝播
    - ループ外 `break` / `continue` の位置付きエラー
  - [x] P1継続: リストの最小導入（`[1, 2]` の lex/parse/eval/runner）
  - [x] P1継続: `for ... in <list>` を追加（`range(n)` と併存）
  - [x] P1継続: 組み込み `len` を `list` に拡張（`len("abc")` と `len([1,2])` の併存）
  - [x] P1継続: 組み込み `append(list, value)` を追加（新規リストを返す）
  - [x] P1継続: 型規則エラーメッセージを整理（`+` 混在型、`for` iterable エラー）
  - [x] P1継続: 辞書の最小導入（`{k: v}` の lex/parse/eval/runner）
  - [x] P1継続: 辞書組み込み `keys(dict)` / `get(dict, key)` を追加
  - [x] P1継続: 組み込み `values(dict)` / `items(dict)` を追加
  - [x] P1継続: `range(start, stop[, step])` を追加（step=0 は runtime error）
  - [x] P1継続: 単項マイナスを拡張（`-2`, `-x`, `-(1+2)`）
  - [x] P1継続: 単項マイナスの型エラー文言を明確化（`-"a"` で専用メッセージ）
  - [x] P1継続: `keys` / `values` / `items` の入力順維持をテストで固定
  - [x] P1継続: MVP境界を固定（`pop` は未対応）
  - [x] P1継続: 組み込み呼び出しを関数形式に固定（メソッド構文は未対応）
  - [x] P2開始: 乗除剰余（`*` / `/` / `%`）を lexer/parser/evaluator に追加
  - [x] P2継続: `get(dict, key, default)` を追加（未発見時 default を返す）
  - [x] P2継続: `for ... in {k:v}` を追加（辞書はキー反復、挿入順を維持）
  - [x] P2継続: 未定義識別子/関数エラーを `Name error` 語彙に統一
  - [x] P3開始: `elif` を lexer/parser/runner に追加（`if` の else-if 連鎖をサポート）
  - [x] P3継続: 複合代入 `+=` を追加（既存 `+` 型規則と Name error 規約を適用）
  - [x] P3継続: 複合代入 `-=` を追加（int専用、Name error 規約を適用）
  - [x] P3継続: 複合代入 `*=` を追加（int専用、Name error 規約を適用）
  - [x] P3継続: 複合代入 `/=` を追加（int専用、ゼロ除算は Value error）
  - [x] P3継続: 複合代入 `%=` を追加（int専用、ゼロ剰余は Value error）
  - [x] P3継続: 複合代入 `//=` を追加（int専用、ゼロ除算は Value error）
  - [x] P3継続: `True` / `False` / `None` リテラルを追加（print/比較の最小互換）
  - [x] P3継続: `None` を falsy として評価（if/while/not/and/or で 0 と同等扱い）
  - [x] P3継続: `None` 利用時のエラー互換を固定（`None + 1` と `len(None)` の型エラー文言）
  - [x] P3継続: `bool(...)` 組み込みを追加（int/None の最小truthiness変換）
  - [x] P3継続: 関数からグローバル変数の読み取りを許可（引数はローカル優先）
  - [x] P3継続: `bool(...)` を空文字/空リスト/空辞書でも評価可能に拡張
  - [x] P3継続: `if/while/not/and/or` を空文字/空リスト/空辞書でも評価可能に拡張
  - [x] P3継続: 関数スコープ互換テストを追加（引数優先解決と Name error 位置）
  - [x] P3継続: `global` 文を追加（キーワード認識・構文解析・実行時 no-op）
  - [x] P3継続: `pass` 文を追加（トップレベル/関数本体で no-op）
  - [x] P3継続: `pass` の制御構文内ケースを受け入れテストで固定（if/while/for）
  - [x] P3継続: 反復回数ガードを追加（while/for 10000回超で Value error）
  - [x] P3継続: インデント字句化を追加（`INDENT` / `DEDENT` の発行）
  - [x] P3継続: parser の suite 境界処理を修正（DEDENT 後に次文を継続可能化）
  - [x] P3継続: `if/elif/else` 複文後の後続文継続を parser/runner テストで固定
  - [x] P3継続: `elif` ブランチ内の入れ子条件分岐と後続文継続を parser/runner テストで固定
  - [x] P3継続: `while/for` ブランチ内の入れ子条件分岐と後続文継続を parser/runner テストで固定
  - [x] P3継続: インデント不整合（dedent不一致）の lexer/runner エラーを固定
  - [x] P3継続: `if/def` ヘッダ直後にボディ欠落時の parse エラー位置を parser/runner テストで固定
  - [x] P3継続: `while/for` ヘッダ直後にボディ欠落時の parse エラー位置を parser/runner テストで固定
  - [x] P3継続: トップレベル不正 `elif/else` ヘッダの parse エラー位置を parser/runner テストで固定
  - [x] P3継続: 先頭インデント不正入力（`  print 1`）の parse エラー位置を parser/runner テストで固定
  - [x] P3継続: `if/while` ヘッダ後に空行のみでボディ欠落する parse エラー位置を parser/runner テストで固定
  - [x] P3継続: 先頭タブ入力（`\tprint 1`）を空白として受理する現行挙動を lexer/runner テストで固定
  - [x] P3継続: 行頭以外のタブ区切り（`x\t=\t1`, `print\tx`）を空白として受理する現行挙動を lexer/runner テストで固定
  - [x] P3継続: 多段 `DEDENT`（`while/for/if` ネスト終了）後のトップレベル文継続を Runner 統合テストで固定
  - [x] P3継続: `if/while/for/def` ヘッダのコロン欠落時 parse エラー位置を parser/runner テストで固定
  - [x] P3継続: `update(dict, key, value)` を追加（既存キー更新/新規キー追加を eval/runner で固定）
  - [x] P3継続: `pop(list)` を追加（末尾要素返却、空リスト/型/引数個数エラーを eval/runner で固定）
  - [x] P3継続: `clear(list|dict)` を追加（空コレクション返却、型/引数個数エラーを eval/runner で固定）
  - [x] P3継続: `setdefault(dict, key, default)` を追加（既存キーは不変、未存在キーは追加、型/引数個数エラーを eval/runner で固定）
  - [x] P3継続: `pop(dict, key[, default])` を追加（キー存在時は値返却、未存在時はdefault返却/エラーを eval/runner で固定）
  - [x] 最新品質ゲート: `cabal test`（264 examples） / `cabal run check-structure` 成功
  - [x] CI改善: GitHub Actions に Cabal キャッシュ（`~/.cabal/packages`, `~/.cabal/store`, `dist-newstyle`）を追加
