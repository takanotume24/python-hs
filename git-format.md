# Git コミットメッセージのフォーマット

このリポジトリでは履歴を明確にし、レビューを容易にするために
構造化されたコミットメッセージ形式を採用します。

## 構造

各コミットメッセージは次の要素で構成します。

1. **ヘッダー**
   - 変更内容の短い要約（50文字以内）。
   - 命令形を使う（例："Add feature"、"Fix bug"）。
   - 句点（ピリオド）は付けない。

2. **空行**

3. **本文**（任意だが非自明な変更では推奨）
   - _何を_、_なぜ_行ったのかを説明し、_どのように_は書かない。
   - 行は72文字で折り返す。
   - 複数の項目がある場合は箇条書きを使う。

4. **フッター**（任意）
   - 関連する issue 番号を含める（例: `Fixes #123`）。
   - 破壊的変更は `BREAKING CHANGE:` で始める。

## 例

```
Add evalProgram tests

Add initial unit tests for `evalProgram` covering basic
expressions and error conditions.

Fixes #42
```

```
Refactor Lexer.scanTokens

Simplify token scanning logic and improve performance by
caching results. No functional change.
```

---

これらのルールに従うことでプロジェクト履歴が読みやすくなり、
チームメンバー（および将来のあなた自身）がなぜ変更が行われたのかを
理解しやすくなります。