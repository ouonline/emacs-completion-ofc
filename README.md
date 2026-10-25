# emacs-completion-ofc

Fuzzy completion for Emacs' native completion-at-point facility, plus extensions for `fido-mode`.

## Description

`emacs-completion-ofc` adds fuzzy completion to Emacs' native `completion-at-point-functions` (CAPF) and includes quality-of-life extensions for `fido-mode`.

The token backend indexes words of four or more characters (by default) across every buffer where it is enabled, keeps the index current while you type, and matches input case-insensitively as a subsequence. Candidates are ranked by lifetime usage frequency, then match quality, annotated with a source buffer, and shown with matched characters highlighted. A fuzzy path-candidate helper and the fido extras are included. Requires Emacs 30+ (`track-changes`).

## Features

- **Live token index** — indexes eligible buffers and updates changed regions as you type; no save required.
- **Fuzzy matching** — case-insensitive subsequence matching over the buffer's syntax-table words.
- **Ranking** — most-used tokens first; ties favor shorter tokens and exact-case matches.
- **Completion UI** — source-buffer annotations and matched-region highlighting, with lazy highlighting when supported.
- **Path candidates** — `ofc-path` provides cached fuzzy matching for absolute, relative, and `~` paths.
- **Fido extras** — buffer/recent-file switching, directory-aware `/` and `RET`, and `./` filtering.

## Requirements

- Emacs 30 or newer (`track-changes` is built in).
- `recentf` (built in) for recent-file candidates in `ofc-fido-switch-buffer`.

## Installation

```lisp
(unless (package-installed-p 'emacs-completion-ofc)
  (package-vc-install "https://github.com/ouonline/emacs-completion-ofc"))
```

## Usage

### Token completion

```lisp
(require 'capf-ofc-token)

(add-hook 'prog-mode-hook
          (lambda ()
            (setq-local completion-at-point-functions '(capf-ofc-token))))
```

Invoke your usual CAPF command (for example `completion-at-point`, bound to
`M-TAB` by default). Add the backend to other mode hooks as needed.

Customize `ofc-token-min-len` (default 4) to change the shortest token that is
indexed. Set it before the buffer is indexed.

### Fido extras

```lisp
(require 'ofc-fido-extra)

(fido-mode 1)
(fido-vertical-mode 1)  ; optional
(recentf-mode 1)        ; enables recent-file candidates
```

- `M-x ofc-fido-switch-buffer` — pick a buffer or an unopened recent file. Buffers come first; recent files are matched by base name and opened by full path.
- `M-n` / `M-p` — move through vertical completion candidates.
- `/` in file completion — enter the only matching directory; after a slash, jump to the completion root.
- `RET` — enter the selected directory instead of exiting; otherwise exit normally.
- `./` is hidden from `find-file` completion.

## Modules

| File | Role |
| --- | --- |
| `capf-ofc-token.el` | CAPF token-completion backend |
| `ofc-token.el` | Token index, incremental updates, and ranking |
| `ofc-common.el` | Shared fuzzy subsequence matcher |
| `ofc-fido-extra.el` | `fido-mode` commands and key bindings |

## Notes

- The token index is kept in memory for the Emacs session; usage frequencies reset on restart.
- Only buffers with `capf-ofc-token` in `completion-at-point-functions` are indexed.
- Tokens shorter than `ofc-token-min-len` are ignored, and buffers whose names contain `*temp*` are skipped.
- Matching is case-insensitive, but completed text keeps its original spelling.

## Screenshots

### Token completion

![ofc-token](img/ofc-token.png)

### Path completion

![ofc-path](img/ofc-path.png)
