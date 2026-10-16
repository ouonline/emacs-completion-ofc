# Overview

These are fuzzy completion backends for the native completion-at-point-functions(`CAPF`).

Candidates are case sensitive but the matching behavior is not. Modified or newly inserted words of a buffer cannot be found until this buffer is saved. Candidates are sorted by their used frequencies and similarities.

# Screenshots

## Token Completions

![ofc-token](img/ofc-token.png)

## Path Completions

![ofc-path](img/ofc-path.png)

# Settings

```lisp
(if (< emacs-major-version 29)
    (add-to-list 'load-path "/path/to/emacs-completion-ofc")
  (unless (package-installed-p 'emacs-completion-ofc)
    (package-vc-install "https://github.com/ouonline/emacs-completion-ofc")))

(require 'capf-ofc-token)

(add-hook 'prog-mode-hook
          (lambda ()
            (setq-local completion-at-point-functions '(capf-ofc-token))))
```
