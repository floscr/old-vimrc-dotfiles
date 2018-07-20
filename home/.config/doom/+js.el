;;; ~/.homesick/repos/Dotfiles/home/.config/doom/+js.el -*- lexical-binding: t; -*-

(setq flycheck-javascript-eslint-executable (executable-find "eslint_d"))
(after! rjsx-mode (add-hook 'js2-mode-hook #'eslintd-fix-mode))
