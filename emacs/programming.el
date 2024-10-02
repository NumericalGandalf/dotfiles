(setq-default tab-width 4)

(with-eval-after-load 'compile
  (setq compilation-ask-about-save nil
        compile-command nil
        compilation-auto-jump-to-first-error t))

(with-eval-after-load 'cc-vars
  (setq-default c-basic-offset 4))

(with-eval-after-load 'simple
  (setq-default indent-tabs-mode nil))

(use-package magit
  :init
  (setq magit-auto-revert-mode nil))
