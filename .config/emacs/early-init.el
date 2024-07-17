(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache "var/eln-cache/"))

(menu-bar-mode 0)

(let ((font "Fira Code"))
  (set-face-attribute 'default nil :font font :height 130)
  (set-face-attribute 'fixed-pitch nil :family font)
  (set-face-attribute 'fixed-pitch-serif nil :family font)
  (set-face-attribute 'variable-pitch nil :family font))
