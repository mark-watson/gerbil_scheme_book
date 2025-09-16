# Setting Up Gerbil Scheme Development Environment

We covered the basic installation of Gerbil Scheme for macOS and Linux in the **Preface**. Here cover setting up Emacs to edit Gerbil Scheme.

## Emacs Configuration

Assuming that you have Emacs installed with the file **~/.emacs** and the directory **~/.emacs.d/**, copy the following two files into **~/.emacs.d/**:

- [https://github.com/gambit/gambit/blob/master/misc/gambit.el](https://github.com/gambit/gambit/blob/master/misc/gambit.el)
- [https://github.com/mighty-gerbils/gerbil/blob/master/etc/gerbil-mode.el](https://github.com/mighty-gerbils/gerbil/blob/master/etc/gerbil-mode.el)

Then add the following to your **~/.emacs** file:

{lang="lisp", linenos=off}
```
(load "~/.emacs.d/gambit.el")
(load "~/.emacs.d/gerbil-mode.el")

(autoload 'gerbil-mode "gerbil-mode" "Gerbil editing mode." t)
(require 'gambit)
(add-hook 'inferior-scheme-mode-hook 'gambit-inferior-mode)
(defvar gerbil-program-name
  (expand-file-name "/opt/gerbil/bin/gxi")) ; adjust for your GERBIL_INSTALL_PREFIX
(setq scheme-program-name gerbil-program-name)
```
