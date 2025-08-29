#!/usr/bin/env gxi
;;; -- Gerbil --
(import :std/build-script)

;; This script defines the build process for your project.
;; Gerbil automatically finds and compiles all dependencies
;; (like data/stop-words.ss, category.ss, etc.) by following
;; the 'import' statements from the main and testapp files.
(defbuild-script
  '((script: "testapp" bin: "testapp")
    (script: "main" bin: "kbtm")))
