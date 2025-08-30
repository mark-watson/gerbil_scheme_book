#!/usr/bin/env gxi
;;; -*- Gerbil -*-
(import :std/build-script)

(defbuild-script
  '("command_line_utilities/lib"
    (exe: "command_line_utilities/main" bin: "command_line_utilities")))
