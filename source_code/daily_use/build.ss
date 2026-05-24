#!/usr/bin/env gxi
;;; -*- Gerbil -*-
(import :std/build-script)

(defbuild-script
  '("daily_use/cache"
    (exe: "daily_use/main" bin: "daily-use")))
