;;; -*- Gerbil -*-
(import :std/error
        :std/sugar
        :std/cli/getopt
        ./lib)
(export main)

(def (main . args)
  (call-with-getopt command_line_utilities-main args
    program: "command_line_utilities"
    help: "A one line description of your program"
    ;; commands/options/flags for your program; see :std/cli/getopt
    ;; ...
    ))

(def* command_line_utilities-main
  ((opt)
   (command_line_utilities-main/options opt))
  ((cmd opt)
   (command_line_utilities-main/command cmd opt)))

;;; Implement this if your CLI doesn't have commands
(def (command_line_utilities-main/options opt)
  (error "Implement me!"))

;;; Implement this if your CLI has commands
(def (command_line_utilities-main/command cmd opt)
  (error "Implement me!"))
