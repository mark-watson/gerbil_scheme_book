# Writing Command Line Utilities"

Writing command line utilities is one of the most rewarding ways to use Gerbil Scheme. A small, fast executable that does one thing well can become a trusted part of your daily workflow, and Gerbil makes this style of development unusually pleasant. With a standard library that includes tools for argument parsing, clean error reporting, and multi-call interfaces, you can focus on the logic of your utility rather than boilerplate. The result is code that feels both expressive and pragmatic: terse enough to fit in a few dozen lines, yet powerful enough to interoperate with your wider toolchain. In a world of ever-growing frameworks and layers of abstraction, a lean Gerbil command line program can be a breath of fresh air.

The Gerbil ecosystem is designed around building and packaging binaries, so distribution is straightforward. A single source file can be compiled into a standalone executable, or for more ambitious projects, you can define a build script that packages multiple tools under one roof. Command line options are handled declaratively with getopt, while helper libraries provide standard exit codes, usage banners, and even multi-call support for utilities that expose multiple subcommands. This structured approach encourages writing utilities that are not only functional but also friendly to end users—programs that behave consistently and provide clear feedback. Once you have mastered this pattern, it becomes natural to treat Gerbil as both a systems language and a scripting tool.

Equally important is the mindset that comes with writing command line utilities. Each utility should feel like a sharp instrument—simple to invoke, with predictable behavior and minimal dependencies. Gerbil lets you achieve this balance while still giving you access to higher-level abstractions when needed, whether you are parsing JSON, making HTTP requests, or wrapping a C library through the foreign functionmain.ss files interface. This chapter will show how to construct utilities from the ground up, starting with a minimal “hello world” executable and building toward more ambitious projects. Along the way, we will explore not just the mechanics of building programs, but also the design philosophy that makes small tools enduringly useful.

## Overview of the Command Line Utilities Project Structure and Build System

Here we look at two examples of ways to structure Gerbil Scheme command line applications set up in two project directories:

- command_line_utilities_first_demo_START_HERE - This is the simplest structure and the one I usually use.
- command_line_utilities - This is a more general purpose structure using a separate lib.ss and main.ss files.


## Simple Structure for Command Line Utilities

Let’s take a look at the directory structure and code:

```console
$ pwd
command_line_utilities_first_demo_START_HERE
$ ls
README.md	test-tool.ss
$ gxc -exe -o test-tool test-tool.ss
$ ls -lh
total 128
-rw-r--r--  1 markw  staff   206B Aug 30 12:40 README.md
-rwxr-xr-x  1 markw  staff    54K Aug 30 12:39 test-tool
-rw-r--r--@ 1 markw  staff   601B Aug 30 12:33 test-tool.ss
$ ./test-tool --name Mark -v 
verbose on
Hello, Mark
$ rm test-tool
$
```

The example (test tool):

```scheme
;; test-mytool.ss
(export main)
(import :std/cli/getopt :std/cli/print-exit)
(import :std/format) ;; for 'format'

(def (usage)
  (print-exit 2 "usage: mytool [-v] --name=STR [ARGS...]"))

(def (main . argv)
  (let* ((parser (getopt (flag 'verbose "-v" "--verbose")
                         (option 'name "--name" help: "Your name" value: identity)))
         (opts (getopt-parse parser argv))
         (name (hash-get opts 'name))
         (verbose (hash-get opts 'verbose)))
    (unless name (usage))
    (when verbose (display "verbose on\n"))
    (displayln (format "Hello, ~a\n" name))
    0))
```

