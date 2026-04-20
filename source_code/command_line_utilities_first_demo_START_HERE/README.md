# Simple test comand line utility

**Book Chapter:** [Introduction To Writing Command Line Utilities](https://leanpub.com/read/Gerbil-Scheme/introduction-to-writing-command-line-utilities) — *Gerbil Scheme in Action* (free to read online).


Build a dynamically linked executable:

    gxc -exe -o test-tool test-tool.ss

Run it:

    $ ./test-tool --name Mark -v                  
    verbose on
    Hello, Mark
