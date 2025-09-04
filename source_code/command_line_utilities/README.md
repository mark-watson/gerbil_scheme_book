# General Purpose Exmale of a Gerbil Scheme Command Line Utility

Build:

    make

Run:

    .gerbil/bin/command_line_utilities --help
    .gerbil/bin/command_line_utilities pwd
    .gerbil/bin/command_line_utilities ls
    
If you add this to your .profile, .bashrc, or zshrc file:

    export PATH=$PATH:.gerbil/bin

Then local executables created in Gerbil projects will be available in your project directories, for exmaple:

```bash
$ export PATH=$PATH:.gerbil/bin
$ command_line_utilities pwd
/Users/markw/GITHUB/gerbil_scheme_book/source_code/command_line_utilities/
```
