# Preface

I have used Lisp languages since the late 1970s, Common Lisp and Clojure professionally and other Lisp languages, mostly various Scheme implementations, for writing utilities, network programming, and various AI experiments. This book is specifically about effectively using Gerbil Scheme for writing software to solve practical problems.

Both the source code examples and the manuscript files for this book are maintained in a single GitHub repository: [https://github.com/mark-watson/gerbil_scheme_book](https://github.com/mark-watson/gerbil_scheme_book). I recommend that you keep a local copy:

    git clone https://github.com/mark-watson/gerbil_scheme_book.git

## A Comment on Licenses

The source code examples are licensed using the "business friendly" LGPL license and the manuscript files under a Creative Commons Share and Share Alike, No Commercial Reuse license. It is my hope, dear reader, that you get value from this material and reuse it in your own projects. Please note that LGPL software can be freely mixed in projects with other code using Apache 2, MIT, etc. licenses. LGPL code can be freely used in commercial applications but if you modify LGPL code you need to publicly release just your changes to the LGPL code, not the code you write. LGPL is very different from the GPL and AGPL licenses!

## History and Background to the Gerbil Scheme Project

I recommend that you keep a local the GitHub repository for Gerbil Scheme:

    git clone https://github.com/mighty-gerbils/gerbil.git
    
You will find useful tutorial examples for using Gerbil Scheme in the sub-directory **gerbil/src/tutorial**.

### Introduction: Gerbil as a Systems Language

While Gerbil Scheme is another dialect in the Lisp family, it is like Racket Scheme in that it is "opinionated", reflecting the Gerbil developers'  style an philosophy.

Gerbil is built on Gambit Scheme, a high-performance, retargetable compile.  Gerbil inherits a legacy of speed and portability while introducing a state-of-the-art module and object system inspired by Racket. The result is a language engineered for creating efficient, concurrent, and robust long-running applications, positioning it as a powerful tool for tasks ranging from web services to distributed systems.

Gerbil has a comprehensive standard library that provides a "batteries included" experience uncommon in the often-fragmented Scheme ecosystem. Rather than relying on a disparate collection of third-party packages for fundamental operations, Gerbil offers canonical, high-quality, and officially maintained implementations for core functionalities. This includes built-in libraries for HTTP clients and servers, JSON parsing and serialization, cryptographic primitives, and database drivers. This approach favors a strong, coherent core, ensuring that developers have a stable and predictable foundation for building complex systems.

Gerbil Scheme can also function as a systems language with its "Integrated FFI" (Foreign Function Interface). This FFI allows Gerbil code to interface directly and efficiently with libraries written in C. The FFI is useful for specialized applications like high-speed data parsing or hardware interaction.

Gerbil provides powerful general-purpose primitives and expects the developer to compose them to interact with specific protocols, often for systems-level software development.

### Gerbil Ecosystem: Package Management and Foundational Libraries

Navigating any programming ecosystem begins with understanding its tooling for package management and the core libraries that form the bedrock of application development. In Gerbil, these components are designed with the same philosophy of directness and control that characterizes the language itself.

The Gerbil Package Manager (gxpkg)

Gerbil includes a command-line package manager **gxpkg**, invoked as **gerbil pkg** or its alias **gxpkg**.

The essential commands for managing packages include:

- gerbil pkg install <package-url> - installs a package from a specified Git repository.
- gerbil pkg update <package-name|all> - updates one or all installed packages.
- gerbil pkg list - lists all installed packages.
- gerbil pkg search <keyword> - searches configured package directories for packages matching a keyword.

A key feature of **gxpkg** is that packages are sourced directly from public Git repositories on providers like GitHub, GitLab, or Bitbucket. For a repository to be recognized as a Gerbil package, it must contain a **gerbil.pkg** manifest file that declares the package's namespace and dependencies, and a **build.ss** script that defines how to compile the package. 

This direct-from-source model has significant security implications so I manually inspect the source code for 3rd party packages that I use. The **build.ss** script is not sandboxed and runs with the user's privileges.

#### Discovering Packages

Package discovery in Gerbil is facilitated through package directories. These are themselves Git repositories containing a package-list file that maps package names to their repository URLs and descriptions. By default **gxpkg** is configured to search the "Mighty Gerbils" directory, which contains packages developed and maintained by the Gerbil Core Team. 

The primary community-curated package list can be found at **github.com/vyzo/gerbil-directory**. Developers can add additional directories using the 
**gerbil pkg dir -a <directory-repo-or-url>** command, allowing for the creation of private or specialized package collections.

## This Book's Code Examples as a Specialized Gerbil Package Collection

TBD
TBD: configure book examples as a community curated package....
TBD

## Core Gerbil Toolkits Used in this Book: :std/net/request and :std/text/json

Many examples in this book rely on **:std/net/request** and **:std/text/json** so we will give a brief overview here.

The foundation for all network communication in Gerbil is the **:std/net/request** library that is  a comprehensive HTTP client built into the standard library. This module obviates the need for a third-party HTTP client for most use cases. Its key features are:

- Full Method Support: Dedicated procedures for all standard HTTP methods (http-get, http-post, http-put, http-delete, etc.), plus an http-any for custom methods.
- Rich Request Customization: Keyword arguments for setting custom headers, URL parameters, cookies, authentication credentials, and the request body.
- Secure Communication: Integrated SSL/TLS context management for HTTPS requests, using the system's certificate authorities for verification by default. 
- Introspective Response Objects: Requests return a request object that provides access to the status code, headers, and response body in various formats (raw bytes, text, or parsed JSON).

This library is the sole tool required to interact with any RESTful API, from the complex and we will in later chapters use it to access commercial services offered by OpenAI and Google Gemini as well as locally hosted Large Language Models (LLMs) using Ollama.

For handling the responses from web services we will use the Data Interchange Layer package **:std/text/json**.

Virtually all modern web APIs use JSON as their data interchange format. Gerbil provides a canonical, flexible, and efficient JSON processing library in **:std/text/json**. This module is the essential counterpart to **:std/net/request**, handling the serialization of Scheme data into JSON strings for request bodies and the parsing of JSON responses back into Scheme objects.
