# Fabricator of Merriments
---

A WIP replacement for the GameMaker Studio compiler and runtime, used by
[Fields of Mistria](https://www.fieldsofmistria.com/).

This project is a bit different than other GameMaker runtime replacements in that it reads GMS2
project files and compiles and runs GML directly, rather than running GMS2 compiled bytecode. As
such, it can have a totally different compiler and VM design than GMS2.

Notably, the VM here is more similar to Lua's VM, register based rather than stack based and
designed for multiple return values, heap allocated closures, and coroutines.

Written to be compatible *enough* with GML and GameMaker to make it possible to port a large,
complex project, but is explicitly NOT aiming for perfect bug-for-bug compatibility.

Includes a working Parser, Compiler, and VM for GML that compiles GML faster than GameMaker's VM
mode and runs code faster than GameMaker's YYC mode. A new (in-development) FML language mode can
freely interoperate with GML and is a stricter, cleaner variant of GML with more modern features.

Provides an extremely rich FFI to Rust to replace GameMaker dll extensions.

Compatibility with GML (the language) is very good and the compiler and VM are fairly complete.
Compatibility with the higher level parts of GameMaker such as the stdlib, graphics, input, and
sound is a WIP.

This project does NOT aim to be a drop-in replacement for GameMaker Studio or a replacement for
the GameMaker Studio IDE in any way, it only allows running existing projects using an alternate
compiler and runtime. It will primarily be useful to projects that are straining at the boundaries
of what can be accomplished in vanilla GameMaker Studio and would like to move away from GameMaker's
abstractions or integrate new, complex APIs to custom engine code.

## License

Everything in this repository is licensed under any of:

* MIT license [LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT
* MIT-0 license [LICENSE-MIT-0](LICENSE-MIT-0) or http://opensource.org/licenses/MIT-0
* Creative Commons CC0 1.0 Universal Public Domain Dedication [LICENSE-CC0](LICENSE-CC0)
  or https://creativecommons.org/publicdomain/zero/1.0/

at your option.
