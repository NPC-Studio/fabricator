# Fabricator of Merriments
---

A WIP replacement for the GameMaker Studio runtime, used by
[Fields of Mistria](https://www.fieldsofmistria.com/).

Written to be compatilbe *enough* with GML and GameMaker to make it possible to port a complex
project, but is explicitly NOT aiming for perfect compatibility.

Includes a working Parser, Compiler, and VM for GML that compiles GML faster than GameMaker's VM
mode and runs code faster than GameMaker's YYC mode. A new (in-development) FML language mode can be
freely mixed with GML and allows for modern features such as lexical scoping and closures.

Compatibility with the higher level parts of GameMaker such as the stdlib, graphics, input, and
sound is extremely WIP.

## License

Everything in this repository is licensed under any of:

* MIT license [LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT
* MIT-0 license [LICENSE-MIT-0](LICENSE-MIT-0) or http://opensource.org/licenses/MIT-0
* Creative Commons CC0 1.0 Universal Public Domain Dedication [LICENSE-CC0](LICENSE-CC0)
  or https://creativecommons.org/publicdomain/zero/1.0/

at your option.
