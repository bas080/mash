# Mash

Mash is a tool to replace "require statements" in files with the contents of
another.

```markdown
1 # Data structures
2
3 require ./chapter/linked-lists.md  < 1 ## Linked lists
                                       2
                                       3
4
5
```

# Getting Started

Ever wanted to have a big file be split up into several files. Maybe you're
writing a book, or maybe you want to concatenate in very specific manner. Mash
allows you to do this by defining a `require <file>` at the start of line.

Mash knows if something has been required before. When encountering a require
statement that was already required, it will by default require it unless the
`-o` or `--once` flag is defined.

That is all you need to know. Enjoy `mash` and don't abuse it.

# Reason

I needed a tool for building my bash scripts. I wanted to split my bash script
up in files and then `source` whenever I needed the functions or variables
defined in that file.

This however only works when the user either is in the correct directory or if
a full path is used in the source statements. This would require convention or
configuration.

Another reason is that it is nice to have one file be executable and working
while at the same time the source code is split across several files.

Even though I use Mash to write and build bash scripts, that doesn't mean it
can't be used for all types of things.

# Usage

`mash file [-o|--once]`

  **file** - Either a path of a file or `-` when receiving text from stdin

  **-o | --once** - Will only require a file once. Subsequent duplicate require
  statements are removed and not replaced with the reference text content

```bash
cat doc/README.md | mash - > README.md

# or

mash doc/README.md > README.md
```

# Installation

First you need to install stack.

As mentioned in the [stack install
guide](https://docs.haskellstack.org/en/stable/install_and_upgrade/) you can
use the following command to install stack.

```bash
curl -sSL https://get.haskellstack.org/ | sh
  # or
wget -qO- https://get.haskellstack.org/ | sh
```

After that make sure you are in mash's root directory and type

`stack install`

This will place a bin file in `~/.local/bin/mash`. Make sure to have
`$HOME/.local/bin` in your `$PATH`.

# Projects

Are you using mash in your project? Consider putting your project on this list.

## Generating `./README.md`

- https://github.com/bas080/pie
- https://github.com/bas080/knest
- https://github.com/bas080/mash

# Roadmap

## Support get requests for getting content.

How nice would it be to reference some external text and have it be placed in
your README.md. Maybe your project requires some setup. Just point to the
installation instructions file of another project and let mash get it and
substitute the require statement.

# License

Copyright Bas Huis (c) 2018

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Author name here nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

