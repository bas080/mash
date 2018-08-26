# mash

[![Build Status](https://travis-ci.org/bas080/mash.svg?branch=master)](https://travis-ci.org/bas080/mash)

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

Enjoy `mash` and don't abuse it.

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

```
Mash (C) Bas Huis

mashargs [OPTIONS] [FILE/URL]
  More flexible file concatenation

Common flags:
  -? --help     Display help message
  -V --version  Print version information

Mash helps you split up files and concat them easier

To output the generated file to stdout:
  mash ./doc/README.md
```

```bash
cat doc/README.md | mash - > README.md

# or

mash doc/README.md > README.md
```

You can also have the stdout of a command substitute the require statement. To
achieve this we use a different require statement.

```
  require-run ./mash.hs --help
```

The stderr is not written. In case you do want to write the error, you have to
pipe the stderr to the stdout. `mash --help 2>&1`


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

Mash is a stack script. The script is located at `./mash.hs` and is executable.
Make sure to add this script to your path by linking or copying it to a bin
directory.

# Test

Tests are writtin in bash. You can run the tests from the project directory by
typing the following in your terminal.

`./script/test`

Note that the tests require an internet connection in order to test the url
support.

The tests use a "snapshot" to check if mash is generating documentation
correctly. It uses the mash documentation to check if it does so. This means
that changing the mash docs results in failing tests. A minor inconvenience.

To update the snapshots simply prepend `SNAPSHOT=1`.

`SNAPSHOT=1 ./script/test`

# Projects

Are you using mash in your project? Consider putting your project on this list.

## Generating `./README.md`

- https://github.com/bas080/pie
- https://github.com/bas080/knest
- https://github.com/bas080/mash

# Roadmap

## Exit with non zero when command not successfully run

Currently when a command fails it does not exit with an error code. Read more
about
[ioError](http://hackage.haskell.org/package/base-4.11.1.0/docs/src/GHC.IO.Exception.html#ioError).

## Better document the DSL

Create a separate paragraph for each type of require statement.

- require-run
- require

# License

Copyright Bas Huis (c) 2018

Want to see the full [LICENSE](./LICENSE)?
