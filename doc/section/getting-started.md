# Getting Started

Ever wanted to have a big file be split up into several files. Maybe you're
writing a book, or maybe you want to concatenate in very specific manner. Mash
allows you to do this by defining a `require <file>` at the start of line.

Mash knows if something has been required before. When encountering a require
statement that was already required, it will by default require it unless the
`-o` or `--once` flag is defined.

That is all you need to know. Enjoy `mash` and don't abuse it.
