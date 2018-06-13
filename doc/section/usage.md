# Usage

```
require-run ./mash.hs --help
```

```bash
cat doc/README.md | mash - > README.md

# or

mash doc/README.md > README.md
```

You can also have the stdout of a command substitute the require statement. To
achieve this we use a different require statement.

```
  require-run mash --help
```

The stderr is not written. In case you do want to write the error, you have to
pipe the stderr to the stdout. `mash --help 2>&1`

