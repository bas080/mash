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
