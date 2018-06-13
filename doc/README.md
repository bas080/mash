# Mash

[![Build Status](https://travis-ci.org/bas080/Mash.svg?branch=master)](https://travis-ci.org/bas080/Mash)

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

require ./doc/section/getting-started.md

require ./doc/section/reason.md

require ./doc/section/usage.md

require ./doc/section/installation.md

require ./doc/section/test.md

require ./doc/section/projects.md

require ./doc/section/roadmap.md

require ./doc/section/license.md
