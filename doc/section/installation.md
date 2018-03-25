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
