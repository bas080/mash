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

After that make sure you are in mash's project root and do the following

```sh
./script/install
```

This will place mash's bin file in `~/.local/bin/`.  Stack adds this directory
to the $PATH. You can also place the mash bin in your own bin directory.
