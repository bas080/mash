# Roadmap

## Deprecate once flag and replace with `require-once`

This allows for more granular control over when something should be required
regardless of the fact if it has been required before.

## Better document the DSL

Create a separate paragraph for each type of require statement.

- require-run
- require
- require-once

## Exit with non zero when command not succesfully run

Currently when a command fails it does not exit with an error code.
