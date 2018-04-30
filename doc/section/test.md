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
