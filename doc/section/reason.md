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
