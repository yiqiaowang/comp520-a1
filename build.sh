#/bin/bash

# TODO: Specify the compiler build commands
#
# You *MUST* replace the following commands with the
# commands for building your compiler.

(cd src && stack install --local-bin-path .)
