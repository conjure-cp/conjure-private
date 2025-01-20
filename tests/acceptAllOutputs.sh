#!/bin/bash

set -o errexit
set -o nounset

tests/custom/acceptOutput.sh ::: $(find tests/custom -type d)
tests/exhaustive/acceptOutput.sh ::: $(find tests/exhaustive -type d)
tests/parse_print/acceptOutput.sh ::: $(find tests/parse_print -type d)