#!/bin/bash

set -o errexit
set -o nounset
shopt -s nullglob # for the loop

for TESTCASE in $*; do
    if [ -d "${TESTCASE}" ]; then
        NUM_ESSENCE=$(ls "${TESTCASE}/"*.essence 2> /dev/null | wc -l | tr -d ' ')
        NUM_INVALID=$(ls "${TESTCASE}/invalid" 2> /dev/null | wc -l | tr -d ' ')
        if [ "$NUM_ESSENCE" -eq "1" ] && [ "$NUM_INVALID" -eq 0 ]; then
            echo "Accepting the output of ${TESTCASE}"
            rm -rf "${TESTCASE}"/expected
            mkdir -p "${TESTCASE}"/expected
            for file in "${TESTCASE}"/outputs/*.eprime "${TESTCASE}"/outputs/*.solution "${TESTCASE}"/outputs/*.eprime-param ; do
                cp $file "${TESTCASE}"/expected/
            done
            parallel --no-notice "[ -f {} ] && (cat {} | grep -v '\\$' > {}.temp ; mv {}.temp {})" \
                ::: "${TESTCASE}"/expected/*.eprime
        fi
    fi
done
