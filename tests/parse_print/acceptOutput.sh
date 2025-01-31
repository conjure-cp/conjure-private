#!/bin/bash

set -o errexit
set -o nounset

for TESTCASE in $*; do
    if [ -d "${TESTCASE}" ]; then
        NUM=$(ls "${TESTCASE}/"*.essence 2> /dev/null | grep -v disabled.essence | wc -l | tr -d ' ')
        if [ "$NUM" -eq "1" ]; then
            echo "Accepting the output of ${TESTCASE}"
            touch "${TESTCASE}"/stdout "${TESTCASE}"/model.json "${TESTCASE}"/stderr "${TESTCASE}"/typecheck
            cp "${TESTCASE}"/stdout "${TESTCASE}"/stdout.expected
            cp "${TESTCASE}"/model.json "${TESTCASE}"/model.expected.json
            cp "${TESTCASE}"/stderr "${TESTCASE}"/stderr.expected
            cp "${TESTCASE}"/typecheck "${TESTCASE}"/typecheck.expected
            find \
                "${TESTCASE}"/stdout \
                "${TESTCASE}"/model.json \
                "${TESTCASE}"/stderr \
                "${TESTCASE}"/typecheck \
                "${TESTCASE}"/stdout.expected \
                "${TESTCASE}"/model.expected.json \
                "${TESTCASE}"/stderr.expected \
                "${TESTCASE}"/typecheck.expected \
                -size 0 \
                -exec rm {} \;
        fi
    fi
done
