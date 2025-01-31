#!/bin/bash

set -o errexit
set -o nounset

for TESTCASE in $*; do
    if [ -d "${TESTCASE}" ] && [ -f "${TESTCASE}/run.sh" ] ; then
        echo "Accepting the output of ${TESTCASE}"
        touch "${TESTCASE}"/stdout "${TESTCASE}"/stderr
        cp "${TESTCASE}"/stdout "${TESTCASE}"/stdout.expected
        cp "${TESTCASE}"/stderr "${TESTCASE}"/stderr.expected
        find \
            "${TESTCASE}"/stdout \
            "${TESTCASE}"/stderr \
            "${TESTCASE}"/stdout.expected \
            "${TESTCASE}"/stderr.expected \
            -size 0 \
            -exec rm {} \;
    fi
done
