#!/bin/bash

set -o errexit
set -o nounset

rm -rf conjure-output *solutions*

for sym in Quick Complete; do
    for amount in Consecutive AllPairs AllPermutations; do
        for combine in Independently Altogether; do
            echo $sym-$amount-$combine
            conjure solve --number-of-solutions=all --solutions-in-one-file --output-format=jsonstream --unnamed-symmetry-breaking=$sym-$amount-$combine *.essence 2>&1
            cat *.json | LC_ALL=C sort
            rm -rf conjure-output *solutions*
        done
    done
done