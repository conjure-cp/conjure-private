
rm -rf conjure-output

conjure solve 00.essence --output-format=json --solutions-in-one-file --number-of-solutions=all --line-width=50 --copy-solutions=no --unnamed-symmetry-breaking=Complete-AllPermutations-Independently

# conjure solve 00.essence --output-format=json --solutions-in-one-file --number-of-solutions=all --line-width=50 --copy-solutions=no --unnamed-symmetry-breaking=full
# conjure solve 00.essence --output-format=json --solutions-in-one-file --number-of-solutions=all --line-width=50 --copy-solutions=no --unnamed-symmetry-breaking=Quick-Consecutive-Independently
# conjure solve 00.essence --output-format=json --solutions-in-one-file --number-of-solutions=all --line-width=50 --copy-solutions=no --unnamed-symmetry-breaking=Quick-Consecutive-Altogether
# conjure solve 00.essence --output-format=json --solutions-in-one-file --number-of-solutions=all --line-width=50 --copy-solutions=no --unnamed-symmetry-breaking=Quick-AllPairs-Independently
# conjure solve 00.essence --output-format=json --solutions-in-one-file --number-of-solutions=all --line-width=50 --copy-solutions=no --unnamed-symmetry-breaking=Quick-AllPairs-Altogether
# conjure solve 00.essence --output-format=json --solutions-in-one-file --number-of-solutions=all --line-width=50 --copy-solutions=no --unnamed-symmetry-breaking=Quick-AllPermutations-Independently
# conjure solve 00.essence --output-format=json --solutions-in-one-file --number-of-solutions=all --line-width=50 --copy-solutions=no --unnamed-symmetry-breaking=Quick-AllPermutations-Altogether
# conjure solve 00.essence --output-format=json --solutions-in-one-file --number-of-solutions=all --line-width=50 --copy-solutions=no --unnamed-symmetry-breaking=Complete-Consecutive-Independently
# conjure solve 00.essence --output-format=json --solutions-in-one-file --number-of-solutions=all --line-width=50 --copy-solutions=no --unnamed-symmetry-breaking=Complete-Consecutive-Altogether
# conjure solve 00.essence --output-format=json --solutions-in-one-file --number-of-solutions=all --line-width=50 --copy-solutions=no --unnamed-symmetry-breaking=Complete-AllPairs-Independently
# conjure solve 00.essence --output-format=json --solutions-in-one-file --number-of-solutions=all --line-width=50 --copy-solutions=no --unnamed-symmetry-breaking=Complete-AllPairs-Altogether
# conjure solve 00.essence --output-format=json --solutions-in-one-file --number-of-solutions=all --line-width=50 --copy-solutions=no --unnamed-symmetry-breaking=Complete-AllPermutations-Independently
# conjure solve 00.essence --output-format=json --solutions-in-one-file --number-of-solutions=all --line-width=50 --copy-solutions=no --unnamed-symmetry-breaking=Complete-AllPermutations-Altogether

cat conjure-output/model000001.solutions.json
rm -rf conjure-output
