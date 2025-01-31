conjure solve --number-of-solutions=all --solutions-in-one-file --output-format=jsonstream *.essence
cat *.json | LC_ALL=C sort
rm -rf conjure-output *solutions*
