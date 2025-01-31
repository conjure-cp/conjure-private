rm -rf conjure-output
conjure solve set_comprehension.essence --copy-solutions=no
cat conjure-output/*.eprime | grep -v '\$'
cat conjure-output/*.solution
rm -rf conjure-output