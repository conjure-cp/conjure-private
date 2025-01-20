
rm -rf conjure-output
conjure solve 00.essence --output-format=json --solutions-in-one-file --number-of-solutions=all --line-width=50 --copy-solutions=no --channelling=no --responses=1,1
mv conjure-output/model000001.solutions.json 1.sols

rm -rf conjure-output
conjure solve 00.essence --output-format=json --solutions-in-one-file --number-of-solutions=all --line-width=50 --copy-solutions=no --channelling=no --responses=2,2
mv conjure-output/model000001.solutions.json 2.sols

rm -rf conjure-output
conjure solve 00.essence --output-format=json --solutions-in-one-file --number-of-solutions=all --line-width=50 --copy-solutions=no --channelling=no --responses=3,3
mv conjure-output/model000001.solutions.json 3.sols

rm -rf conjure-output
conjure solve 00.essence --output-format=json --solutions-in-one-file --number-of-solutions=all --line-width=50 --copy-solutions=no --channelling=no --responses=4,4
cp conjure-output/model000001.solutions.json 4.sols

cat conjure-output/model000001.solutions.json
diff 1.sols 2.sols
diff 1.sols 3.sols
diff 1.sols 4.sols
diff 2.sols 3.sols
diff 2.sols 4.sols
diff 3.sols 4.sols

rm -rf conjure-output *.sols
