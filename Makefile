
SHELL := /bin/bash

# these are default values
# override by calling the makefile like so: "GHC_VERSION=9.2 make"
export GHC_VERSION?=9.4
export BIN_DIR?=${HOME}/.local/bin
export LIB_DIR?=${BIN_DIR}/lib
export PATH := $(BIN_DIR):$(PATH)
export CI?=false
export BUILD_TESTS?=false
export COVERAGE?=false
export LIMIT_TIME?=10

.PHONY: install
install:
	@echo "Using GHC version ${GHC_VERSION} (major version)"
	@echo "Set the environment variable GHC_VERSION to change this location."
	@echo "For example: \"GHC_VERSION=9.4 make install\""
	@echo "Supported versions:  9.0, 9.2, 9.4"
	@echo ""
	@echo "Installing executables to ${BIN_DIR}"
	@echo "Add this directory to your PATH."
	@echo ""
	@echo "Installing shared libraries to ${LIB_DIR}"
	@echo "Add this directory to your LD_LIBRARY_PATH."
	@echo ""
	@echo "You can set the environment variables BIN_DIR and LIB_DIR to change these locations"
	@echo "For example: \"BIN_DIR=your/preferred/path LIB_DIR=/usr/local/lib make install\""
	@echo ""
	@mkdir -p ${BIN_DIR}
	@mkdir -p ${LIB_DIR}
	@echo Using Stack file: etc/hs-deps/stack-${GHC_VERSION}.yaml
	@if ${BUILD_TESTS} ; then echo "BUILD_TESTS=true"; fi
	@if ${CI} ; then echo "CI=true"; fi
	@bash etc/build/install-stack.sh
	@make stack.yaml
	@if  [ ${GHC_VERSION} == "head" ] ; then\
		stack --local-bin-path ${BIN_DIR} setup --resolver nightly;\
	else\
		stack --local-bin-path ${BIN_DIR} setup;\
	fi
	@bash etc/build/version.sh
	@stack runhaskell etc/build/gen_Operator.hs
	@stack runhaskell etc/build/gen_Expression.hs
	@bash etc/build/install.sh
	@etc/build/copy-conjure-branch.sh
	@cp -r etc/savilerow/* ${BIN_DIR}
	@echo "Copied savilerow to ${BIN_DIR}"
	@echo
	@${BIN_DIR}/conjure --version
	@${BIN_DIR}/savilerow -help | head -n1
	@echo
	@echo

# mainly for CI
.PHONY: installdeps
installdeps:
	bash etc/build/install-stack.sh
	make stack.yaml
	stack --local-bin-path ${BIN_DIR} setup;
	stack build --only-dependencies

.PHONY: test
test:
	@if ${COVERAGE}; then \
		stack test --coverage --test-arguments '--hide-successes --limit-time ${LIMIT_TIME}';\
		stack hpc report conjure-cp $(find . -name conjure.tix);\
		ls .stack-work/install/*/*/*/hpc/combined/custom;\
	else\
		stack test --test-arguments '--hide-successes --limit-time ${LIMIT_TIME}';\
	fi

.PHONY: stack.yaml
stack.yaml: etc/hs-deps/stack-${GHC_VERSION}.yaml
	@cp etc/hs-deps/stack-${GHC_VERSION}.yaml stack.yaml

.PHONY: preinstall
preinstall:
	@make stack.yaml
	@bash etc/build/version.sh
	@stack runhaskell etc/build/gen_Operator.hs
	@stack runhaskell etc/build/gen_Expression.hs

.PHONY: clean
clean:
	@bash etc/build/clean.sh

.PHONY: docs
docs:
	(cd docs; bash build.sh)

.PHONY: ghci
ghci:
	@cabal exec ghci -- -isrc -isrc/test           \
	    -idist/build/autogen                       \
	    -XFlexibleContexts                         \
	    -XFlexibleInstances                        \
	    -XMultiParamTypeClasses                    \
	    -XNoImplicitPrelude                        \
	    -XOverloadedStrings                        \
	    -XQuasiQuotes                              \
	    -XScopedTypeVariables                      \
	    -XTypeOperators                            \
	    -XLambdaCase                               \
	    -XMultiWayIf                               \
	    -fwarn-incomplete-patterns                 \
	    -fwarn-incomplete-uni-patterns             \
	    -fwarn-missing-signatures                  \
	    -fwarn-name-shadowing                      \
	    -fwarn-orphans                             \
	    -fwarn-overlapping-patterns                \
	    -fwarn-tabs                                \
	    -fwarn-unused-do-bind                      \
	    -fwarn-unused-matches                      \
	    -Wall                                      \
	    -Werror                                    \
	    `find src -name '*.hs' | grep -v 'Main.hs' | grep -v '\.#'`

.PHONY: hlint
hlint:
	-@hlint -r `find src -name '*.hs' | grep -v LogFollow` \
	    -i "Use camelCase" \
	    -i "Reduce duplication" \
	    -i "Use &&" \
	    -i "Use ++" \
	    -i "Redundant return" \
	    -i "Monad law, left identity"

# @etc/build/silent-wrapper.sh etc/build/install-glasgow-subgraph-solver.sh

.PHONY: solvers
solvers:
	@echo "Installing executables to ${BIN_DIR}"
	@echo "Add this directory to your PATH."
	@echo "Set the environment variable BIN_DIR to change this location."
	@echo "For example: \"BIN_DIR=your/preferred/path make install\""
	@echo ""
	@echo "Set the environment variable PROCESSES to specify the number of cores to use. Default is 1."
	@echo ""
	@echo "Dependencies: autoconf, cmake and gmp."
	@if [ `uname` == "Darwin" ]; then echo "You can run: 'brew install autoconf cmake gmp' to install them."; fi
	@echo ""
	@mkdir -p ${BIN_DIR}
	@rm -f make-solvers-*.stderr make-solvers-*.stdout > /dev/null 2> /dev/null
	@etc/build/silent-wrapper.sh etc/build/install-bc_minisat_all.sh
	@etc/build/silent-wrapper.sh etc/build/install-boolector.sh
	@etc/build/silent-wrapper.sh etc/build/install-cadical.sh
	@etc/build/silent-wrapper.sh etc/build/install-chuffed.sh
	# @etc/build/silent-wrapper.sh etc/build/install-gecode.sh
	@etc/build/silent-wrapper.sh etc/build/install-glucose.sh
	@etc/build/silent-wrapper.sh etc/build/install-kissat.sh
	@etc/build/silent-wrapper.sh etc/build/install-lingeling.sh
	@etc/build/silent-wrapper.sh etc/build/install-minion.sh
	@etc/build/silent-wrapper.sh etc/build/install-nbc_minisat_all.sh
	@etc/build/silent-wrapper.sh etc/build/install-open-wbo.sh
	@etc/build/silent-wrapper.sh etc/build/install-ortools.sh
	@etc/build/silent-wrapper.sh etc/build/install-minizinc.sh
	@etc/build/silent-wrapper.sh etc/build/install-yices.sh
	@etc/build/silent-wrapper.sh etc/build/install-z3.sh
	@etc/build/silent-wrapper.sh etc/build/install-runsolver.sh
	@if ls make-solvers-*.stderr make-solvers-*.stdout > /dev/null 2> /dev/null; then echo "At least one solver didn't build successfully."; exit 1; fi
