#!/bin/bash
#
# Runs one or more tests, sharing the output cache among them.
#
# Usage:
#    hrepl/tests/run_tests.sh $TARGETS


set -ueo pipefail
set -x

# Build tests, including the hrepl binary, with the default parallelism
bazel build "$@"

OUTPUT="$(mktemp -d)"
trap "bazel --output_base=$OUTPUT/output-base clean --expunge && rm -rf $OUTPUT" EXIT

# Now run each test in sequence, using the same output directory to share the workspace
# and cache:
bazel test -j 1 \
    --test_env=PATH \
    --test_env=HREPL_TEST_CLIENT="$PWD" \
    --test_env=HREPL_TEST_OUTPUT="$OUTPUT" \
    --test_output=streamed \
    -k \
    "$@"
