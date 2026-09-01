#!/usr/bin/env bash

set -uo pipefail

compile_log="$(mktemp -t emacs-qq-compile.XXXXXX)"
test_log="$(mktemp -t emacs-qq-test.XXXXXX)"
trap 'rm -f "$compile_log" "$test_log"' EXIT

if ! eask recompile >"$compile_log" 2>&1; then
  echo "emacs-qq: byte compilation failed" >&2
  tail -n 200 "$compile_log" >&2
  exit 1
fi

emacs_version="$(emacs -Q --batch --eval '(princ emacs-version)')"
elpa_dir=".eask/$emacs_version/elpa"
if [[ ! -d "$elpa_dir" ]]; then
  echo "emacs-qq: missing Eask package directory $elpa_dir" >&2
  exit 1
fi

load_args=(-L "$PWD")
while IFS= read -r -d '' directory; do
  load_args+=(-L "$directory")
done < <(find "$elpa_dir" -mindepth 1 -maxdepth 1 -type d -print0)

if (( $# > 0 )); then
  test_files=("$@")
else
  test_files=(test/*.el)
fi
test_args=()
for test_file in "${test_files[@]}"; do
  test_args+=(-l "$test_file")
done

# Eask's test command reloads package archive metadata and can block before
# ERT starts.  Recompilation above has already prepared the exact dependency
# tree, so run the batch directly against that local tree.
emacs -Q --batch "${load_args[@]}" -l ert "${test_args[@]}" \
  --eval '(ert-run-tests-batch-and-exit)' >"$test_log" 2>&1
status=$?

# ERT treats a test-level `quit' as neither expected nor unexpected and may
# still exit zero.  That is never a successful batch for this project.
if (( status == 0 )) \
  && grep -Eq '^[[:space:]]+(QUIT|ABORTED)[[:space:]]' "$test_log"; then
  status=1
fi

if (( status == 0 )); then
  grep -E '^(Running |Ran )' "$test_log"
else
  echo "emacs-qq: ERT failed" >&2
  grep -E '^(Running |Ran |Test .* condition:|[[:space:]]+(FAILED|QUIT|ABORTED)[[:space:]])' \
    "$test_log" >&2 || true
  echo "emacs-qq: diagnostic tail" >&2
  tail -n 120 "$test_log" >&2
fi

exit "$status"
