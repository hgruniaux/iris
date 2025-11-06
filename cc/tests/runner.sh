#!/bin/bash

CPP="clang -E -P" # C Preprocessor
ASM="clang" # Assembler

# Parse arguments
POSITIONAL_ARGS=()

while [[ $# -gt 0 ]]; do
  case $1 in
    --skip-fail)
      SKIP_FAIL=1
      shift # past argument
      ;;
    --skip-pass)
      SKIP_PASS=1
      shift # past argument
      ;;
    --stop-on-fail)
      STOP_ON_FAIL=1
      shift # past argument
      ;;
    -d|--debug)
      DEBUG=1
      shift # past argument
      ;;
    --dump-asm)
      DUMP_ASM=1
      shift # past argument
      ;;
    -t|--only-typing)
      TYPING_ONLY=1
      shift # past argument
      ;;
    -i|--only-ir)
      IR_ONLY=1
      shift # past argument
      ;;
    -*|--*)
      echo "Unknown option $1"
      exit 1
      ;;
    *)
      POSITIONAL_ARGS+=("$1") # save positional arg
      shift # past argument
      ;;
  esac
done

if [ ${#POSITIONAL_ARGS[@]} -ne 1 ]; then
  echo "Error: Exactly one positional argument (path to tested binary) is required."
  exit 1
fi

TESTED_BIN="${POSITIONAL_ARGS[0]}"

temp_dir=$(mktemp -d)
mkdir -p "$temp_dir"

total_tests=0
passed_tests=0
failed_tests=0

function _fail {
  failed_tests=$((failed_tests + 1))
  total_tests=$((total_tests + 1))

  if [ ! -z "$STOP_ON_FAIL" ]; then
    echo -e "[ \e[31mFAILED\e[0m ] \e[1m$1\e[0m $2"
    echo "Stopping on first failure as per --stop-on-fail option."
    exit 1
  fi

  if [ ! -z "$SKIP_FAIL" ]; then
    return
  fi

  echo -e "[ \e[31mFAILED\e[0m ] \e[1m$1\e[0m $2"
}

function _pass {
  passed_tests=$((passed_tests + 1))
  total_tests=$((total_tests + 1))

  if [ ! -z "$SKIP_PASS" ]; then
    return
  fi

  echo -e "[ \e[32mPASSED\e[0m ] \e[1m$1\e[0m"
}

function _run_test_impl {
  local test_file="$1"
  local expected_output_file="$2"
  local test_name="$test_file"

  local preprocessed_file="$temp_dir/${test_name%.c}_preprocessed.c"
  local asm_file="$temp_dir/${test_name%.c}.s"

  $CPP "$test_file" -o "$preprocessed_file"
  if [ $? -ne 0 ]; then
    _fail "$test_name" "(preprocessing failed)"
    return 1
  fi

  # When debugging is enabled, do not redirect output to /dev/null
  local compil_cmd="$TESTED_BIN $preprocessed_file -o $asm_file"
  if [ ! -z "$DEBUG" ]; then
    $compil_cmd
  else
    $compil_cmd > /dev/null 2>&1
  fi

  local exit_code=$?
  if [ $exit_code -ne 0 ]; then
    _fail "$test_name" "(compilation failed, exit code: $exit_code)"
    return 1
  fi

  # When debugging is enabled, do not redirect output to /dev/null
  local asm_cmd="$ASM $asm_file -o $temp_dir/${test_name%.c}"
  if [ ! -z "$DEBUG" ]; then
    $asm_cmd
  else
    $asm_cmd > /dev/null 2>&1
  fi

  if [ $? -ne 0 ]; then
    if [ ! -z "$DUMP_ASM" ]; then
      echo "Dumping assembly for debugging:"
      cat "$asm_file"
    fi
    _fail "$test_name" "(assembly failed)"
    return 1
  fi

  local output_file="$temp_dir/${test_name%.c}_output.txt"
  "$temp_dir/${test_name%.c}" > "$output_file" 2>&1
  if [ $? -ne 0 ]; then
    _fail "$test_name" "(execution failed)"
    return 1
  fi

  # When debugging is enabled, show diff output
  if [ ! -z "$DEBUG" ]; then
    diff "$output_file" "$expected_output_file"
    if [ $? -ne 0 ]; then
      _fail "$test_name" "(output mismatch)"
      return 1
    fi
  else
    if ! diff -q "$output_file" "$expected_output_file" > /dev/null 2>&1; then
      _fail "$test_name" "(output mismatch)"
      return 1
    fi
  fi

  _pass "$test_name"
  return 0
}

function run_tcc_tests {
  mkdir -p "$temp_dir/tcc"

  for test_file in tcc/*.c; do
    local expected_output_file="${test_file%.c}.expect"
    _run_test_impl "$test_file" "$expected_output_file"
  done

  echo "Report: $passed_tests/$total_tests tests passed."
}

function run_c_testsuite_tests {
  mkdir -p "$temp_dir/c-testsuite"

  for test_file in c-testsuite/*.c; do
    local expected_output_file="${test_file}.expected"
    _run_test_impl "$test_file" "$expected_output_file"
  done

  echo "Report: $passed_tests/$total_tests tests passed."
}


run_c_testsuite_tests
# rm -rf "$temp_dir"
