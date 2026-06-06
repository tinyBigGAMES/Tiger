#!/usr/bin/env bash
# Run Linux aarch64 ELFs built by Testbed under Docker (linux/arm64).
# Usage:
#   ./tests/run_linux_arm64.sh [output_dir]
# Default output_dir: tests/bin/output/linarm (relative to repo root)
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
OUT="${1:-$ROOT/tests/bin/output/linarm}"
IMAGE="${TIGER_LINUX_ARM64_IMAGE:-arm64v8/ubuntu:22.04}"
FAILED=0
PASSED=0
SKIPPED=0

if ! command -v docker >/dev/null 2>&1; then
  echo "ERROR: docker not found in PATH"
  exit 1
fi

if ! docker info >/dev/null 2>&1; then
  echo "ERROR: docker daemon not running"
  exit 1
fi

if [[ ! -d "$OUT" ]]; then
  echo "ERROR: output directory not found: $OUT"
  echo "Build Testbed with LINUXARM64 first (e.g. Testbed LINUXARM64 1)."
  exit 1
fi

run_elf() {
  local name="$1"
  local expect="${2:-0}"
  local path="$OUT/$name"
  if [[ ! -f "$path" ]]; then
    echo "SKIP  $name (not built)"
    SKIPPED=$((SKIPPED + 1))
    return 0
  fi
  chmod +x "$path" 2>/dev/null || true
  set +e
  local out
  out="$(docker run --rm --platform linux/arm64 \
    -v "$OUT:/work:ro" \
    "$IMAGE" \
    "/work/$name" 2>&1)"
  local code=$?
  set -e
  if [[ "$code" -eq "$expect" ]]; then
    echo "PASS  $name (exit $code)"
    PASSED=$((PASSED + 1))
  else
    echo "FAIL  $name (exit $code, expected $expect)"
    echo "$out"
    FAILED=$((FAILED + 1))
  fi
}

echo "Tiger Linux ARM64 Docker validation"
echo "  image:  $IMAGE"
echo "  output: $OUT"
echo ""

# Core smoke tests (names match Testbed output paths without extension)
run_elf Test_HelloWorld 0
run_elf Test_Factorial_WhileLoop 0
run_elf Test_CaseStatement 0
run_elf Test_GlobalVariables 0
run_elf Test_ManagedStrings 0
run_elf Test_Printf_Basic 0
run_elf Test_VariadicFunctions 0
run_elf Test_RuntimeMemory 0
run_elf Test_SEH 0
run_elf Test_StaticLinking 0
run_elf Test_DLLGeneration 0
run_elf Test_DynamicLoading 0

echo ""
echo "Results: $PASSED passed, $FAILED failed, $SKIPPED skipped"
if [[ "$FAILED" -gt 0 ]]; then
  exit 1
fi
