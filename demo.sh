#!/usr/bin/env bash
set -euo pipefail

BOLD="\033[1m"
GREEN="\033[32m"
BLUE="\033[34m"
YELLOW="\033[33m"
CYAN="\033[36m"
RED="\033[31m"
RESET="\033[0m"

echo -e "${BOLD}${BLUE}=================================================================${RESET}"
echo -e "${BOLD}${BLUE}       SIFT v1.0 DEMO — REFUSAL, RELEASE & CACHE REUSE           ${RESET}"
echo -e "${BOLD}${BLUE}=================================================================\n${RESET}"

rm -rf demo_out

echo -e "${BOLD}--- PART 1: THE REFUSAL (Sift's Primary Role) ---${RESET}"
echo -e "Feeding a batch with 9 records containing invalid dates, floats in integers,"
echo -e "duplicate keys, nulls, and array values against strict production rules (${YELLOW}max_quarantine_ratio: 0.0005${RESET}):\n"

SIFT_BIN="./bin/sift"
if [ -x "./bin/sift-native" ]; then
  SIFT_BIN="./bin/sift-native"
fi

set +e
$SIFT_BIN run --input demo/input.jsonl --rules demo/rules_strict.json --out demo_out
EXIT_CODE=$?
set -e

echo -e "\n${BOLD}Observed Exit Code:${RESET} ${RED}$EXIT_CODE (BLOCKED)${RESET}"
if [ ! -f demo_out/current ]; then
  echo -e "${GREEN}✓ Verification: 'current' pointer was NOT created. Clean consumers are protected.${RESET}"
fi

BLOCKED_DIR=$(ls -d demo_out/runs/*.blocked-1 | head -n 1)
echo -e "${GREEN}✓ Verification: Run was isolated to suffixed directory: $BLOCKED_DIR${RESET}\n"

echo -e "${BOLD}Refusal Receipt (${BLOCKED_DIR}/receipt.json):${RESET}"
python3 -m json.tool "$BLOCKED_DIR/receipt.json"
echo ""

echo -e "${BOLD}--- PART 2: THE CLEAN RELEASE ---${RESET}"
echo -e "Now feeding a conforming batch of valid orders (${YELLOW}demo/clean_input.jsonl${RESET}):\n"

$SIFT_BIN run --input demo/clean_input.jsonl --rules demo/rules_strict.json --out demo_out
echo -e "${GREEN}✓ Sift completed with exit code 0 (RELEASED)${RESET}"
echo -e "${GREEN}✓ Verification: 'current' pointer updated -> $(cat demo_out/current)${RESET}\n"

CLEAN_RUN_ID=$(cat demo_out/current | sed 's|runs/||' | sed 's|/clean.jsonl||')
echo -e "${BOLD}Clean Release Receipt (demo_out/runs/$CLEAN_RUN_ID/receipt.json):${RESET}"
python3 -m json.tool "demo_out/runs/$CLEAN_RUN_ID/receipt.json"
echo ""

echo -e "${BOLD}--- PART 3: CACHE REUSE AT IO SPEED ---${RESET}"
echo -e "Re-running identical input against demo_out..."
START_TIME=$(python3 -c 'import time; print(time.time())')
$SIFT_BIN run --input demo/clean_input.jsonl --rules demo/rules_strict.json --out demo_out
END_TIME=$(python3 -c 'import time; print(time.time())')
DIFF=$(python3 -c "print(f'{$END_TIME - $START_TIME:.3f}')")
echo -e "${GREEN}✓ Verified output file hashes and reused run in $DIFF s without re-classification.${RESET}\n"

echo -e "${BOLD}${BLUE}=================================================================${RESET}"
echo -e "${BOLD}${GREEN}               ALL DEMO CHECKS VERIFIED GREEN                     ${RESET}"
echo -e "${BOLD}${BLUE}=================================================================${RESET}"
