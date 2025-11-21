#!/usr/bin/env bash
# Vulpea Benchmark Runner
#
# Usage:
#   ./run-benchmarks.sh [OPTIONS]
#
# Options:
#   -n, --notes COUNT      Number of notes to generate (default: 1000)
#   -s, --sync             Run sync benchmark
#   -q, --query            Run query benchmarks
#   -e, --extract          Run extraction benchmark
#   -a, --all              Run all benchmarks (default)
#   -c, --clean            Clean benchmark output before running
#   -h, --help             Show this help

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
BENCH_OUTPUT="$SCRIPT_DIR/bench-output"
NOTES_DIR="$BENCH_OUTPUT/notes"
DB_FILE="$BENCH_OUTPUT/vulpea-bench.db"

# Default options
NOTE_COUNT=1000
RUN_SYNC=0
RUN_QUERY=0
RUN_EXTRACT=0
RUN_ALL=0
CLEAN=0

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        -n|--notes)
            NOTE_COUNT="$2"
            shift 2
            ;;
        -s|--sync)
            RUN_SYNC=1
            shift
            ;;
        -q|--query)
            RUN_QUERY=1
            shift
            ;;
        -e|--extract)
            RUN_EXTRACT=1
            shift
            ;;
        -a|--all)
            RUN_ALL=1
            shift
            ;;
        -c|--clean)
            CLEAN=1
            shift
            ;;
        -h|--help)
            head -n 13 "$0" | tail -n +2 | sed 's/^# //'
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            echo "Use --help for usage information"
            exit 1
            ;;
    esac
done

# If no specific benchmark selected, run all
if [[ $RUN_SYNC -eq 0 && $RUN_QUERY -eq 0 && $RUN_EXTRACT -eq 0 ]]; then
    RUN_ALL=1
fi

# Clean output if requested
if [[ $CLEAN -eq 1 ]]; then
    echo "Cleaning benchmark output..."
    rm -rf "$BENCH_OUTPUT"
fi

# Create output directory
mkdir -p "$BENCH_OUTPUT"
mkdir -p "$NOTES_DIR"

echo "========================================"
echo "Vulpea Performance Benchmark"
echo "========================================"
echo "Notes:     $NOTE_COUNT"
echo "Directory: $NOTES_DIR"
echo "Database:  $DB_FILE"
echo "========================================"
echo ""

# Generate notes if directory is empty
if [[ $(find "$NOTES_DIR" -name "*.org" | wc -l) -lt "$NOTE_COUNT" ]]; then
    echo "Generating $NOTE_COUNT notes..."
    cd "$PROJECT_ROOT"
    eldev -dtT exec "(progn
      (add-to-list 'load-path \"$SCRIPT_DIR\")
      (require 'vulpea-bench-generate)
      (vulpea-bench-generate-notes \"$NOTES_DIR\" $NOTE_COUNT t))"
    echo ""
fi

# Run sync benchmark
if [[ $RUN_SYNC -eq 1 || $RUN_ALL -eq 1 ]]; then
    echo "Running sync benchmark..."
    cd "$PROJECT_ROOT"
    eldev -dtT exec "(progn
      (add-to-list 'load-path \"$SCRIPT_DIR\")
      (require 'vulpea-bench)
      (vulpea-bench-sync \"$NOTES_DIR\" \"$DB_FILE\"))"
    echo ""
fi

# Run extraction benchmark
if [[ $RUN_EXTRACT -eq 1 || $RUN_ALL -eq 1 ]]; then
    echo "Running extraction benchmark..."
    SAMPLE_SIZE=$((NOTE_COUNT < 100 ? NOTE_COUNT : 100))
    cd "$PROJECT_ROOT"
    eldev -dtT exec "(progn
      (add-to-list 'load-path \"$SCRIPT_DIR\")
      (require 'vulpea-bench)
      (vulpea-bench-extraction \"$NOTES_DIR\" $SAMPLE_SIZE))"
    echo ""
fi

# Run query benchmarks
if [[ $RUN_QUERY -eq 1 || $RUN_ALL -eq 1 ]]; then
    echo "Running query benchmarks..."
    cd "$PROJECT_ROOT"
    eldev -dtT exec "(progn
      (add-to-list 'load-path \"$SCRIPT_DIR\")
      (require 'vulpea-bench)

      ;; Query all notes
      (vulpea-bench-query \"$DB_FILE\" \"all notes\"
        (lambda () (vulpea-db-query)))

      ;; Query by single tag
      (vulpea-bench-query \"$DB_FILE\" \"by single tag\"
        (lambda () (vulpea-db-query-by-tags-some '(\"project\"))))

      ;; Query by multiple tags
      (vulpea-bench-query \"$DB_FILE\" \"by multiple tags\"
        (lambda () (vulpea-db-query-by-tags-every '(\"project\" \"active\"))))

      ;; Query by level
      (vulpea-bench-query \"$DB_FILE\" \"file-level only\"
        (lambda () (vulpea-db-query-by-level 0)))

      ;; Search by title
      (vulpea-bench-query \"$DB_FILE\" \"title search\"
        (lambda () (vulpea-db-search-by-title \"Lorem\"))))"
    echo ""
fi

echo "========================================"
echo "Benchmark complete!"
echo "========================================"
