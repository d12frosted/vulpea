#!/bin/bash
# Compare temp-buffer vs find-file parsing methods

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"

NOTES_COUNT="${1:-1000}"
NOTES_DIR="$SCRIPT_DIR/bench-output/notes"
DB_FILE_TEMP="$SCRIPT_DIR/bench-output/temp-buffer.db"
DB_FILE_FIND="$SCRIPT_DIR/bench-output/find-file.db"

echo "========================================"
echo "Parse Method Comparison Benchmark"
echo "========================================"
echo "Notes:     $NOTES_COUNT"
echo "Directory: $NOTES_DIR"
echo "========================================"
echo

# Generate notes if needed
if [ ! -d "$NOTES_DIR" ] || [ $(find "$NOTES_DIR" -name "*.org" | wc -l) -ne "$NOTES_COUNT" ]; then
    echo "Generating $NOTES_COUNT notes..."
    eldev -p exec \
        "(progn \
           (add-to-list 'load-path \"$SCRIPT_DIR\") \
           (require 'vulpea-bench-generate) \
           (vulpea-bench-clean-directory \"$NOTES_DIR\") \
           (vulpea-bench-generate-notes \"$NOTES_DIR\" $NOTES_COUNT t))" \
        2>&1 | grep -E "(Generating|Generated|notes)"
    echo
fi

# Benchmark temp-buffer method
echo "========================================"
echo "METHOD 1: temp-buffer (fast)"
echo "========================================"
rm -f "$DB_FILE_TEMP"

eldev -p exec \
    "(progn \
       (add-to-list 'load-path \"$SCRIPT_DIR\") \
       (require 'vulpea-bench) \
       (setq vulpea-db-parse-method 'temp-buffer) \
       (vulpea-bench-sync \"$NOTES_DIR\" \"$DB_FILE_TEMP\"))" \
    2>&1 | grep -E "(===|Notes directory|Database|Full sync|Files|Throughput|Average)" | grep -v ">>>"

echo

# Benchmark find-file method
echo "========================================"
echo "METHOD 2: find-file (correct)"
echo "========================================"
rm -f "$DB_FILE_FIND"

eldev -p exec \
    "(progn \
       (add-to-list 'load-path \"$SCRIPT_DIR\") \
       (require 'vulpea-bench) \
       (setq vulpea-db-parse-method 'find-file) \
       (vulpea-bench-sync \"$NOTES_DIR\" \"$DB_FILE_FIND\"))" \
    2>&1 | grep -E "(===|Notes directory|Database|Full sync|Files|Throughput|Average)" | grep -v ">>>"

echo
echo "========================================"
echo "Comparison complete!"
echo "========================================"
