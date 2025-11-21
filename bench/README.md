# Vulpea Performance Benchmarks

Infrastructure for performance testing and benchmarking vulpea operations.

## Quick Start

```bash
# Run all benchmarks with 1,000 notes (default)
./bench/run-benchmarks.sh

# Run with 10,000 notes
./bench/run-benchmarks.sh --notes 10000

# Run only sync benchmark with 100,000 notes
./bench/run-benchmarks.sh --notes 100000 --sync

# Clean and rebuild with 1,000 notes
./bench/run-benchmarks.sh --notes 1000 --clean
```

## Components

### Note Generator (`vulpea-bench-generate.el`)

Fast generator for creating test notes:

```elisp
(require 'vulpea-bench-generate)

;; Generate 1000 notes
(vulpea-bench-generate-notes "/tmp/notes" 1000)

;; Generate with random headings
(vulpea-bench-generate-notes "/tmp/notes" 1000 t)

;; Clean directory
(vulpea-bench-clean-directory "/tmp/notes")
```

Features:
- Fast generation (thousands per second)
- Realistic content (lorem ipsum)
- Random IDs, titles, tags, aliases
- Optional random headings
- Scalable to millions of notes

### Benchmark Infrastructure (`vulpea-bench.el`)

Core benchmarking utilities:

```elisp
(require 'vulpea-bench)

;; Benchmark full sync
(vulpea-bench-sync "/path/to/notes" "/path/to/db.db")
;; => (time note-count)

;; Benchmark query
(vulpea-bench-query "/path/to/db.db" "query-name"
  (lambda () (vulpea-db-query-by-tags-some '("project"))))
;; => (time result-count)

;; Benchmark extraction only
(vulpea-bench-extraction "/path/to/notes" 100)
;; => (time file-count)

;; Measure custom operation
(vulpea-bench-measure "my-operation"
  (do-expensive-work))
;; => (time . result)
```

### Benchmark Runner (`run-benchmarks.sh`)

Shell script for running standard benchmarks:

```bash
# Options
./run-benchmarks.sh \
  --notes 1000      # Number of notes to generate
  --sync            # Run sync benchmark
  --query           # Run query benchmarks
  --extract         # Run extraction benchmark
  --all             # Run all benchmarks (default)
  --clean           # Clean output before running
```

## Benchmark Scenarios

### 1. Scale Testing

Test performance at different scales:

```bash
# Small (1k notes)
./bench/run-benchmarks.sh --notes 1000

# Medium (10k notes)
./bench/run-benchmarks.sh --notes 10000

# Large (100k notes)
./bench/run-benchmarks.sh --notes 100000

# Very Large (1M notes)
./bench/run-benchmarks.sh --notes 1000000
```

### 2. Sync Performance

Measure full database sync:

```bash
./bench/run-benchmarks.sh --notes 10000 --sync
```

Output shows:
- Total time
- Notes synced
- Throughput (notes/sec)
- Average time per note

### 3. Query Performance

Test various query types against synced database:

```bash
./bench/run-benchmarks.sh --notes 10000 --query
```

Benchmarks:
- Query all notes
- Query by single tag
- Query by multiple tags
- Query by level
- Title search

### 4. Extraction Performance

Measure file parsing without database:

```bash
./bench/run-benchmarks.sh --notes 10000 --extract
```

Tests raw parsing speed (samples 100 random files).

## Custom Benchmarks

### Writing Custom Benchmark

Create `bench/my-benchmark.el`:

```elisp
(require 'vulpea-bench)
(require 'vulpea-bench-generate)

;; Generate test data
(let ((notes-dir "/tmp/my-bench-notes")
      (db-file "/tmp/my-bench.db"))

  ;; Setup
  (vulpea-bench-generate-notes notes-dir 1000 t)

  ;; Benchmark
  (vulpea-bench-sync notes-dir db-file)

  ;; Custom query benchmark
  (vulpea-bench-query db-file "complex query"
    (lambda ()
      (vulpea-db-query
       :filter (lambda (note)
                 (and (vulpea-note-tagged-all-p note '("project"))
                      (> (length (vulpea-note-links note)) 5)))))))
```

### Comparing Approaches

Test different implementations:

```elisp
(let ((notes-dir "/tmp/bench-notes"))
  (vulpea-bench-generate-notes notes-dir 1000)

  (message "Approach A:")
  (vulpea-bench-measure "approach-a"
    (approach-a-implementation notes-dir))

  (message "Approach B:")
  (vulpea-bench-measure "approach-b"
    (approach-b-implementation notes-dir)))
```

## Output

Benchmark output includes:

- **Time**: Execution time (μs, ms, s, or min)
- **Count**: Number of items processed
- **Throughput**: Operations per second
- **Average**: Time per operation

Example:
```
=== Benchmarking Full Sync ===
Notes directory: /tmp/bench-notes
Database: /tmp/bench.db
[Full sync] 15.23 s
Files found: 10000
Notes synced: 10000
Throughput: 656.5 ops/s
Average: 1.52 ms per note
```

## Performance Targets

Reference targets for performance regression testing:

| Operation | 1k notes | 10k notes | 100k notes |
|-----------|----------|-----------|------------|
| Full sync | < 3s | < 20s | < 5min |
| Query all | < 50ms | < 100ms | < 500ms |
| Query by tag | < 20ms | < 50ms | < 200ms |
| Extraction | < 2ms/note | < 2ms/note | < 2ms/note |

(Targets based on modern hardware, may vary)

## Tips

1. **Disable GC during measurement**: Done automatically by `vulpea-bench-measure`
2. **Run multiple times**: Results may vary, average multiple runs
3. **Close other processes**: Minimize system load during benchmarking
4. **Use SSD**: File I/O significantly impacts sync performance
5. **Monitor memory**: Large databases may require significant RAM

## Files

- `vulpea-bench-generate.el` - Note generator
- `vulpea-bench.el` - Benchmark infrastructure
- `run-benchmarks.sh` - Benchmark runner script
- `bench-output/` - Generated notes and databases (gitignored)
