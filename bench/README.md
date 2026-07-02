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
| Schema validation | ~6ms | ~66ms | ~665ms |

(Targets based on modern hardware, may vary)

## Tips

1. **Disable GC during measurement**: Done automatically by `vulpea-bench-measure`
2. **Run multiple times**: Results may vary, average multiple runs
3. **Close other processes**: Minimize system load during benchmarking
4. **Use SSD**: File I/O significantly impacts sync performance
5. **Monitor memory**: Large databases may require significant RAM

## Schema Pipeline Benchmarks

The benchmarks above are about sync, query, and extraction. This one
measures schema **validation**: how long `vulpea-schema-collection-health`
takes to validate a collection of notes. It exists so I can catch
regressions in the validation pipeline as the schema engine grows.

It lives in `vulpea-bench-schema.el` and reuses the shared bench helpers
(`vulpea-bench-measure` for timing, `vulpea-bench-uuid` for ids).

### What it measures

The notes are fabricated in memory with `make-vulpea-note`: there is no
database, no generator, and nothing is written to disk. Reading notes
from the DB is already covered by the query benchmark above, and the
schema dashboard is a vulpea-ui concern benchmarked there. This
benchmark isolates the validation compute.

The schema under test is a wine schema (predicate "has tag wine", seven
fields including required strings, a number, and a required `:one-of`
symbol). It is deliberately DB-free: no `:type note` / `:target-tags`
fields, so per-field validation issues no DB lookups. Reference-heavy
schemas are a separate performance regime and are not what this
benchmark measures. Each fabricated note carries the wine fields plus a
little extra (a couple more meta keys and an alias) so the timing is
representative of real notes rather than a bare floor. A configurable
fraction (default 20%) is invalid: they drop the required "producer"
field and set a disallowed "colour".

### Running it

Driven through `eldev exec` from the repo root, like the other benches.
For each scale it fabricates the notes, does one warmup validation, then
times `vulpea-schema-collection-health` and prints a summary line
(scale, time, invalid count):

```bash
eldev -dtT exec "(progn \
  (add-to-list 'load-path (expand-file-name \"bench\")) \
  (require 'vulpea-bench-schema) \
  (vulpea-bench-schema-run))"
```

`vulpea-bench-schema-run` takes optional SCALES and INVALID-FRACTION
arguments; it defaults to scales `(1000 10000 100000)` and a 0.2 invalid
fraction.

### Measured reference numbers

Numbers I measured while building this, so you know what good looks like.
Hardware and Emacs version shift the absolutes; the shape (linear) is the
thing to watch.

Validation is linear, roughly 6us per note:

| scale | time  |
|-------|-------|
| 1k    | 6ms   |
| 10k   | 66ms  |
| 100k  | 665ms |

The takeaway: schema validation is linear and cheap, so the validation
pipeline itself is not a bottleneck at realistic collection sizes.

## Files

- `vulpea-bench-generate.el` - Note generator (writes .org files)
- `vulpea-bench.el` - Benchmark infrastructure (sync/query/extraction)
- `vulpea-bench-schema.el` - Schema validation benchmark (in-memory notes)
- `run-benchmarks.sh` - Benchmark runner script
- `bench-output/` - Generated notes and databases (gitignored)
