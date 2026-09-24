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

## Reference Numbers

The performance figures quoted in the guides come from this section.
Each one names the benchmark that produced it, so it can be re-run.

Measured on 2026-09-23 on a MacBook Pro with an Apple M1 Pro (32GB),
macOS 26.3, Emacs 31.0.50, stock org-mode, vulpea at commit 21747c2.
Every run is byte-compiled: pass `-c` to `eldev exec` (`eldev -p`
compiles too). Without it vulpea runs interpreted from source and the
numbers come out several times slower. Absolute times move with
hardware and Emacs build; ratios between rows of one table are what the
guides rely on.

### Full sync throughput

`vulpea-bench-sync` over generated notes with headings
(`vulpea-bench-generate-notes DIR N t`), one `eldev -p -dtT exec` run
per parse method and size:

| parse method         | 1k            | 10k            | 100k            |
|----------------------|---------------|----------------|-----------------|
| `single-temp-buffer` | 1.8s (563/s)  | 12.8s (783/s)  | 2.0min (844/s)  |
| `temp-buffer`        | 2.6s (381/s)  | 20.6s (486/s)  | 3.3min (499/s)  |
| `find-file`          | 3.3s (304/s)  | 27.2s (367/s)  | 4.5min (368/s)  |

Every method holds its rate across a run: each 10k files of the 100k
runs took 11.4s, 19.7s and 26.8s respectively, from the first 10k to
the last. (Until #523, org registered every parsed file with its
persistent element cache and each file cost more than the one before;
100k took 16 minutes with `single-temp-buffer`.) The 1k runs include
start-up, which is why their rate is lower.

The November 2025 numbers in [PERFORMANCE.md](PERFORMANCE.md) came from
a different setup.

### Saving one large file

`vulpea-bench-file-run` and `vulpea-bench-file-async-run` (see
[Single Large File Benchmark](#single-large-file-benchmark)). The
number is how long the main thread is blocked when autosync processes a
save, which is the freeze you feel:

| file  | notes  | synchronous | async `t` | async `full` |
|-------|--------|-------------|-----------|--------------|
| 1MB   | 365    | 252ms       | 43ms      | 12ms         |
| 10MB  | 3.7k   | 2.28s       | 0.45s     | 1.1ms        |
| 100MB | 36.7k  | 25.9s       | 6.0s      | 1.3ms        |

The first request of a session also spawns the worker, which is most
of the 1MB async figures. In `full` mode the database is written by the
worker, so the data becomes queryable later: 1.4s, 3.4s and 25s after
the save for the three sizes.

### What the indexing options change

The same 10MB file, save path, with one option changed from the
default at a time:

| setting                                  | save path | AST parse |
|------------------------------------------|-----------|-----------|
| defaults                                 | 2.28s     | 0.92s     |
| `vulpea-db-parse-granularity 'object`    | 3.64s     | 2.39s     |
| `vulpea-db-index-plain-links nil`        | 2.16s     | 0.90s     |
| `vulpea-db-index-heading-level nil`      | 1.68s     | 0.90s     |

The object parse is 2.6x slower than the element parse, and a whole
save is 1.6x slower with it. The file has 3.7k heading notes, which is
what turning heading notes off saves; a collection of file-level notes
gains nothing from it.

### Metadata: one call per property vs a batch

`vulpea-bench-meta-batch`, new properties on a file-level note, mean of
20 runs:

| properties | one by one | batch  | ratio |
|------------|------------|--------|-------|
| 5          | 4.2ms      | 1.7ms  | 2.5x  |
| 20         | 21.8ms     | 3.7ms  | 5.9x  |
| 50         | 88.5ms     | 4.9ms  | 18x   |

### Listing files and starting autosync

Generated notes in plain directories under one root, with a git
history next to them (`git init`, the notes committed, the repository
packed): 14,000 notes in 140 directories of 100, 100,000 and 1,000,000
notes in directories of 1,000. `vulpea-bench-file-listing`, measured
until vulpea has the file list; median of 10 runs (5 at 1M):

| notes | `fd` (vulpea's scan) | `find` (fd absent) | `directory-files-recursively` |
|-------|----------------------|--------------------|-------------------------------|
| 14k   | 24ms                 | 72ms               | 34ms, blocking                |
| 100k  | 152ms                | 547ms              | 253ms, blocking               |
| 1M    | 1.25s                | 13.2s              | 3.08s, blocking               |

The listing commands alone (output to `/dev/null`, `/usr/bin/find`)
take 19ms, 76ms and 0.57s with `fd`, and 80ms, 458ms and 13.3s with
`find`; the rest is vulpea reading the list back, in the process
sentinel on the main thread. `sync-timing-test-run` with
`VULPEA_NOTES_DIR` pointing at the 14k tree: enabling
`vulpea-db-autosync-mode` (async startup scan, fswatch) returns in
10ms, the scan finishes 0.4s later, and checking all 14k unchanged
files takes another 0.85s, spread over idle batches.

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

;; Benchmark org-id registration of 100k files in a fresh session
;; (no notes on disk needed)
(vulpea-bench-org-id-registration 100000)
;; => time in seconds
;; Benchmark the startup org-id repair pass over 100k database rows,
;; cold (empty index) and warm (nothing to do)
(vulpea-bench-org-id-repair 100000)
;; => (cold . warm) in seconds
;; Benchmark the sync queue over unchanged files: per-batch cost of
;; the org-id check the batches make for files they skip
(vulpea-bench-queue-unchanged "/path/to/notes" "/path/to/db.db")
;; => ((cold-total cold-max) (warm-total warm-max)) in seconds
;; Compare one vulpea-meta-set per property with one batch call
(vulpea-bench-meta-batch)
;; => ((count single-seconds batch-seconds) ...)
;; Time fd, find and directory-files-recursively listing a tree
;; (not under a hidden directory: vulpea skips those)
(vulpea-bench-file-listing "/path/to/notes")
;; => (:count N :fd S :find S :directory-files-recursively S)
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
eldev -c -dtT exec "(progn \
  (add-to-list 'load-path (expand-file-name \"bench\")) \
  (require 'vulpea-bench-schema) \
  (vulpea-bench-schema-run))"
```

The `-c` matters: it makes eldev byte-compile vulpea and load the `.elc`
files. Without it vulpea runs interpreted from source, and validation is
about 6x slower (roughly 500ms at 10k instead of 90ms). That measures the
interpreter, not the code a user runs.

`vulpea-bench-schema-run` takes optional SCALES and INVALID-FRACTION
arguments; it defaults to scales `(1000 10000 100000)` and a 0.2 invalid
fraction.

### Measured reference numbers

Measured on an Apple M1 Pro with Emacs 31.0.50, byte-compiled (`-c`).
Hardware and Emacs version shift the absolutes; the shape (linear) is the
thing to watch.

Validation is linear, roughly 8us per note:

| scale | time  |
|-------|-------|
| 1k    | 29ms  |
| 10k   | 91ms  |
| 100k  | 770ms |

The 1k row is mostly fixed overhead; the per-note cost shows from 10k up.

The takeaway: schema validation is linear and cheap, so the validation
pipeline itself is not a bottleneck at realistic collection sizes.

## Single Large File Benchmark

The benchmarks above measure throughput across many small files. This
one measures **latency for one file as its size grows** — the setup
from issue [#359](https://github.com/d12frosted/vulpea/issues/359),
where a user keeps most content in a single ~1.2MB org file and every
save freezes the UI.

The freeze exists because sync is only *scheduled* asynchronously:
the queue is driven by timers, but once a timer fires, hashing,
`org-element-parse-buffer`, extraction, and the DB write all run on
the main thread. For many small files batching hides this; for one
big file the freeze lasts as long as that file takes to index.

It lives in `vulpea-bench-file.el`. For each size it generates a
realistic single org file (headings, ~30% with IDs, id-links, meta)
and measures three things:

1. **First index** — `vulpea-db-update-file` with per-phase breakdown
   (io, org-mode init, AST parse, extract+hash, db write).
2. **Save path** — `vulpea-db-sync--update-file-if-changed` on a
   modified file: hash verification + full re-index. This is exactly
   what autosync runs after a save; its wall time is the UI freeze.
3. **Unchanged check** — same entry point when only mtime changed:
   the cost of confirming there is nothing to do.

### Running it

```bash
eldev -c -dtT exec "(progn \
  (add-to-list 'load-path (expand-file-name \"bench\")) \
  (require 'vulpea-bench-file) \
  (vulpea-bench-file-run))"
```

`vulpea-bench-file-run` takes an optional SIZES list in bytes;
it defaults to 100KB, 1MB, 10MB, and 100MB.

## Files

- `vulpea-bench-generate.el` - Note generator (writes .org files)
- `vulpea-bench.el` - Benchmark infrastructure (sync/query/extraction)
- `vulpea-bench-schema.el` - Schema validation benchmark (in-memory notes)
- `vulpea-bench-file.el` - Single large file latency benchmark (issue #359)
- `run-benchmarks.sh` - Benchmark runner script
- `bench-output/` - Generated notes and databases (gitignored)
