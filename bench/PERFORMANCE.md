# Vulpea Sync Performance Guide

## Table of Contents
- [Executive Summary](#executive-summary)
- [The Critical Choice: Parse Method](#the-critical-choice-parse-method)
- [Performance Benchmarks](#performance-benchmarks)
- [Implementation Details](#implementation-details)
- [Choosing the Right Method](#choosing-the-right-method)
- [Configuration](#configuration)

## Executive Summary

Vulpea v2 provides three parsing strategies so you can choose the exact
trade-off between speed and correctness:

1. **single-temp-buffer (fastest)** – Reuses a hidden buffer and never
   re-runs `org-mode`. Ideal when every Org setting is global.
2. **temp-buffer (default)** – Reuses the buffer but re-runs `org-mode`
   per file, so `#+TODO`, `#+PROPERTY`, and `org-mode-hook` execute with
   the correct `buffer-file-name`.
3. **find-file (slowest)** – Visits files exactly like `find-file`, so
   `.dir-locals.el` and file-visiting hooks are honored.

Buffer reuse still delivers significant wins (3-4x speedup), but now you can
retain correctness by enabling per-file `org-mode` execution when you need it.

## The Critical Choice: Parse Method

The `vulpea-db-parse-method` setting controls how org files are parsed,
and **this choice has dramatic performance and correctness implications**.

### Quick Comparison

| Aspect | single-temp-buffer | temp-buffer (default) | find-file |
|--------|-------------------|-----------------------|-----------|
| **Speed** | ⚡ **~1k files/sec** | ⚠️ **~460/s** (degrades to ~56/s) | 🐌 ~34 files/sec (degrades to ~16/s) |
| **Hooks** | ✗ Never run | ✓ `org-mode` + hooks each file | ✓ All file-visiting hooks |
| **Dir-locals** | ✗ Ignored | ✗ Ignored | ✓ Respected |
| **#+TODO / #+PROPERTY** | ✗ Ignored after first file | ✓ Respected | ✓ Respected |
| **org-attach-dir** | ⚠️ Global only | ✓ Honors per-file keywords | ✓ Honors dir-locals |
| **Best for** | Purely global setups | Users needing per-file keywords/hooks | Complex dir-locals |

### `single-temp-buffer` (fastest)

- Reuses a hidden `org-mode` buffer and never re-runs `org-mode`.
- Ideal when you only rely on global Org configuration.
- `org-mode-hook` never runs, so integrations like `org-roam` are skipped.
- `#+TODO`, `#+PROPERTY`, and other buffer-local keywords are ignored after
  the first file—this is the intentional trade-off for speed.

### `temp-buffer` (default)

- Reuses the buffer but re-runs `org-mode` (and its hooks) after loading
  every file, so buffer-local keywords and hook-based logic see the correct
  `buffer-file-name`.
- Performance depends on your `org-mode-hook`. A lean hook list keeps the
  per-file cost around ~1ms; heavy hooks like `org-roam` add their own cost.
- Hooks can detect Vulpea parsing by checking
  `vulpea-db--active-parse-method` and skip expensive work:

  ```elisp
  (add-hook 'org-mode-hook
            (lambda ()
              (unless (bound-and-true-p 'vulpea-db--active-parse-method)
                ;; Heavy features only when visiting manually
                (org-roam-db-autosync-mode 1))))
  ```

### `find-file` (slowest, most correct)

- Uses `find-file-noselect`, so `.dir-locals.el`, file-visiting hooks,
  and every other Emacs mechanism run exactly as if you visited the file.
- 30–40x slower than the temp-buffer options, but required when you depend
  on dir-locals or other mechanisms that only trigger during real visits.

## Performance Benchmarks

> **Benchmark Environment:** MacBook Pro 2021 with Apple M1 Pro and APPLE SSD
> AP0512R. All benchmarks below use **stock org-mode configuration** (no custom
> hooks, no dir-locals, no buffer-local settings) to show the pure difference
> between parsing methods. Real-world performance will vary based on machine
> state, hardware specifications, note complexity, and your configuration.

### Current Performance (v2 with optimizations)

**single-temp-buffer** (fastest, skips hooks):

| Files | Time | Throughput | Per-file | Notes |
|-------|------|------------|----------|-------|
| 1K | 1.33s | 754/s | 1.33ms | ⚡ Instant |
| 10K | 8.89s | 1.13k/s | 888μs | ⚡ Fast |
| 100K | **1.59 min** | **1.05k/s** | **951μs** | ✓ Acceptable |
| 1M | **20.98 min** | 795/s | 1.26ms | ✓ Practical |

**temp-buffer** (default, hooks enabled per file):

| Files | Time | Throughput | Per-file | Notes |
|-------|------|------------|----------|-------|
| 1K | 2.20s | 455/s | 2.20ms | Base cost w/ stock org-mode |
| 10K | 21.77s | 459/s | 2.18ms | Base cost w/ stock org-mode |
| 100K | **29.89 min** | **56/s** | **17.93ms** | ⚠️ Performance degrades at scale |
| 1M | Not tested | - | - | 100K already > single-temp 1M time |

> **Performance degradation:** With repeated operations, this method shows
> eventual performance degradation. For large note collections (100K+), consider
> `single-temp-buffer` if you don't need per-file hooks or keywords.
>
> Hook overhead is additive: custom hooks add their cost per file. Guard heavy
> hooks with `(unless (bound-and-true-p 'vulpea-db--active-parse-method) …)` to
> avoid this penalty.

**find-file** (respects dir-locals & file-visiting hooks):

| Files | Time | Throughput | Per-file | Notes |
|-------|------|------------|----------|-------|
| 1K | 25.80s | 39/s | 25.80ms | 🐌 Slow |
| 10K | 4.90 min | 34/s | 29.43ms | 🐌 Very slow |
| 100K | **102.34 min** | **16/s** | **61.41ms** | ⚠️ Impractical for large collections |
| 1M | Not tested | - | - | Estimated > 24 hours |

> **Performance degradation:** Shows even more pronounced degradation with scale.
> Only use this method when dir-locals or file-visiting hooks are essential.

### Performance Evolution

Evolution of parse methods (all measured with stock org-mode, no custom hooks, no dir-locals, no buffer-local settings):

| Implementation | Method | Files | Time | Speedup vs find-file | Notes |
|----------------|--------|-------|------|---------------------|-------|
| find-file | find-file, new buffers each time | 100K | 102.34 min | 1x (baseline) | Full correctness, slowest |
| temp-buffer | temp-buffer, buffer reuse | 100K | 29.89 min | 3.4x | Reuses buffer, re-runs org-mode per file |
| single-temp-buffer | single buffer reuse, no hooks | 100K | **1.59 min** | **64x** | Fastest, skips per-file org-mode entirely |

These benchmarks use stock org-mode configuration to show the pure difference between
parsing methods. Real-world performance with custom hooks, dir-locals, and buffer
settings will vary - especially for temp-buffer and find-file methods.

## Implementation Details

### Buffer Reuse Optimization

**Problem:** Each file created a new temp buffer and called `org-mode`, adding 0.90ms overhead per file (59% of total time).

**Solution:** Reuse a single buffer with org-mode already initialized:

```elisp
(defvar vulpea-db--parse-buffer nil
  "Reusable buffer for parsing org files.")

(defvar vulpea-db--parse-buffer-run-hooks t
  "Whether org-mode hooks should run when initializing the buffer.")

(defmacro vulpea-db--with-parse-buffer (&rest body)
  "Execute BODY inside the shared buffer."
  `(progn
     (unless (and vulpea-db--parse-buffer
                  (buffer-live-p vulpea-db--parse-buffer))
       (setq vulpea-db--parse-buffer (generate-new-buffer " *vulpea-parse*"))
       (with-current-buffer vulpea-db--parse-buffer
         (let ((delay-mode-hooks (not vulpea-db--parse-buffer-run-hooks)))
           (org-mode))))
     (with-current-buffer vulpea-db--parse-buffer
       ,@body)))

(defun vulpea-db--parse-with-temp-buffer (path rerun-org-mode)
  (let ((vulpea-db--parse-buffer-run-hooks rerun-org-mode))
    (vulpea-db--with-parse-buffer
      (erase-buffer)
      (insert-file-contents path)
      (setq buffer-file-name path
            default-directory (file-name-directory path))
      (when rerun-org-mode
        (let ((delay-mode-hooks nil))
          (org-mode)))
      (org-element-parse-buffer)
      ...)))
```

**Impact:** buffer reuse yields a 3.4x speedup (~40 min → 29.89 min for
100K files with temp-buffer) while the new helper toggles whether `org-mode`
re-runs per file. Skipping per-file org-mode entirely (single-temp-buffer)
provides 64x speedup vs find-file (102.34 min → 1.59 min for 100K files).

### `single-temp-buffer`

- Skips per-file `org-mode` entirely after the first buffer setup
  (hooks never run).
- Fastest possible parsing; no extra Emacs machinery runs.
- Limited correctness: ignores `#+TODO`, `org-attach-dir`, and any
  hook-based configuration after the first file.

### `temp-buffer`

- Calls `org-mode` after loading each file so hooks and file keywords
  execute with the correct `buffer-file-name`.
- Still benefits from buffer reuse (no reallocation), but hook cost is
  paid per file.
- Guard heavy hooks using `vulpea-db--active-parse-method` if desired.

### `find-file`

- Falls back to `find-file-noselect` and kills the buffer afterwards.
- Slow but 100% faithful to normal file visiting: dir-locals, minor
  modes, and file hooks all run automatically.

## Choosing the Right Method

### Decision Tree

```
Do you rely on .dir-locals.el or file-visiting hooks?
├─ Yes → Use find-file (correctness wins)
└─ No
   └─ Do you need per-file #+TODO / hook-based tweaks?
      ├─ Yes → Use temp-buffer (default)
      └─ No → Use single-temp-buffer (fastest)
```

### Real-World Scenarios

**Scenario 1: Default Org user (90% of users)**
- Global `org-todo-keywords`
- Default `org-attach-dir` behavior
- No `.dir-locals.el` customizations

**Recommendation:** ✓ Use `single-temp-buffer`
- Sync time: 1.6 min for 100K files
- Pure global configuration

**Scenario 2: Multi-project with dir-locals**
- Different TODO keywords per project
- Custom `org-attach-id-dir` in some directories
- `.dir-locals.el` for project-specific settings

**Recommendation:** ⚠️ Use `find-file`
- Sync time: 102 min for 100K files
- Correctness guaranteed

**Scenario 3: File-level tweaks but no dir-locals**
- Need per-file `#+TODO`, `#+PROPERTY`, or hook-based configuration
- Want faster sync than `find-file`

**Recommendation:** ✓ Use `temp-buffer`
- Sync time: ~30 min for 100K files (with stock org-mode)
- Guard heavy hooks for better throughput
- Note: Performance degrades at scale; consider `single-temp-buffer` for 100K+ notes

## Configuration

### Setting Parse Method

In your Emacs config:

```elisp
;; Fastest, skips hooks entirely
(setq vulpea-db-parse-method 'single-temp-buffer)

;; Balanced (default), re-runs org-mode per file
(setq vulpea-db-parse-method 'temp-buffer)

;; Slowest, but respects dir-locals + file hooks
(setq vulpea-db-parse-method 'find-file)
```

### Performance Expectations

After changing the setting:

| Setting | 1K notes | 10K notes | 100K notes | When to use |
|---------|----------|-----------|------------|-------------|
| `'single-temp-buffer` | ~1.3s | ~8.9s | ~1.6min | Pure global config |
| `'temp-buffer` | ~2.2s | ~22s | ~30min | Need `#+` keywords / hooks |
| `'find-file` | ~26s | ~4.9min | ~102min | Need dir-locals / file hooks |

### Verifying Your Choice

Check if you're using dir-locals features:

```bash
# Search for .dir-locals.el files in your notes directory
find ~/org -name ".dir-locals.el"

# Check for per-directory TODO keywords
grep -r "org-todo-keywords" ~/org/.dir-locals.el

# Check for custom attach directories
grep -r "org-attach-id-dir" ~/org/.dir-locals.el
```

If these searches return nothing, you can safely use either
`single-temp-buffer` (fastest) or `temp-buffer` (default). Use the latter
if you rely on `#+TODO`, `#+PROPERTY`, or hook-based configuration.

## Testing

### Test Results

All optimizations are tested with the full test suite:

- 238 tests total
- 237 passed ✓
- 1 skipped (requires fswatch)

### Verification

`single-temp-buffer` and `temp-buffer` produce identical results when
your configuration is purely global:

```elisp
(should (equal
         (let ((vulpea-db-parse-method 'single-temp-buffer))
           (vulpea-db--parse-file path))
         (let ((vulpea-db-parse-method 'temp-buffer))
           (vulpea-db--parse-file path))))
```

Differences arise only when hooks, `#+` keywords, or dir-locals modify
buffer-local state. Use `find-file` if you need dir-locals-driven
behavior.

## Files Modified

- `vulpea-db-extract.el`: Buffer reuse infrastructure and parse methods
- `vulpea-db-extract.el`: `vulpea-db-parse-method` customization variable
- `bench/timing-test.el`: Performance measurement utilities

## References

- Buffer reuse optimization: 3.4x speedup by eliminating buffer creation overhead
- Parse method comparison: single-temp-buffer is 64x faster than find-file at 100K files
- Balanced default: temp-buffer re-runs org-mode per file for correctness
- Hook guard: `vulpea-db--active-parse-method` lets integrations skip work
- Current fastest performance: 1.05k files/sec (951μs/file) with single-temp-buffer at 100K files
- Performance degradation: temp-buffer and find-file show degradation at scale
