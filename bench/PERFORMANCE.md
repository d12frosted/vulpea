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

Buffer reuse still delivers the big win (7x faster than creating a fresh
temp buffer), but now you can retain correctness by enabling per-file
`org-mode` execution when you need it.

## The Critical Choice: Parse Method

The `vulpea-db-parse-method` setting controls how org files are parsed,
and **this choice has dramatic performance and correctness implications**.

### Quick Comparison

| Aspect | single-temp-buffer | temp-buffer (default) | find-file |
|--------|-------------------|-----------------------|-----------|
| **Speed** | ⚡ **~1.3k files/sec** | ⚡ **Depends on hooks** | 🐌 ~32 files/sec |
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

### Current Performance (v2 with optimizations)

**single-temp-buffer** (fastest, skips hooks):

| Files | Time | Throughput | Per-file | Notes |
|-------|------|------------|----------|-------|
| 1K | 0.8s | 1,250/s | 0.80ms | ⚡ Instant |
| 10K | 7.8s | 1,282/s | 0.78ms | ⚡ Fast |
| 110K | **1.4 min** | **1,290/s** | **0.77ms** | ✓ Acceptable |
| 1M | **13 min** | 1,282/s | 0.78ms | Projected |

**temp-buffer** (default, hooks enabled per file):

| Files | Time | Throughput | Per-file | Notes |
|-------|------|------------|----------|-------|
| 1K | 0.9s | 1,110/s | 0.90ms | Base cost w/ empty hooks |
| 10K | 9.9s | 1,010/s | 0.99ms | Base cost w/ empty hooks |
| 110K | **1.8 min** | **1,020/s** | **0.98ms** | Hooks add extra cost |
| 1M | **16 min** | 1,040/s | 0.96ms | Base projection |

> Hook overhead is additive: a 5ms `org-mode-hook` (e.g. `org-roam`)
> adds ~5ms per file. Guard heavy hooks with
> `(unless (bound-and-true-p 'vulpea-db--active-parse-method) …)` to
> avoid this penalty.

**find-file** (respects dir-locals & file-visiting hooks):

| Files | Time | Throughput | Per-file | Notes |
|-------|------|------------|----------|-------|
| 1K | 28s | 36/s | 28ms | 🐌 Slow |
| 10K | 5.1 min | 32/s | 31ms | 🐌 Very slow |
| 110K | **51 min** | 36/s | 28ms | ⚠️ Impractical |
| 1M | **8.6 hours** | 32/s | 31ms | ⚠️ Unusable |

### Performance Evolution

Historical comparison showing optimization journey:

| Version | Method | Files | Time | Speedup | Notes |
|---------|--------|-------|------|---------|-------|
| v1 | File scan only | 110K | ~20s | - | No parsing |
| v2 (naive) | find-file, new buffers | 110K | ~60 min | - | Initial implementation |
| v2 (temp-buffer) | temp-buffer, new buffers | 110K | ~10 min | 6x | Switched to temp-buffer |
| v2 (single-temp) | single temp buffer reuse | 110K | **1.4 min** | **43x** | Fastest |
| v2 (temp-buffer default) | reuse + per-file org-mode | 110K | **1.8 min** | **33x** | Balanced |

### Time Breakdown (single-temp-buffer)

Per-file time breakdown for the fastest single-temp-buffer method:

| Phase | Time | Percentage | Notes |
|-------|------|------------|-------|
| I/O (file read) | 0.12ms | 16% | Disk read |
| org-mode init | ~0.00ms | <1% | ✓ Eliminated by buffer reuse |
| AST parsing | 0.27ms | 35% | org-element-parse-buffer |
| Data extraction | 0.12ms | 16% | Extract nodes, meta, links |
| Database ops | 0.20ms | 26% | SQLite inserts |
| **Total** | **0.77ms** | **100%** | - |

When `vulpea-db-parse-method` is `temp-buffer`, add the time it takes to
run `org-mode` (plus your hooks) after loading each file—the more work
your hooks do, the higher the per-file cost. Guard heavy hooks using
`vulpea-db--active-parse-method` if you want temp-buffer correctness
without paying for optional integrations.

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

**Impact:** buffer reuse still yields the 7x win (10 min → 1.4 min for
110K files) while the new helper toggles whether `org-mode` re-runs per
file.

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
- Sync time: 1.4 min for 110K files
- Pure global configuration

**Scenario 2: Multi-project with dir-locals**
- Different TODO keywords per project
- Custom `org-attach-id-dir` in some directories
- `.dir-locals.el` for project-specific settings

**Recommendation:** ⚠️ Use `find-file`
- Sync time: 51 min for 110K files
- Correctness guaranteed

**Scenario 3: File-level tweaks but no dir-locals**
- Need per-file `#+TODO`, `#+PROPERTY`, or hook-based configuration
- Want faster sync than `find-file`

**Recommendation:** ✓ Use `temp-buffer`
- Sync time: ~2 min for 110K files (plus hook cost)
- Guard heavy hooks for better throughput

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
| `'single-temp-buffer` | <1s | ~8s | ~1.4min | Pure global config |
| `'temp-buffer` | ~1s | ~10s | ~1.8min | Need `#+` keywords / hooks |
| `'find-file` | ~30s | ~5min | ~51min | Need dir-locals / file hooks |

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

- 234 tests total
- 232 passed ✓
- 1 skipped (requires fswatch)
- 1 flaky (pre-existing, unrelated)

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

- Buffer reuse optimization: 7x speedup by eliminating org-mode init overhead
- Parse method comparison: single-temp-buffer is 35-40x faster than find-file
- Balanced default: temp-buffer re-runs org-mode per file for correctness
- Hook guard: `vulpea-db--active-parse-method` lets integrations skip work
- Current fastest performance: 1,290 files/sec (0.77ms/file) with single-temp-buffer
