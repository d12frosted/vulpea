# Performance Analysis: Parse Methods

## Executive Summary

The temp-buffer parsing method provides **13-19x better performance** than `find-file-noselect` while producing identical results after setting `default-directory` to fix attach-dir extraction.

## Benchmark Results

### 1,000 Notes

| Method          | Time   | Throughput | Avg/File | Ratio            |
|-----------------|--------|------------|----------|------------------|
| **temp-buffer** | 2.09s  | 479 ops/s  | 2.09 ms  | **1.0x**         |
| find-file       | 28.27s | 35 ops/s   | 28.27 ms | **13.5x slower** |

### 10,000 Notes

| Method          | Time            | Throughput | Avg/File | Ratio          |
|-----------------|-----------------|------------|----------|----------------|
| **temp-buffer** | 16.27s          | 615 ops/s  | 1.63 ms  | **1.0x**       |
| find-file       | 5.15 min (309s) | 32 ops/s   | 30.92 ms | **19x slower** |

## Implementation Details

### temp-buffer Method (Current, Recommended)

```elisp
(with-temp-buffer
  (insert-file-contents path)
  (setq buffer-file-name path)
  (setq default-directory (file-name-directory path))  ; Key fix
  (org-mode)
  ...)
```

**Pros:**
- Fast: No hooks, no unnecessary setup
- Correct: Setting `default-directory` fixes attach-dir paths
- Predictable: Same behavior regardless of user configuration

**Cons:**
- Doesn't respect `.dir-locals.el` settings
- Doesn't run file-visiting hooks

### find-file Method (Correct but Slow)

```elisp
(let ((buffer (find-file-noselect path t)))
  (unwind-protect
      (with-current-buffer buffer ...)
    (kill-buffer buffer)))
```

**Pros:**
- Respects `.dir-locals.el` settings
- Runs all file-visiting hooks
- "True" file visiting behavior

**Cons:**
- **13-19x slower** due to hook execution
- Less predictable (varies with user config)
- Higher memory pressure (buffer management)

## Verification

Both methods produce **identical** attach-dir paths after the fix:

```
temp-buffer: /path/to/notes/data/f1/fab67d-48c8-405c-8da8-5f10b80683cc
  find-file: /path/to/notes/data/f1/fab67d-48c8-405c-8da8-5f10b80683cc
```

Previously (without `default-directory` fix):
```
Wrong: /path/to/vulpea/data/f1/fab67d-...  # Wrong directory context
```

## Recommendation

**Use temp-buffer method with `default-directory` set.**

The performance penalty of find-file is too high (13-19x) for:
- Only one column (attach-dir) that depends on directory context
- Can be fixed with simple `default-directory` assignment
- Edge case: .dir-locals.el with custom `org-attach-id-dir` won't be respected
  - But this is rare, and users can still use find-file mode if needed

## Configuration

Users can override via:

```elisp
(setq vulpea-db-parse-method 'find-file)  ; Slower but respects dir-locals
```

Default is `'temp-buffer` (recommended).

## Scaling Projection

Based on 10K results:

| Notes | temp-buffer | find-file     |
|-------|-------------|---------------|
| 100   | 0.16s       | 3.09s         |
| 1K    | 1.6s        | 30.9s         |
| 10K   | 16s         | 5.1 min       |
| 100K  | **2.7 min** | **51 min**    |
| 1M    | **27 min**  | **8.6 hours** |

For large repositories (100K+ notes), the difference becomes critical.
