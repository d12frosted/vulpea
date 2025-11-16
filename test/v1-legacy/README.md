# V1 Legacy Tests

This directory contains tests from Vulpea v1 that use the old API (org-roam dependency, buttercup framework).

## Status

These tests are **not currently running** as part of the v2 test suite. They are preserved here for reference when implementing Phase 4 (High-Level API) of the v2 rewrite.

## Files

- `vulpea-test.el` - High-level API tests (vulpea-find, etc.)
- `vulpea-select-test.el` - Selection API tests
- `vulpea-buffer-test.el` - Buffer manipulation tests
- `vulpea-meta-test.el` - Metadata API tests
- `vulpea-note-test.el` - Note operations tests
- `vulpea-utils-test.el` - Utility function tests
- `vulpea-perf-test.el` - Performance tests
- `vulpea-meta-perf-test.el` - Metadata performance tests

## Migration Plan

When implementing Phase 4, these tests should be:

1. Reviewed for test cases that are still relevant
2. Rewritten to use v2 APIs (no org-roam dependency)
3. Converted from buttercup to ERT for consistency with v2 tests
4. Moved back to `test/` directory

## V2 Tests

Current v2 tests (using ERT) are in the `test/` directory:

- `vulpea-db-test.el` - Database core tests (11 tests)
- `vulpea-db-extract-test.el` - Parser & extractor tests (18 tests)
- `vulpea-db-sync-test.el` - File watching & sync tests (19 tests)

**Total: 48 passing tests**
