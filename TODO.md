# scschooldata TODO

## Completed

### Network Timeout During Vignette Build (Fixed 2026-01-02)

**Issue:** pkgdown build failed on CI due to network timeouts when
vignette tried to fetch data from ed.sc.gov.

**Fix Applied:** Added `eval = !identical(Sys.getenv("CI"), "true")` to
vignette setup chunk to skip code evaluation during CI builds. Also
added missing `forcats` and `tidyr` to Suggests in DESCRIPTION.

The vignette HTML will still be built from pre-cached results, but code
chunks won’t attempt to download data during CI.
