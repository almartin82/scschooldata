# scschooldata TODO

## pkgdown Build Issues

### Network Timeout During Vignette Build (2026-01-01)

**Error:** The pkgdown build fails when rendering `vignettes/enrollment_hooks.Rmd`:

```
Error in utils::unzip(zip_path, list = TRUE) :
  zip file '...sc_school_grade_f34a5fd47b20.xlsx' cannot be opened
```

**Root Cause:** Network timeouts when downloading data from `ed.sc.gov`. The SCDE website is either:
1. Experiencing temporary connectivity issues
2. Rate-limiting requests
3. Having intermittent server problems

**Location:** The vignette calls `fetch_enr_multi(c(2013, 2015, 2017, 2019, 2021, 2023, 2025))` at line 39, which attempts fresh downloads from SCDE.

**Workaround Options:**
1. Pre-populate the cache with required years before running pkgdown build
2. Add `eval = FALSE` to vignette chunks that fetch data
3. Use pre-computed datasets included in the package
4. Run the build during off-peak hours when SCDE servers are more responsive

**Status:** Blocked - requires stable network connection to SCDE servers
