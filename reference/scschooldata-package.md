# scschooldata: Fetch and Process South Carolina School Data

Downloads and processes school data from the South Carolina Department
of Education (SCDE). Provides functions for fetching enrollment data
from Active Student Headcounts and SC Report Cards, transforming it into
tidy format for analysis. Supports data from 2018-present via SC Report
Cards and 2013-present via Active Student Headcounts.

The scschooldata package provides functions for downloading, processing,
and analyzing school enrollment data from the South Carolina Department
of Education (SCDE).

## Data Sources

The package uses two primary data sources:

- **Active Student Headcounts** (ed.sc.gov): The primary source,
  providing school and district-level enrollment by grade and
  demographics. Available from 2012-13 school year onwards.

- **SC Report Cards** (screportcards.com): Secondary source with
  comprehensive school information. Available from 2017-18 school year
  onwards.

## Main Functions

- [`fetch_enr`](https://almartin82.github.io/scschooldata/reference/fetch_enr.md):

  Download and process enrollment data for a single year

- [`fetch_enr_multi`](https://almartin82.github.io/scschooldata/reference/fetch_enr_multi.md):

  Download enrollment data for multiple years

- [`tidy_enr`](https://almartin82.github.io/scschooldata/reference/tidy_enr.md):

  Convert wide enrollment data to long format

- [`get_available_years`](https://almartin82.github.io/scschooldata/reference/get_available_years.md):

  List available data years

## Caching

Downloaded data is cached locally to avoid repeated downloads. Use
[`cache_status`](https://almartin82.github.io/scschooldata/reference/cache_status.md)
to view cached files and
[`clear_cache`](https://almartin82.github.io/scschooldata/reference/clear_cache.md)
to remove them.

## See also

Useful links:

- <https://github.com/almartin82/scschooldata>

- Report bugs at <https://github.com/almartin82/scschooldata/issues>

Useful links:

- <https://github.com/almartin82/scschooldata>

- Report bugs at <https://github.com/almartin82/scschooldata/issues>

## Author

**Maintainer**: Al Martin <almartin@example.com>
