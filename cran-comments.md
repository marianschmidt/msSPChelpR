# CRAN comments for msSPChelpR

## 2022-06-11: Resubmission notes

This is a resubmission of an updated version of the CRAN package with new features and bug fixes.

### Full list of changes since last submission
* new function `calc_refrates()` to calculate age-, sex-, region-, year-specific reference rates from a long format dataframe with cancer cases that are counted for incident cases and then matched with a reference population. The resulting reference rates dataframe can directly be used with `sir_byfutime()` function.
* functions gain new default `dattype = NULL` and thus are more flexible to take other source data types (Closes #73)
* functions `asir`, `calc_futime*`, `calc_refrates`, `ir_crosstab_byfutime`, `pat_status*`, `renumber_time_id*`, and  `sir_byfutime` now by default are set to `dattype = NULL`. If you relied on automatic variable naming feature, you need to add `dattype = "seer"`or `dattype = "zfkd"` to your function call.
* fix typo in attribute names: attributes are now correctly named `problems_missing_count_strata` and `problems_missing_fu_strata` (Closes #80)
* `sir_byfutime()`:
  * attributes with notes and problems are now correctly saved to `results_df`
* deprecated functions from `tidytable` package have been replaced (Closes #71 and #74)

### Test environments
* local Windows 10, R 4.1.3, x64
* win-builder (devel and release)
* r-hub (Windows Server 2022, R-devel, 64 bit)
* r-hub (Fedora Linux, R-devel, clang, gfortran)
* r-hub (Ubuntu Linux 20.04.1 LTS, R-release, GCC)
* r-hub (Ubuntu Linux 20.04.1 LTS, R-devel with rchk)  ⇒ currently not possible to test due to Bioconductor issue with packages under R 4.3
* github-actions (windows-latest, macOS-latest, ubuntu-lastes (release), ubuntu-latest (devel), ubuntu-latest (oldrel-1))

### R CMD check results
0 errors | 0 warnings | 0 notes

### Downstream dependencies
There are currently no downstream dependencies for this package.
