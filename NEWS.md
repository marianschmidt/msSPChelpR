# msSPChelpR 0.8.7

### New Features
* new function `sir_ratio()` and related `sir_ratio_lci()` and `sir_ratio_uci()` to calculate ratio of two SIRs/SMRs to get relative risk and confidence limits for this ratio.
* tidytable variant of reshape_long function, i.e. `reshape_long_tt()` ⇒ the _tt variants usually have smaller memory use than tidyverse and data.table variants. Execution time is usually much faster than tidyverse and comparable to or a little slower than the data.table variant.
* `summarize_sir_results()`:
  * add ability to summarize by different site_var than the one used in `sir_byfutime()`

### Bug fixes
* `summarize_sir_results()`:
  * PYARs are now correctly calculated when using `summarize_site == TRUE`. Previously the results incorrectly counted each site multiple times. (Closes #62)
* `pat_status()`:
  * update default values for `dattype = "zfkd"`

### Internal
* add R-CMD-Check to github actions


# msSPChelpR 0.8.6

### New Features
* new sample data set for standard populations ⇒ `data("standard_population")`
* new sample data set for us population ⇒ `data("population_us")` (Closes #58)

### Bug fixes
* `sir_byfutime()`: change output of integer columns to numeric to fix bug in `summarize_sir_results()` (Closes #59)

### Other changes
* add examples to function documentation (Closes #56)
* remove "R" from package title (Closes #57)
* update package description (Closes #54)
* update introduction vignette `vignette("introduction")`


# msSPChelpR 0.8.5 - 2020-09-28

### New Features
* tidytable variants of functions, i.e. `reshape_wide_tt()`, `renumber_time_id_tt()`, `pat_status_tt()`, `vital_status_tt()`,  `calc_futime_tt()` ⇒ the _tt variants usually have smaller memory use than tidyverse and data.table variants. Execution time is usually much faster than tidyverse and comparable to or a little slower than the data.table variant.
* `sir_byfutime()`:
 * is much faster using tidytable package
 * gained the option `race_var` to optionally stratify SIR calculations by race.
* `summarize_sir_results()`:
 * new function that increases functionality in summarizing results from `sir_byfutime()` function
 * new option to define custom `site_var_name`
* new package website https://marianschmidt.github.io/msSPChelpR
* new sample datasets included in the package to demonstrate examples (#36)

### Breaking Changes
* `sir_byfutime()`: 
  * options `add_total_row` and `add_total_fu` are replaced by `calc_total_row` and `calc_total_fu`. These are logical parameters now. The positioning of total rows and columns is completely handled by the `summarize_sir_results()` function now. There total rows can be set to top and bottom and total columns to left and right.
  * option `expcount_src` including related parameters `stdpop_df`, `refpop_df`, `std_pop`, `truncate_std_pop` and `pyar_var` have been removed. Function `sir_byfutime()` will only work calculating expected counts based on reference rates, not within the cohort of the dataset. To calculate expected based on the cohort, a new function `create_refrates` will be added in the future. (#41)
  * option `collapse_ci` has been removed and added to `summarize_sir_results()` instead.
  * option name for tumor site variable changed from `icdcat_var` to `site_var`
  * option name for age/age group variable changed from `agegroup_var` to `age_var`
  * in total the parameters `expcount_src`, `futime_src`, `stdpop_df`, `refpop_df`, `std_pop`, `truncate_std_pop`, `pyar_var`, `icdcat_var`, `collapse_ci` have been removed to simply the function ⇒ make sure you remove these arguments from your `sir_byfutime()` function calls.
* `sir()`: 
  * is superseded by the use of `sir_byfutime()`. To migrate your former `sir()` functions, you can simply use `sir_byfutime(, futime_breaks = "none")` that will yield the same results.
* `summarize_sir_results()`: 
   * option name for tumor site variable changed from `summarize_icdcat` to `summarize_site`
* `reshape_long_tidyr()`: 
  * option `var_selection` is deprecated. Please select variables before running the `reshape_long_*` functions.
* `asir()`:
  * option name for age/age group variable changed from `agegroup_var` to `age_var`
  * option name for tumor site variable changed from `icdcat_var` to `site_var`
* `pat_status()`, `pat_status_tt()`, `vital_status()`, and `vital_status_tt()`:
  * Capitalized default variable labelling. 
  * This might break code that relied on using the labels coming out of these functions in later filter or mutate functions.
* `ir_crosstab_byfutime()`: 
  * option `futime_breaks` now uses breaks in years instead of months as previously.
  * default `futime_var` is now follow-up time in years
* now requires dplyr version 1.0.0
* now requires tidytable package
* the default option name for tumor site variable changed from `icdcat_var` to `site_var`. This need manual update of function calls of `sir_byfutime()` and `asir()`, if option is specified.
* the default variable name for tumor site in all functions has been changed from `t_icdcat` to `t_site`. So the reference data frames used will need to have a `t_site` column.
* the data.table variants of functions (`renumber_time_id_dt()`, `pat_status_dt()`, `reshape_long_dt()`, `reshape_wide_dt()`, `vital_status_dt()`) have been removed for simplicity, please use tidytable variants, i.e. `reshape_wide_tt()`, `renumber_time_id_tt()`, `pat_status_tt()`, `vital_status_tt()`,  `calc_futime_tt()`, instead. They will give the same data.table output and same performance.

### Bug Fixes
* implement new reliable routine to split df when `reshape_wide()` with option `chunks` is used. Closes #1.
* Sorting of columns in wide datasets by `reshape_wide_tidyr()` and `reshape_wide_tt()` is now preserved. Closes #31.
* ensure sorting in `renumer_time_id()` and make sure that `new_time_id_var` is returned as integer.
* fix bug in `pat_status_*(., check = TRUE)`option
* improve internal tests in `sir_byfutime()` so that PYARs do not get lost before running summary function
* `sir_byfutime()` now also gives correct results if range of `futime_breaks` is not 0-Inf but smaller

# msSPChelpR 0.8.4 - 2020-05-21

### New Features
* add timevar_max option to `renumber_time_id()` function; use sorting by date of diagnosis instead of old time_id_var
* various improvements to  `reshape_wide_tidyr()` function
* various improvements to `reshape_wide_dt()` function which is much faster now and uses `data.table::dcast` instead of `stats::reshape` now
* various improvements to `pat_status()` and `pat_status_dt()` functions
* option summarize_icdcat in `summarize_sir_results()` is now functional
* update vignette `vignette("introduction")`

### Bug Fixes
* fix incomplete check for required variables in `pat_status()` and `pat_status_dt()` functions
* fix error in check for required variables in `renumber_time_id()` that broke functions
* fix bug in check for end of FU time in `pat_status()` and `calc_futime()`
* implement new tidyselect routine using `tidyselect::all_of` in `summarize_sir_results()`


# msSPChelpR 0.8.3

### New Features
* new faster version of reshape_long based on data.table 
* start new vignette on workflow from filtered long dataset to follow-up times `vignette("patstatus_futime")` 

### Bug Fixes
* implement new tidyselect routine using `tidyselect::all_of` for vector-based variable selection
* implement correct referencing in `vital_status_dt` and `pat_status_dt`
* add exports from `data.table`
* update documentation for sir and sir_byfutime functions
* make `reshape_long` function work

# msSPChelpR 0.8.2

# msSPChelpR 0.8.1

### New Features
* new faster version of vital_status function using data.table
* new faster version of pat_status function using data.table

# msSPChelpR 0.8.0

### New Features
* new faster version of reshape_wide_dt function based on data.table and without problematic slices done by reshape_wide
* new faster version of renumber_time_id function based on data.table

# msSPChelpR 0.7.4

### New Features
* new function renumber_time_id

# msSPChelpR 0.7.3

### Bug Fixes
* add check to revert status_var to numeric in case it was created with option as_labelled_factor
* fix label bug in life_var_new

# msSPChelpR 0.7.2
* add option as_labelled_factor to vital_status function
* fix newly introduced error in vital_status function

# msSPChelpR 0.7.1
* fix error in vital_status function by replacing sjlabelled::get_label function

# msSPChelpR 0.7.0
* fix error in pat_status and vital_status functions due to change in sjlabelled package

# msSPChelpR 0.6.10
* rebuild description file and manual

# msSPChelpR 0.6.9

* remove nest_legacy functions and use new tidyr syntax, close #19

# msSPChelpR 0.6.8

* make summarize_sir_results function work without break variables

# msSPChelpR 0.6.7

* for function sir_byfutime ⇒ make option `add_total_row` work, even if option `ybreak_vars = "none"`

# msSPChelpR 0.6.6

* Make use of time_id_var and case_id_var use coherent across reshape functions

# msSPChelpR 0.6.5

* Fixed issue in Namespace

# msSPChelpR 0.6.4

* Added a `NEWS.md` file to track changes to the package.

# msSPChelpR 0.6.3

* add option `futime_breaks = "none"` to `sir_byfutime` function

# major changes in msSPChelpR 0.6.0

*	includes a new function to calculate crude (absolute) incidence rates a tabulate them by whatever number of grouping variables and it can be used as a Table 1 for publications ⇒ The function is called msSPChelpR::ir_crosstab 
*	includes a new function to calculate SIRs (standardized incidence ratios) by whatever strata you desire (unlimited ybreak_vars; one xbreak_var) and additionally customized breaks for follow-up times (default is: to 6 months, .5-1 year, 1-5 years, 5-10 years, >10 years)
⇒ attention, it only makes sense to stratify results (ybreak_vars or xbreak_var) by variables measured at baseline and not for variables that are dependent on the occurrence of an SPC)
⇒ function msSPChelpR::sir_byfutime
⇒ depending on the number of stratification variables you are using, this function may result in a very long results data.frame. So please use it together with the new function msSPChelpR::summarize_sir_results
*	includes a new function to summarize results dataframes from SIR calculations 
*	New reshape functions that are faster and are using less memory 
