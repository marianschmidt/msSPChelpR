# msSPChelpR 0.8.4

##New Features
* add timevar_max option to `renumber_time_id()` function; use sorting by date of diagnosis instead of old time_id_var
* various improvements to  `reshape_wide_tidyr()` function
* various improvements to `reshape_wide_dt()` function which is much faster now and uses `data.table::dcast` instead of `stats::reshape` now

##Bug Fixes
* fix incomplete check for required variables in `pat_status()` and `pat_status_dt()` functions
* fix error in check for required variables in `renumber_time_id()` that broke functions
* fix bug in check for end of FU time in `pat_status()` and `calc_futime()`


# msSPChelpR 0.8.3

##New Features
* new faster version of reshape_long based on data.table 
* start new vignette on workflow from filtered long dataset to follow-up times `vignette("patstatus_futime")` 

##Bug Fixes
* implement new tidyselect routine using `tidyselect::all_of` for vector-based variable selection
* implement correct referencing in `vital_status_dt` and `pat_status_dt`
* add exports from `data.table`
* update documentation for sir and sir_byfutime functions
* make `reshape_long` function work

# msSPChelpR 0.8.2

# msSPChelpR 0.8.1

##New Features
* new faster version of vital_status function using data.table
* new faster version of pat_status function using data.table

# msSPChelpR 0.8.0

##New Features
* new faster version of reshape_wide_dt function based on data.table and without problematic slices done by reshape_wide
* new faster version of renumber_time_id function based on data.table

# msSPChelpR 0.7.4

##New Features
* new function renumber_time_id

# msSPChelpR 0.7.3

##Bug Fixes
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

* for function sir_byfutime -> make option `add_total_row` work, even if option `ybreak_vars = "none"`

# msSPChelpR 0.6.6

* Make use of time_id_var and case_id_var use coherent across reshape funtions

# msSPChelpR 0.6.5

* Fixed issue in Namespace

# msSPChelpR 0.6.4

* Added a `NEWS.md` file to track changes to the package.

# msSPChelpR 0.6.3

* add option `futime_breaks = "none"` to `sir_byfutime` function

# major changes in msSPChelpR 0.6.0

*	includes a new function to calculate crude (absolute) incidence rates a tabulate them by whatever number of grouping variables and it can be used as a Table 1 for publications --> The function is called msSPChelpR::ir_crosstab 
*	includes a new function to calculate SIRs (standardized incidence ratios) by whatever strata you desire (unlimited ybreak_vars; one xbreak_var) and additionally customized breaks for follow-up times (default is: to 6 months, .5-1 year, 1-5 years, 5-10 years, >10 years)
--> attention, it only makes sense to stratify results (ybreak_vars or xbreak_var) by variables measured at baseline and not for variables that are dependent on the occurrence of an SPC)
--> function msSPChelpR::sir_byfutime
--> depending on the number of stratification variables you are using, this function may result in a very long results data.frame. So please use it together with the new function msSPChelpR::summarize_sir_results
*	includes a new function to summarize results dataframes from SIR calculations 
*	New reshape functions that are faster and are using less memory 
