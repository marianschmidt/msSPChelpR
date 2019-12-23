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
