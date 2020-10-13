# CRAN comments for msSPChelpR

## Resubmission

### Reviewer comments on initial submission by CRAN 2020-10-05

 * Please omit the redundant "R" from your title.

 * The Description field is intended to be a (one paragraph) description of what the package does and why it may be useful. Please add more details about the package functionality and implemented methods in your Description text.

 * If there are references describing the methods in your package, please add these in the description field of your DESCRIPTION file in the form authors (year) <doi:...> authors (year) <arXiv:...> authors (year, ISBN:...) or if those are not available: <https:...> with no space after 'doi:', 'arXiv:', 'https:' and angle brackets for auto-linking. (If you want to add a title as well please put it in quotes: "Title")

 * Please add a few more small executable examples in your Rd-files to illustrate the use of the exported function but also enable automatic testing.


## Initial submission notes
This is a new package that provides functions for Second Primary Cancer analyses.

## Test environments
* local Windows 10, R 4.0.2, x64
* win-builder (devel and release)
* r-hub (Windows Server 2008 R2 SP1, R-devel, 32/64 bit)
* r-hub (Fedora Linux, R-devel, clang, gfortran)
* r-hub (Ubuntu Linux 16.04 LTS, R-release, GCC)
* r-hub (Ubuntu Linux 16.04 LTS, R-devel with rchk)
* travis-ci (Ubuntu 16.04.6 LTS, xenial, R 4.0.2_1_amd64)

## R CMD check results
0 errors | 0 warnings | 2 notes

* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Marian Eberl’
New submission

* checking for future file timestamps ... NOTE
unable to verify current time

## Downstream dependencies
There are currently no downstream dependencies for this package.
