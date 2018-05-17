Dear CRAN Maintainers,

We are pleased to provide an update to EcoNetGen package, as 
detailed in NEWS.md:


* Add `netsampler()` routine
* Modify user interface for `netgen()` to provide more self-explanatory argument names.
* Ensure that FORTRAN correctly obeys `set.seed()` by always using the R random number generator. 

## Test environments

* local OS X install, R 3.5.0
* ubuntu 14.04 (on travis-ci), R 3.5.0
* win-builder (devel and release)
* Dockerized valgrind tests (debian Linux)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

There are no reverse dependencies.


