# kgroups 0.2.1

* Convert all but top level function and S3 methods to Rcpp.

# kgroups 0.2.0

* Completely revised implementation.
* Much faster R version than original R version released on GitHub 2017 (last updated 2018-06-30 version 0.1.3).
* This version 0.2.0 is 100 or more times faster in benchmarks.
* Expect further improvement when compiled implementation is complete. 
* Changes in arguments:
    - x can now optionally be dist object, which removed need for alpha argument.
    - alpha argument and support for it are removed - if user requires it, simply pre-compute the distances and apply the exponent to the distances in advance.
    - new optional argument clus allows starting with a given pre-initialized clustering vector.
    - method is now only "point" so the method argument and all support for moving pairs is removed from this version. To use pairs method, install version 0.1.3 from the branch. 
* No dependence or reverse dependence with energy package in version 0.2.0.


# kgroups 0.1.3 

* Final update. This branch will not be updated after 2018-06-30. It is replaced by a completely rewritten R version. Some features will no longer be supported; other features will be added. The new code is faster.
* Fixed a bug in OnePass.R at updating step for within cluster (bug inherited from original code of S. Li).
* other minor internal changes

# kgroups 0.1.2

* Fixed iteration counter on 2018-05-19
* Added the NEWS file

# kgroups 0.1.1

* Initial release on GitHub 2017-11-14 repackaging and documenting by M. Rizzo of code developed by Songzi Li in 2014.   

