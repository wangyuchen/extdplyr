## Test environments
* local OS X install, R 3.3.2
* ubuntu 12.04 (on travis-ci), R 3.3.2
* win-builder (release and devel)

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTES on win-builder:

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Yuchen Wang <ycwang0712@gmail.com>'

Days since last update: 4
```

I found a problem that could cause `ind_to_char_` to fail when data are grouped as a `grouped_df` object from `dplyr`. This has been fixed in the latest version, and several tests are added to make sure the functions work with various data structures.

