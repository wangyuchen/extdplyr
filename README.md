`extdplyr` is an extension package for data manipulation based on `dplyr` and `tidyr`.

[![Travis-CI Build Status](https://travis-ci.org/wangyuchen/extdplyr.svg?branch=master)](https://travis-ci.org/wangyuchen/extdplyr)


If `dplyr` is a grammar for data manipulation, `extdplyr` is like a short paragraph written in `dplyr`. `extdplyr` extends `dplyr` and `tidyr` verbs to some common "routines" that manipulate datasets. It uses the same interface and preserves all features from `dplyr` like fast performance and various data sources.

### Motivation
`dplyr` is powerful but also restrained in that it aims to provide the most important tools, but not to suit every needs. After several years of extensive use of `dplyr`, I found that some combinations of `dplyr` verbs become too frequent in my code that I wanted to extract them as functions. This is the origin of those routines in `extdplyr`.


Also there are some cases where the operations don't comform to `dplyr`'s rules. Naturally one wouldn't expect these operations to be included into `dplyr`, but they can be implemented here for the occasional use.


