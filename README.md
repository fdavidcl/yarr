yarr
====

*Yet Another ARFF Reader*

[![R language](https://img.shields.io/badge/language-R-lightgrey.svg)](https://www.r-project.org/)
[![Downloads](https://cranlogs.r-pkg.org/badges/yarr)](https://cranlogs.r-pkg.org/downloads/total/last-month/yarr)
[![Travis](https://img.shields.io/travis/fdavidcl/yarr/master.svg)](https://travis-ci.org/fdavidcl/yarr)
[![GPL v3 license](https://img.shields.io/github/license/fdavidcl/yarr.svg)](https://www.gnu.org/licenses/gpl.html)

---

So you need to read an ARFF file. You can use:

- `foreign::read.arff`, but only on dense format.
- `farff::readARFF`, but only on dense format.
- `RWeka::read.arff`, but you need to configure Java.

`yarr::read.arff` can read dense **and sparse** ARFF files, and it's implemented in pure R.

The implementations in R are derivatives of those in [`mldr`](https://github.com/fcharte/mldr) and [`mldr.datasets`](https://github.com/fcharte/mldr.datasets), packages for management of multilabel learning datasets.


### Usage

```r
remotes::install_github("fdavidcl/yarr")
library(yarr)
download.file("https://www.openml.org/data/download/1681111/phpEUwA95", "dexter.arff")
dexter <- read.arff("dexter.arff")
str(dexter)
# 'data.frame':	600 obs. of  20001 variables:
#   $ V0    : num  1 1 1 1 1 1 1 1 1 1 ...
# $ V1    : num  1 1 1 1 1 1 1 1 1 1 ...
# $ V2    : num  1 1 1 1 1 1 1 1 1 1 ...
# $ V3    : num  1 1 1 1 1 1 1 1 1 1 ...
# $ V4    : num  1 1 1 1 1 1 1 1 1 1 ...
# $ V5    : num  1 1 1 1 1 1 1 1 1 1 ...
# $ V6    : num  1 1 1 1 1 1 1 1 1 1 ...
# $ V7    : num  1 1 1 1 1 1 1 1 1 1 ...
# $ V8    : num  1 1 1 1 1 1 1 1 1 1 ...
# $ V9    : num  1 1 1 1 1 1 1 1 1 1 ...
# [list output truncated]
# - attr(*, "relation")= chr "@relation dexter"
# - attr(*, "variables")= Named chr  "numeric" "numeric" "numeric" "numeric" ...
# ..- attr(*, "names")= chr  "V0" "V1" "V2" "V3" ...
# - attr(*, "name")= chr "dexter"
relation(dexter)
# [1] "dexter"
write.arff(dexter, "dexter-new.arff")
```

### Unsupported stuff

- Date attributes are read as mere strings
