## This file is part of coronet, which is free software: you
## can redistribute it and/or modify it under the terms of the GNU General
## Public License as published by  the Free Software Foundation, version 2.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License along
## with this program; if not, write to the Free Software Foundation, Inc.,
## 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
##
## Copyright 2017, 2019 by Claus Hunsen <hunsen@fim.uni-passau.de>
## Copyright 2020-2021 by Thomas Bock <bockthom@cs.uni-saarland.de>
## All Rights Reserved.

## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Initialization ----------------------------------------------------------

source("util-init.R")


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Logging -----------------------------------------------------------------

library("methods") # to prevent weird error during logger initialization (see #153)
library("logging")
logging::basicConfig(level = "DEBUG")
options(mc.cores = 1L)


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Debug information -------------------------------------------------------

logging::loginfo("Session information:")
sessionInfo()


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Run tests in subfolder 'tests' ------------------------------------------

logging::loginfo("Running test suite.")

## load packages 'testthat' and 'patrick'
requireNamespace("testthat")
requireNamespace("patrick")

## starting tests
do.tests = function(dir) {
    res = testthat::test_dir(dir, reporter = "check")
    if (length(res[["failures"]]) > 0) {
        cat(str_c("Some R tests failed for directory '", dir, "'"))

        for (i in 1:length(res[["failures"]])) {
            cat(str_c("Failing test ", i, ": ", res[["failures"]][[i]], "\n"))
        }

        return(FALSE)
    }
    return(TRUE)
}

res = sapply(c("./tests"), function(dir) {
    do.tests(dir)
})

if (!all(res)) {
    stop("Error exiting because of test failures")
}
