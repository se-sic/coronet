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
## Copyright 2024 by Maximilian Löffler <s8maloef@stud.uni-saarland.de>
## All Rights Reserved.

## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Initialization ----------------------------------------------------------

source("util-init.R")
source("tests/testing-utils.R")


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Logging -----------------------------------------------------------------

library("methods") # to prevent weird error during logger initialization (see #153)
library("logging")
logging::basicConfig(level = "WARN")
options(mc.cores = 1L)


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Debug information -------------------------------------------------------

logging::loginfo("Session information:")
sessionInfo()


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Run tests in subfolder 'tests' ------------------------------------------

logging::loginfo("Generating coverage report.")

## load packages 'testthat' and 'patrick'
library("testthat")
requireNamespace("patrick")
requireNamespace("covr")

## define paths
test.dir = c("./tests")
code.dir = c(".")
excluded.code.files = c("./tests.R", "./coverage.R", "./showcase.R", "./install.R", "./util-init.R")
excluded.test.files = c()

## obtain files
test.files = unlist(sapply(test.dir, list.files, pattern = "\\.R$", full.names = TRUE))
test.files = test.files[!test.files %in% excluded.test.files]
code.files = unlist(sapply(code.dir, list.files, pattern = "\\.R$", full.names = TRUE))
code.files = code.files[!code.files %in% excluded.code.files]

## adjust data path prefix when generating coverage reports
DATA.PATH.PREFIX = "./tests"

## generate and save coverage report
report = covr::file_coverage(source_files = code.files, test_files = test.files)
covr::to_cobertura(report)
