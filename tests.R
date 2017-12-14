## (c) Claus Hunsen, 2017
## hunsen@fim.uni-passau.de


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Initialization ----------------------------------------------------------

source("util-init.R")


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Logging -----------------------------------------------------------------

library("logging")
logging::basicConfig(level = "DEBUG")
assign("last.warning", NULL, envir = baseenv())
options(mc.cores = 1L)


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Libraries ---------------------------------------------------------------

requireNamespace("testthat")


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Run tests in subfolder 'tests' ------------------------------------------

do.tests <- function(dir) {
    res <- testthat::test_dir(dir, reporter = "check")
    if (length(res$failures) > 0) {
        cat(str_c("Some R tests failed for directory '", dir, "'"))

        for (i in 1:length(res$failures)) {
            cat(str_c("Failing test ", i, ": ", res$failures[[i]], "\n"))
        }

        return(FALSE)
    }
    return(TRUE)
}

res <- sapply(c("./tests"), function(dir) {
    do.tests(dir)
})

if (!all(res)) {
    stop("Error exiting because of test failures")
}
