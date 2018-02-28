## Copyright 2014 by Roger Meier <roger@bufferoverflow.ch>
## Copyright 2015 by Andreas Ringlstetter <andreas.ringlstetter@gmail.com>
## Copyright 2015 by Wolfgang Mauerer <wolfgang.mauerer@oth-regensburg.de>
## Copyright 2015-2017 by Claus Hunsen <hunsen@fim.uni-passau.de>

## Adapted from https://github.com/siemens/codeface/blob/be382e9171fb91b4aa99b99b09b2ef64a6dba0d5/packages.r

pacakges = c(
    "yaml",
    "R6",
    "igraph",
    "plyr",
    "parallel",
    "logging",
    "sqldf",
    "testthat",
    "ggplot2",
    "ggraph",
    "markovchain",
    "lubridate"
)


filter.installed.packages = function(packageList)  {
    if("-f" %in% commandArgs(trailingOnly = TRUE)) {
        return(packageList)
    } else {
        return(packageList[which(packageList %in% installed.packages()[, 1] == FALSE)])
    }
}


p = filter.installed.packages(pacakges)
if(length(p) > 0) {
    print(sprintf("Installing package '%s'.", p))
    install.packages(p, dependencies = TRUE, verbose = FALSE, quiet = FALSE)
}
