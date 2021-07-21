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
## Copyright 2014 by Roger Meier <roger@bufferoverflow.ch>
## Copyright 2015 by Andreas Ringlstetter <andreas.ringlstetter@gmail.com>
## Copyright 2015 by Wolfgang Mauerer <wolfgang.mauerer@oth-regensburg.de>
## Copyright 2015-2017 by Claus Hunsen <hunsen@fim.uni-passau.de>
## Copyright 2017 by Thomas Bock <bockthom@fim.uni-passau.de>
## Copyright 2020-2021 by Thomas Bock <bockthom@cs.uni-saarland.de>
## Copyright 2019 by Anselm Fehnker <fehnker@fim.uni-passau.de>
## All Rights Reserved.
##
## Adapted from https://github.com/siemens/codeface/blob/be382e9171fb91b4aa99b99b09b2ef64a6dba0d5/packages.r
## to our needs.


packages = c(
    "yaml",
    "R6",
    "igraph",
    "plyr",
    "parallel",
    "logging",
    "sqldf",
    "data.table",
    "reshape2",
    "testthat",
    "patrick",
    "ggplot2",
    "ggraph",
    "markovchain",
    "lubridate",
    "viridis",
    "jsonlite",
    "rTensor",
    "Matrix"
)


filter.installed.packages = function(packageList)  {
    if ("-f" %in% commandArgs(trailingOnly = TRUE)) {
        return(packageList)
    } else {
        return(packageList[which(packageList %in% installed.packages()[, 1] == FALSE)])
    }
}


p = filter.installed.packages(packages)
if (length(p) > 0) {
    print(sprintf("Installing package '%s'.", p))

    ## set dependencies to 'NA' to install only necessary dependencies (i.e., "Depends", "Imports", "LinkingTo")
    install.packages(p, dependencies = NA, verbose = TRUE, quiet = TRUE)
}
