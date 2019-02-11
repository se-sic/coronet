## This file is part of codeface-extraction-r, which is free software: you
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
## Copyright 2016-2019 by Claus Hunsen <hunsen@fim.uni-passau.de>
## Copyright 2017 by Christian Hechtl <hechtl@fim.uni-passau.de>
## Copyright 2017 by Raphael Nömmer <noemmer@fim.uni-passau.de>
## Copyright 2017 by Sofie Kemper <kemperso@fim.uni-passau.de>
## Copyright 2017 by Felix Prasse <prassefe@fim.uni-passau.de>
## All Rights Reserved.


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## System variables and R settings -----------------------------------------

## * Locale ----------------------------------------------------------------

if (Sys.info()['sysname'] == "Windows") {
    Sys.setlocale(category = "LC_ALL", locale = "english-us")
} else {
    Sys.setlocale(category = "LC_ALL", locale = "en_US.UTF-8")
}

## * Universal time zone ---------------------------------------------------

TIMEZONE = "UTC"
Sys.setenv(TZ = TIMEZONE)

## * Other stuff -----------------------------------------------------------

options(stringsAsFactors = FALSE)

## for proper initialization of the package 'logging', the base package 'methods' needs
## to be attached properly (see #153)

library(methods)


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Sourcing library files --------------------------------------------------

source("util-misc.R")
source("util-conf.R")
source("util-read.R")
source("util-data.R")
source("util-networks.R")
source("util-split.R")
source("util-motifs.R")
source("util-bulk.R")
source("util-plot.R")
source("util-core-peripheral.R")
source("util-networks-metrics.R")
source("util-networks-covariates.R")
