## (c) Claus Hunsen, 2016, 2017
## hunsen@fim.uni-passau.de


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## System variables and R settings -----------------------------------------

Sys.setlocale(category = "LC_ALL", locale = "en_US.UTF-8")
Sys.setenv(TZ = "UTC")
options(stringsAsFactors = FALSE)


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
source("util-covariates.R")