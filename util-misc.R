## (c) Claus Hunsen, 2016, 2017
## hunsen@fim.uni-passau.de
## (c) Raphael Nömmer, 2017
## noemmer@fim.uni-passau.de
## (c) Christian Hechtl, 2017
## hechtl@fim.uni-passau.de
## (c) Felix Prasse, 2017
## prassefe@fim.uni-passau.de


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Libraries ---------------------------------------------------------------

requireNamespace("plyr") # for rbind.fill and dlply
requireNamespace("parallel") # for parallel computation
requireNamespace("igraph") # networks
requireNamespace("logging") # for logging


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Network data ------------------------------------------------------------

#' Construct an edge list for the given network, with timestamps as an extra attribute column.
#'
#' The 'date' attribute has to be added during network construction as default edge attribute
#' in order to avoid problems accessing it.
#'
#' @param net the given network
#'
#' @return the new edgelist
get.edgelist.with.timestamps = function(net) {
  ## get edge list as data.frame
  edges = as.data.frame(igraph::get.edgelist(net))
  colnames(edges) = c("from", "to")
  ## get timestamps
  dates = igraph::get.edge.attribute(net, "date")
  ## bind everything together
  edges = cbind(edges, date = dates)

  return(edges)
}


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Stacktrace --------------------------------------------------------------

#' Get the stacktrace.
#'
#' @param calls the calls of the stacktrace
#'
#' @return the built stacktrace
get.stacktrace = function(calls) {
    lapply(calls, deparse)
}


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Intermediate data -------------------------------------------------------

#' Save the given 'variable' on the file system (in 'dump.path') if it does not exist already,
#' and load the saved data if it exists.
#'
#' With skip, the check for data existance can be skipped (i.e., force a re-save).
#'
#' This function is for repetitive runs of the same script: It saves intermediate data to disk and
#' loads it from a previous runs if possible. This way, computation time can be saved.
#'
#' @param variable a character naming the data variable to be saved to disk
#' @param dump.path the path where the data is to be saved
#' @param if.not.found if the data does not exist on disk, run this function ('variable' must be a variable there!)
#' @param skip re-save although data exists on the disk?
#'
#' @return the data computed by 'if.not.found' or loaded from 'dump.path'
save.and.load = function(variable, dump.path, if.not.found, skip = FALSE) {
    if (!skip && file.exists(dump.path)) {
        logging::logdebug("Load %s from previously dumped object: %s.", variable, dump.path)
        load(file = dump.path) # load the list named "variable" into the current environment
    } else {
        res = if.not.found()

        assign(variable, res) # rewrite to variable name
        rm(res) # clear memory

        logging::logdebug("Dumping object %s to %s.", variable, dump.path)
        save(list = variable, file = dump.path) # save automatically
    }

    return(get0(variable))
}


#' Calculate the bounds of a range from its name.
#' @param range The range name
#'
#' @return Returns a vector with two entries (start, end) of type POSIXct if input was a date;
#'         or of type character if input was a commit hash or version;
#'         or NULL if the string could not be parsed
get.range.bounds = function(range) {

    ## the patterns to test with appropriate conversions (if any)
    tests = list(
        ## date format (assuming dates are GMT)
        ## Add zeros for dates without time. Will be ignored if there is a time already.
        c("\\d{4}-\\d{2}-\\d{2}(\\s\\d{2}:\\d{2}:\\d{2})?", function(x) as.POSIXct(paste(x, " 00:00:00"))),

        ## commit format
        c("[A-F0-9a-f]{40}", identity),

        ## version format
        c("([A-Za-z0-9]+[\\._]?)+", identity)
    )

    for(pattern in tests) {
        start.end = regmatches(range, gregexpr(pattern = pattern[[1]], range))[[1]]

        if (length(start.end) == 2) {
            return (pattern[[2]](start.end))
        }
    }

    return (NULL)
}
