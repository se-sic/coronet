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

## Copyright 2021 by Johannes Hostert <s8johost@stud.uni-saarland.de>
## All Rights Reserved.
##



## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Libraries ---------------------------------------------------------------

requireNamespace("sqldf") # for SQL-selections on data.frames
requireNamespace("igraph") # for calculation of network metrics (degree, eigen-centrality)
requireNamespace("markovchain") # for role stability analysis
requireNamespace("logging") # for logging


#' Get and preprocess issue data, removing unnecessary columns and rows we are are not interested in.
#'
#' Retained rows are given in \code{retained.rows}, which defaults to
#' \code{author.name}, \code{issue.id} and \code{"event.type"}.
#'
#' Retained colums depend on type. If it is \code{"all"}, then all rows are retained.
#' Otherwise, only the rows containing information about either issues or pull requests are retained.
#'
#' @param proj.data the \code{ProjectData} containing the mail data
#' @param retained.rows the rows to be retained. [default: "c("author.name", "issue.id", "event.name")"]
#' @param type which issue type to consider.
#'             One of \code{"issues"}, \code{"pull.requests"} or \code{"all"}
#'             [default: "all"]
#'
#'
preprocess.issue.data = function(proj.data, retained.rows = c("author.name", "issue.id", "event.name"),
                                 type = c("all", "pull.requests", "issues")) {
  type = match.arg(type)
  df = proj.data$get.issues()
  ## if k is a list, and nrow(df) == 0, then df[k, ..] fails
  ## so we abort beforehand
  if (nrow(df) == 0) {
    return (df[retained.rows]);
  }
  switch (type,
          all = {
            df = df[retained.rows]
          },
          issues = {
            df = df[sapply(df[["issue.type"]], function (k) {return ("issue" %in% k)}), retained.rows]
          },
          pull.requests = {
            df = df[sapply(df[["issue.type"]], function (k) {return ("pull request" %in% k)}), retained.rows]
          },
          logging::logerror("Requested unknown issue type %s", type)
  )
  return(df)
}
