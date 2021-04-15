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
requireNamespace("logging") # for logging

#' Helper function to mask all issues in the issue data array.
#'
#' \code{ProjectData$get.issues()} and its cousins returns a dataframe that mixes issue and PR data.
#' This helper function creates a vector of length \code{nrow(issue.data)} which has
#' entry \code{TRUE} iff the corresponding row in \code{issue.data} is an issue.
#'
#' @param issue.data the issue data, returned from calling \code{get.issues()}
#' or \code{get.issues.filtered()} on a project data object
#'
#' @return a vector containing \code{TRUE} or \code{FALSE}
#'
#' @seealso ProjectData$get.issues.filtered()
mask.issues = function(issue.data) {
    return(sapply(issue.data[["issue.type"]], function(tags) {return("issue" %in% tags)}))
}

#' Helper function to mask all pull requests in the issue data frame.
#'
#' \code{ProjectData$get.issues()} returns a dataframe that mixes issue and PR data.
#' This helper function creates a vector of length \code{nrow(issue.data)} which has
#' entry \code{TRUE} iff the corresponding row in \code{issue.data} is a pull request.
#'
#' @param issue.data the pull request data, returned from calling \code{get.issues()}
#' or \code{get.issues.filtered()} on a project data object
#'
#' @return a vector containing \code{TRUE} or \code{FALSE}
#'
#' @seealso ProjectData$get.issues.filtered()
mask.pull.requests = function(issue.data) {
    return(sapply(issue.data[["issue.type"]], function(tags) {return("pull request" %in% tags)}))
}

#' Get and preprocess issue data, removing unnecessary columns and rows we are not interested in.
#'
#' Retained columns are given in \code{retained.cols}, which defaults to
#' \code{author.name}, \code{issue.id} and \code{event.type}.
#'
#' Retained rows depend on the parameter \code{type}. If it is \code{"all"}, then all rows are retained.
#' Otherwise, only the rows containing information about either issues or pull requests are retained.
#'
#' @param proj.data the \code{ProjectData} containing the mail data
#' @param retained.cols the columns to be retained. [default: c("author.name", "issue.id", "event.name")]
#' @param type which issue type to consider.
#'             One of \code{"issues"}, \code{"pull.requests"} or \code{"all"}
#'             [default: "all"]
#'
#' @return a filtered sub-data frame of the unfiltered issue data from \code{proj.data}.
preprocess.issue.data = function(proj.data, retained.cols = c("author.name", "issue.id", "event.name"),
                                 type = c("all", "pull.requests", "issues")) {
  type = match.arg(type)
  df = proj.data$get.issues()

  ## forall vectors k, if nrow(df) == 0, then df[k, ..] fails
  ## so we abort beforehand
  if (nrow(df) == 0) {
      return(df[retained.cols])
  }

  switch (
      type,
      all = {
          df = df[retained.cols]
      },
      issues = {
          df = df[mask.issues(df), retained.cols]
      },
      pull.requests = {
          df = df[mask.pull.requests(df), retained.cols]
      },
      logging::logerror("Requested unknown issue type %s", type)
  )
  return(df)
}



## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Commit-based statistics -------------------------------------------------

## * Count-based statistics ------------------------------------------------

#' Get the commit count per comitter in the given range data, where the committer
#' does not match the author of the respective commits
#'
#' @param range.data The data to count on
#'
#' @return A data frame in descending order by the commit count
get.committer.not.author.commit.count = function(range.data) {
    logging::logdebug("get.committer.not.author.commit.count: starting.")

    ## Get commit data
    commits.df = range.data$get.commits.filtered()

    ## For each commit hash, make sure there is only one row
    commits.df = commits.df[!duplicated(commits.df[["hash"]]), ]

    ## Restrict commits to relevant columns
    commits.df = commits.df[c("author.name", "committer.name")]

    ## Execute a query to get the commit count per author
    res = sqldf::sqldf("SELECT *, COUNT(*) AS `freq` FROM `commits.df`
                       WHERE `committer.name` <> `author.name`
                       GROUP BY `committer.name`, `author.name`
                       ORDER BY `freq` DESC, `author.name` ASC")

    logging::logdebug("get.committer.not.author.commit.count: finished.")
    return(res)
}

#' Get the commit count per person in the given range data for commits where the author equals the committer.
#'
#' @param range.data The data to count on
#'
#' @return A data frame in descending order by the commit count
get.committer.and.author.commit.count = function(range.data) {
    logging::logdebug("get.committer.and.author.commit.count: starting.")

    ## Get commit data
    commits.df = range.data$get.commits.filtered()

    ## For each commit hash, make sure there is only one row
    commits.df = commits.df[!duplicated(commits.df[["hash"]]), ]

    ## Restrict commits to relevant columns
    commits.df = commits.df[c("author.name", "committer.name")]

    ## Execute a query to get the commit count per person
    res = sqldf::sqldf("SELECT *, COUNT(*) AS `freq` FROM `commits.df`
                       WHERE `committer.name` = `author.name`
                       GROUP BY `committer.name`, `author.name`
                       ORDER BY `freq` DESC, `author.name` ASC")

    logging::logdebug("get.committer.and.author.commit.count: finished.")
    return(res)
}

#' Get the commit count per person in the given range data where the person is committer or author or both.
#'
#' @param range.data The data to count on
#'
#' @return A data frame in descending order by the commit count
get.committer.or.author.commit.count = function(range.data) {
    logging::logdebug("get.committer.or.author.commit.count: starting.")

    ## Get commit data
    commits.df = range.data$get.commits.filtered()

    ## For each commit hash, make sure there is only one row
    commits.df = commits.df[!duplicated(commits.df[["hash"]]), ]

    ## Restrict commits to relevant columns
    commits.df = commits.df[c("author.name", "committer.name")]

    ## Execute queries to get the commit count per person
    ungrouped = sqldf::sqldf("SELECT `committer.name` AS `name` FROM `commits.df`
                             WHERE `committer.name` = `author.name`
                                UNION ALL
                             SELECT `author.name` AS `name` FROM `commits.df`
                             WHERE `author.name` <> `committer.name`
                                UNION ALL
                             SELECT `committer.name` AS `name` FROM `commits.df`
                             WHERE `author.name` <> `committer.name`")

    res = sqldf::sqldf("SELECT *, COUNT(*) AS `freq` FROM `ungrouped`
                       GROUP BY `name`
                       ORDER BY `freq` DESC, `name` ASC")

    logging::logdebug("get.committer.or.author.commit.count: finished.")
    return(res)
}


#' Helper function that aggregates counts of things like commits, mails, ... on a per-author basis.
#'
#' For example, called with \code{name = "commit.count"}, \code{data.source = "commits"},
#' \code{grouping.keys = c("committer.name")}, \code{remove.duplicates = TRUE} and
#' \code{remove.duplicates.by = c("hash")}, the returned function will:
#'
#' 1. get the proper data frame (using \code{DATASOURCE.TO.ARTIFACT.FUNCTION}),
#' 2. remove duplicate entries so that there is only one entry per commit hash,
#' 3. project away unneeded columns, leaving only "committer.name",
#' 4. count the commits grouped by the commiter name,
#' 5. return a data frame with columns "commiter.name" and "freq",
#'    which contains the number of commits authored by each author.
#'
#' The signature of the returned function is \code{function(project.data)}.
#'
#' @param name the name the function will be bound to, for logging
#' @param data.source one of \code{"commits"}, \code{"mails"}, \code{"issues"}
#' @param grouping.keys the dataframe keys to group by
#' @param remove.duplicates whether to remove duplicates
#' @param remove.duplicates.by if \code{remove.duplicates}, then the key by which to remove duplicates
#'
#' @return a function that aggregates data according to the above specification contained in a given \code{ProjectData}.
#'         This function itself returns a dataframe consisting of |grouping.keys|+1 columns, the last holding the count,
#'         and the others the respective grouping
group.data.by.key = function(name, data.source = c("commits", "mails", "issues"),
                             grouping.keys, remove.duplicates, remove.duplicates.by) {
    data.source = match.arg(data.source)
    data.extractor = DATASOURCE.TO.ARTIFACT.FUNCTION[[data.source]]
    return(function(proj.data) {
        logging::logdebug("%s: starting", name)

        ## get the data we want to group
        df = proj.data[[data.extractor]]()
        ## if necessary, make sure that there is only one entry for each remove-duplicate key (combination)
        if (remove.duplicates) {
            df = df[!duplicated(df[[remove.duplicates.by]]), ]
        }

        ## throw away unnecessary columns
        df = df[grouping.keys]
        grouping.keys.formatted = paste(grouping.keys, sep="`, `")

        ## execute a query that counts the number of occurrences of the grouping.keys
        stmt = paste0("SELECT `", grouping.keys.formatted, "`, COUNT(*) as `freq` FROM `df`
                       GROUP BY `", grouping.keys.formatted, "` ORDER BY `freq` DESC, `", grouping.keys.formatted, "`")
        logging::logdebug("%s: running SQL %s", name, stmt)
        res = sqldf::sqldf(stmt)

        logging::logdebug("%s: finished", name)
        return(res)
    })
}

#' Get the commit count per committer in the given range data, where the committer
#' may match the author of the respective commits.
#'
#' @param proj.data the data to count on
#'
#' @return a data frame in descending order by the commit count.
get.committer.commit.count = group.data.by.key("get.committer.commit.count", "commits",
                                               c("committer.name"), TRUE, c("hash"))

#' Get the commit count for each author based on the commit data contained in the specified \code{ProjectData}.
#'
#' @param proj.data the \code{ProjectData} containing the commit data
#'
#' @return a dataframe consisting of two columns, the first of which holding the authors' names and the second holding
#'         their respective commit counts
get.author.commit.count = group.data.by.key("get.author.commit.count", "commits",
                                            c("author.name"), TRUE, c("hash"))


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Mail-based statistics ---------------------------------------------------

## * Count-based statistics ------------------------------------------------

#' Get the mail count for each author based on the mail data contained in the specified \code{ProjectData}.
#'
#' @param proj.data the \code{ProjectData} containing the mail data
#'
#' @return a dataframe consisting of two columns, the first of which holding the authors' names and the second holding
#'         their respective mail counts
get.author.mail.count = group.data.by.key("get.author.mail.count", "mails",
                                          c("author.name"), TRUE, c("message.id"))

#' Get the mail-thread count for each author based on the mail data contained in the specified \code{ProjectData}.
#' This is the number of threads the author participated in, i.e., contributed at least one e-mail to.
#'
#' @param proj.data the \code{ProjectData} containing the mail data
#'
#' @return a dataframe consisting of two columns, the first of which holding the authors' names and the second holding
#'         their respective mail thread counts
get.author.mail.thread.count = function(proj.data) {
    logging::logdebug("get.author.mail.thread.count: starting.")

    mails.df = proj.data$get.mails()
    ## Remove unnecessary rows and columns
    mails.df = mails.df[!duplicated(mails.df[["message.id"]]), ]
    mails.df = mails.df[c("author.name", "message.id", "thread")]
    ## Only count each thread once
    stmt = "SELECT `author.name`, COUNT(DISTINCT thread) as `freq` FROM `mails.df`
                             GROUP BY `author.name` ORDER BY `freq` DESC, `author.name` ASC"
    logging::logdebug("get.author.mail.thread.count: running SQL %s", stmt)
    res = sqldf::sqldf(stmt)
    logging::logdebug("get.author.mail.thread.count: finished")
    return(res)
}

## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Issue-/PR-based statistics ----------------------------------------------

## * Count-based statistics ------------------------------------------------

#' Get the issue/pr count for each author based on the issues data contained in the specified \code{ProjectData}.
#' The issue count here is the number of issues the author participated in (which can mean anything,
#' from commenting to closing to assigning the issue to others, to labeling, referencing it in other issues,
#' adding commits, ...).
#'
#' The type argument specifies whether we count PRs alone, issues alone, or both (\code{"all"}).
#'
#' @param proj.data the \code{ProjectData} containing the issue data
#' @param type which issue type to consider. see \code{preprocess.issue.data}
#'             One of \code{"issues"}, \code{"pull.requests"} or \code{"all"}
#'             [default: "all"]
#'
#' @return a dataframe consisting of two columns, the first of which holding the authors' names and the second holding
#'         their respective issue counts
get.author.issue.count = function(proj.data, type = "all") {
    logging::logdebug("get.author.issue.count: starting.")
    df = preprocess.issue.data(proj.data, type = type)
    ## count distinct since an author may appear in the same issues at the same time
    stmt = "SELECT `author.name`, COUNT( DISTINCT `issue.id`) as `freq` FROM `df`
                             GROUP BY `author.name` ORDER BY `freq` DESC, `author.name` ASC"
    res = sqldf::sqldf(stmt)
    logging::logdebug("get.author.issue.count: finished")
    return(res)
}

#' Get the issue/pr count for each author based on the issues data contained in the specified \code{ProjectData}.
#' The issue count here is the number of issues the author created.
#'
#' The type argument specifies whether we count PRs alone, issues alone, or both (\code{"all"}).
#'
#' @param proj.data the \code{ProjectData} containing the issue data
#' @param type which issue type to consider. see \code{preprocess.issue.data}
#'             One of \code{"issues"}, \code{"pull.requests"} or \code{"all"}
#'             [default: "all"]
#'
#' @return a dataframe consisting of two columns, the first of which holding the authors' names and the second holding
#'         their respective issue counts
get.author.issues.created.count = function(proj.data, type = "all") {
    logging::logdebug("get.author.issues.created.count: starting.")
    df = preprocess.issue.data(proj.data, type = type)
    ## count distinct since an author may appear in the same issues at the same time
    stmt = "SELECT `author.name`, COUNT( DISTINCT `issue.id`) as `freq` FROM `df`
                             WHERE `event.name` = 'created'
                             GROUP BY `author.name` ORDER BY `freq` DESC, `author.name` ASC"
    res = sqldf::sqldf(stmt)
    logging::logdebug("get.author.issues.created.count: finished")
    return(res)
}

#' Get the issue/pr count for each author based on the issues data contained in the specified \code{ProjectData}.
#' The issue count here is the number of issues the author commented in.
#'
#' The type argument specifies whether we count PRs alone, issues alone, or both (\code{"all"}).
#'
#' @param proj.data the \code{ProjectData} containing the issue data
#' @param type which issue type to consider. see \code{preprocess.issue.data}
#'             One of \code{"issues"}, \code{"pull.requests"} or \code{"all"}
#'             [default: "all"]
#'
#' @return a dataframe consisting of two columns, the first of which holding the authors' names and the second holding
#'         their respective issue counts
get.author.issues.commented.in.count = function(proj.data, type = "all") {
    logging::logdebug("get.author.issues.commented.in.count: starting.")
    df = preprocess.issue.data(proj.data, type = type)
    ## count distinct since an author may appear in the same issues at the same time
    stmt = "SELECT `author.name`, COUNT( DISTINCT `issue.id`) as `freq` FROM `df`
                             WHERE `event.name` = 'commented'
                             GROUP BY `author.name` ORDER BY `freq` DESC, `author.name` ASC"
    res = sqldf::sqldf(stmt)
    logging::logdebug("get.author.issues.commented.in.count: finished")
    return(res)
}

#' Get the issue/pr comment count for each author based on the issues data contained in the specified \code{ProjectData}.
#' The issue comment count here is the number of comments the author created summed across all issues
#'
#' The type argument specifies whether we count PRs alone, issues alone, or both (\code{"all"}).
#'
#' @param proj.data the \code{ProjectData} containing the issue data
#' @param type which issue type to consider. see \code{preprocess.issue.data}
#'             One of \code{"issues"}, \code{"pull.requests"} or \code{"all"}
#'             [default: "all"]
#'
#' @return a dataframe consisting of two columns, the first of which holding the authors' names and the second holding
#'         their respective comment counts
get.author.issue.comment.count = function(proj.data, type = "all") {
    logging::logdebug("get.author.issue.comment.count: starting.")
    df = preprocess.issue.data(proj.data, type = type)
    stmt = "SELECT `author.name`, COUNT(*) as `freq` FROM `df`
                             WHERE `event.name` = 'commented'
                             GROUP BY `author.name` ORDER BY `freq` DESC, `author.name` ASC"
    res = sqldf::sqldf(stmt)
    logging::logdebug("get.author.issue.comment.count: finished")
    return(res)
}
