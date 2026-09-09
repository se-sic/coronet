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

## Copyright 2017 by Ferdinand Frank <frankfer@fim.uni-passau.de>
## Copyright 2017 by Sofie Kemper <kemperso@fim.uni-passau.de>
## Copyright 2017-2020 by Claus Hunsen <hunsen@fim.uni-passau.de>
## Copyright 2017 by Felix Prasse <prassefe@fim.uni-passau.de>
## Copyright 2018 by Klara Schlüter <schluete@fim.uni-passau.de>
## Copyright 2019 by Jakob Kronawitter <kronawij@fim.uni-passau.de>
## Copyright 2021 by Johannes Hostert <s8johost@stud.uni-saarland.de>
## Copyright 2021 by Christian Hechtl <hechtl@cs.uni-saarland.de>
## Copyright 2022 by Jonathan Baumann <joba00002@stud.uni-saarland.de>
## Copyright 2024 by Thomas Bock <bockthom@cs.uni-saarland.de>
## Copyright 2026 by Thomas Bock <bockthom@cmu.edu>
## Copyright 2025 by Leo Sendelbach <s8lesend@stud.uni-saarland.de>
## All Rights Reserved.

## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Libraries ---------------------------------------------------------------

requireNamespace("sqldf") # for SQL-selections on data.frames
requireNamespace("logging") # for logging
requireNamespace("tm") # for NLP functionalities
requireNamespace("SnowballC") # for text stemming used by NLP package "tm"
requireNamespace("textstem") # for lemmatization
requireNamespace("parallel") # for parallel computation

#' Helper function to mask all issues in the issue data frame.
#'
#' \code{ProjectData$get.issues.unfiltered()} returns a dataframe that mixes issue and PR data.
#' This helper function creates a vector of length \code{nrow(issue.data)} which has
#' entry \code{TRUE} iff the corresponding row in \code{issue.data} is an issue.
#'
#' @param issue.data the issue data, returned from calling \code{get.issues.unfiltered()}
#'                   or \code{get.issues()} on a project data object
#'
#' @return a vector containing \code{TRUE} or \code{FALSE}
#'
#' @seealso ProjectData$get.issues()
#' @seealso ProjectData$get.issues.unfiltered()
mask.issues = function(issue.data) {
    return(sapply(issue.data[["issue.type"]], function(tags) {return("issue" %in% tags)}))
}

#' Helper function to mask all pull requests in the issue data frame.
#'
#' \code{ProjectData$get.issues.unfiltered()} returns a dataframe that mixes issue and PR data.
#' This helper function creates a vector of length \code{nrow(issue.data)} which has
#' entry \code{TRUE} iff the corresponding row in \code{issue.data} is a pull request.
#'
#' @param issue.data the pull request data, returned from calling \code{get.issues.unfiltered()}
#'                   or \code{get.issues()} on a project data object
#'
#' @return a vector containing \code{TRUE} or \code{FALSE}
#'
#' @seealso ProjectData$get.issues()
#' @seealso ProjectData$get.issues.unfiltered()
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
#' Note that we preprocess the unfiltered issue data, since common filtering options typically
#' strip out some of the data we might explicitly want to retain.
#'
#' @param proj.data the \code{ProjectData} containing the mail data
#' @param retained.cols the columns to be retained. [default: c("author.name", "issue.id", "event.name")]
#' @param type which issue type to consider.
#'             One of \code{"issues"}, \code{"pull.requests"} or \code{"all"}
#'             [default: "all"]
#' @param use.unfiltered.data whether to use the unfiltered issue data, i.e. \code{proj.data$get.issues.unfiltered()}
#'                            instead of \code{proj.data$get.issues()} [default: FALSE]
#'
#' @return a filtered sub-data frame of the unfiltered issue data from \code{proj.data}.
preprocess.issue.data = function(proj.data, retained.cols = c("author.name", "issue.id", "event.name"),
                                 type = c("all", "pull.requests", "issues"), use.unfiltered.data = FALSE) {
  type = match.arg(type)
  df = if (use.unfiltered.data) proj.data$get.issues.unfiltered() else proj.data$get.issues()

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
#' @param data.source one of \code{"commits"}, \code{"mails"}, \code{"issues"} [default: "commits"]
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


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Commit-based statistics -------------------------------------------------


#' Get the commit count for each author based on the commit data contained in the specified \code{ProjectData}.
#'
#' @param proj.data the \code{ProjectData} containing the commit data
#'
#' @return a dataframe consisting of two columns, the first of which holding the authors' names and the second holding
#'         their respective commit counts
get.author.commit.count = group.data.by.key("get.author.commit.count", "commits",
                                            c("author.name"), TRUE, c("hash"))

#' Get the commit count for each committer based on the commit data contained in the specified \code{ProjectData}.
#' The count is aggregated like in \code{get.author.commit.count}, but based on the "committer" commit attribute
#'
#' @param proj.data the data to count on
#'
#' @return a data frame in descending order by the commit count.
get.committer.commit.count = group.data.by.key("get.committer.commit.count", "commits",
                                               c("committer.name"), TRUE, c("hash"))



#' Get the commit count for each committer based on the commit data contained in the specified \code{ProjectData}.
#' The count is aggregated like in \code{get.author.commit.count}, but based on the "committer" commit attribute.
#' However, only commits where the commiter is *not* the author are considered.
#'
#' @param proj.data the data to count on
#'
#' @return a data frame in descending order by the commit count.
get.committer.not.author.commit.count = function(range.data) {
    logging::logdebug("get.committer.not.author.commit.count: starting.")

    ## Get commit data
    commits.df = range.data$get.commits()

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

#' Get the commit count for each person based on the commit data contained in the specified \code{ProjectData}.
#' The count is aggregated like in \code{get.author.commit.count}, but only considers commits where the "committer" and
#' "author" fields are identical.
#'
#' @param proj.data the data to count on
#'
#' @return a data frame in descending order by the commit count.
get.committer.and.author.commit.count = function(range.data) {
    logging::logdebug("get.committer.and.author.commit.count: starting.")

    ## Get commit data
    commits.df = range.data$get.commits()

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

#' Get the commit count for each person based on the commit data contained in the specified \code{ProjectData}.
#' The count is aggregated like in \code{get.author.commit.count}, but only considers all commits where a person is
#' "committer" or "author" (or both, but one suffices).
#'
#' @param proj.data the data to count on
#'
#' @return a data frame in descending order by the commit count.
get.committer.or.author.commit.count = function(range.data) {
    logging::logdebug("get.committer.or.author.commit.count: starting.")

    ## Get commit data
    commits.df = range.data$get.commits()

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

#' Get the number of changed lines of code (LOC) for each author based on the commit data contained in the specified
#' \code{ProjectData}. The number is calculated by taking the sum of added and deleted lines of code for each commit.
#'
#' @param proj.data the \code{ProjectData} containing the commit data
#'
#' @return a dataframe consisting of two columns, the first of which holding the authors' names and the second holding
#'         their respective LOC counts
get.author.loc.count = function(proj.data) {
  logging::logdebug("get.author.loc.count: starting.")

  ## Get commit data
  commits.df = proj.data$get.commits()

  ## For each commit hash, make sure there is only one row
  commits.df = commits.df[!duplicated(commits.df[["hash"]]), ]

  ## Restrict commits to relevant columns
  commits.df = commits.df[c("author.name", "added.lines", "deleted.lines")]

  ## Execute a query to get the changed lines per author
  res = sqldf::sqldf("SELECT `author.name`, SUM(`added.lines`) + SUM(`deleted.lines`) AS `loc`
                        FROM `commits.df`
                        GROUP BY `author.name` ORDER BY `loc` DESC, `author.name` ASC")

  logging::logdebug("get.author.loc.count: finished.")
  return(res)
}


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Mail-based statistics ---------------------------------------------------

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

#' Get the issue/pr count for each author based on the issue data contained in the specified \code{ProjectData}.
#' The issue count here is the number of issues the author participated in (which can mean anything,
#' from commenting to closing to assigning the issue to others, to labeling, referencing it in other issues,
#' adding commits, ...).
#'
#' The type argument specifies whether we count PRs alone, issues alone, or both (\code{"all"}).
#'
#' @param proj.data the \code{ProjectData} containing the issue data
#' @param type which issue type to consider (see \code{preprocess.issue.data}).
#'             One of \code{"issues"}, \code{"pull.requests"} or \code{"all"}
#'             [default: "all"]
#' @param use.unfiltered.data whether to use unfiltered issue data (see \code{preprocess.issue.data}) [default: FALSE]
#'
#' @return a dataframe consisting of two columns, the first of which holding the authors' names and the second holding
#'         their respective issue counts
get.author.issue.count = function(proj.data, type = c("all", "issues", "pull.requests"), use.unfiltered.data = FALSE) {
    type = match.arg(type)
    logging::logdebug("get.author.issue.count: starting.")
    df = preprocess.issue.data(proj.data, type = type, use.unfiltered.data = use.unfiltered.data)
    ## count distinct since an author may appear in the same issue multiple times
    stmt = "SELECT `author.name`, COUNT( DISTINCT `issue.id`) as `freq` FROM `df`
                             GROUP BY `author.name` ORDER BY `freq` DESC, `author.name` ASC"
    res = sqldf::sqldf(stmt)
    logging::logdebug("get.author.issue.count: finished")
    return(res)
}

#' Get the issue/pr count for each author based on the issue data contained in the specified \code{ProjectData}.
#' The issue count here is the number of issues the author created.
#'
#' The type argument specifies whether we count PRs alone, issues alone, or both (\code{"all"}).
#'
#' @param proj.data the \code{ProjectData} containing the issue data
#' @param type which issue type to consider (see \code{preprocess.issue.data}).
#'             One of \code{"issues"}, \code{"pull.requests"} or \code{"all"}
#'             [default: "all"]
#' @param use.unfiltered.data whether to use unfiltered issue data (see \code{preprocess.issue.data}). Note that the
#'                            filtered data may not contain issue created events.
#'                            [default: TRUE]
#'
#' @return a dataframe consisting of two columns, the first of which holding the authors' names and the second holding
#'         their respective issue counts
get.author.issues.created.count = function(proj.data, type = c("all", "issues", "pull.requests"),
                                           use.unfiltered.data = TRUE) {
    type = match.arg(type)
    logging::logdebug("get.author.issues.created.count: starting.")
    df = preprocess.issue.data(proj.data, type = type, use.unfiltered.data = use.unfiltered.data)
    ## count distinct since an author may appear in the same issue multiple times
    stmt = "SELECT `author.name`, COUNT( DISTINCT `issue.id`) as `freq` FROM `df`
                             WHERE `event.name` = 'created'
                             GROUP BY `author.name` ORDER BY `freq` DESC, `author.name` ASC"
    res = sqldf::sqldf(stmt)
    logging::logdebug("get.author.issues.created.count: finished")
    return(res)
}

#' Get the issue/pr count for each author based on the issue data contained in the specified \code{ProjectData}.
#' The issue count here is the number of issues the author commented in.
#'
#' The type argument specifies whether we count PRs alone, issues alone, or both (\code{"all"}).
#'
#' @param proj.data the \code{ProjectData} containing the issue data
#' @param type which issue type to consider (see \code{preprocess.issue.data}).
#'             One of \code{"issues"}, \code{"pull.requests"} or \code{"all"}
#'             [default: "all"]
#' @param use.unfiltered.data whether to use unfiltered issue data (see \code{preprocess.issue.data}) [default: FALSE]
#'
#' @return a dataframe consisting of two columns, the first of which holding the authors' names and the second holding
#'         their respective issue counts
get.author.issues.commented.in.count = function(proj.data, type = c("all", "issues", "pull.requests"),
                                                use.unfiltered.data = FALSE) {
    type = match.arg(type)
    logging::logdebug("get.author.issues.commented.in.count: starting.")
    df = preprocess.issue.data(proj.data, type = type, use.unfiltered.data = use.unfiltered.data)
    ## count distinct since an author may appear in the same issue multiple times
    stmt = "SELECT `author.name`, COUNT( DISTINCT `issue.id`) as `freq` FROM `df`
                             WHERE `event.name` = 'commented'
                             GROUP BY `author.name` ORDER BY `freq` DESC, `author.name` ASC"
    res = sqldf::sqldf(stmt)
    logging::logdebug("get.author.issues.commented.in.count: finished")
    return(res)
}

#' Get the issue/pr comment count for each author based on the issue data contained in the specified \code{ProjectData}.
#' The issue comment count here is the number of comments the author created summed across all issues
#'
#' The type argument specifies whether we count PRs alone, issues alone, or both (\code{"all"}).
#'
#' @param proj.data the \code{ProjectData} containing the issue data
#' @param type which issue type to consider (see \code{preprocess.issue.data}).
#'             One of \code{"issues"}, \code{"pull.requests"} or \code{"all"}
#'             [default: "all"]
#' @param use.unfiltered.data whether to use unfiltered issue data (see \code{preprocess.issue.data}) [default: FALSE]
#'
#' @return a dataframe consisting of two columns, the first of which holding the authors' names and the second holding
#'         their respective comment counts
get.author.issue.comment.count = function(proj.data, type = c("all", "issues", "pull.requests"),
                                          use.unfiltered.data = FALSE) {
    type = match.arg(type)
    logging::logdebug("get.author.issue.comment.count: starting.")
    df = preprocess.issue.data(proj.data, type = type, use.unfiltered.data = use.unfiltered.data)
    stmt = "SELECT `author.name`, COUNT(*) as `freq` FROM `df`
                             WHERE `event.name` = 'commented'
                             GROUP BY `author.name` ORDER BY `freq` DESC, `author.name` ASC"
    res = sqldf::sqldf(stmt)
    logging::logdebug("get.author.issue.comment.count: finished")
    return(res)
}

## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Mail Thread Statistics --------------------------------------------------

#' Get the number of contributors to each mail thread based on the mail data contained in the
#' specified \code{ProjectData}.
#'
#' @param proj.data the \code{ProjectData} containing the mail data
#'
#' @return a named list of contributor counts, where the name is the thread.
get.mail.thread.contributor.count = function(proj.data) {
    logging::logdebug("get.mail.thread.contributor.count: starting.")
    thread.to.mails = get.key.to.value.from.df(proj.data$get.mails(), "thread", "author.email")
    thread.to.contributor.count = lapply(thread.to.mails, function(df) {
        length(unique(df[["data.vertices"]]))
    })
    logging::logdebug("get.mail.thread.contributor.count: finished")
    return(thread.to.contributor.count)
}

#' Get the number of messages in each mail thread based on the mail data contained in the
#' specified \code{ProjectData}.
#'
#' @param proj.data the \code{ProjectData} containing the mail data
#'
#' @return a named list of message counts, where the name is the thread.
get.mail.thread.mail.count = function(proj.data) {
    logging::logdebug("get.mail.thread.mail.count: starting.")
    thread.to.mails = get.key.to.value.from.df(proj.data$get.mails(), "thread", "author.email")
    thread.to.mail.count = lapply(thread.to.mails, function(df) {
        length(df[["data.vertices"]])
    })
    logging::logdebug("get.mail.thread.mail.count: finished")
    return(thread.to.mail.count)
}

#' Get the date of the first message in each mail thread based on the mail data contained
#' in the specified \code{ProjectData}.
#'
#' @param proj.data the \code{ProjectData} containing the mail data
#'
#' @return a named list of start dates, where the name is the thread.
get.mail.thread.start.date = function(proj.data) {
    logging::logdebug("get.mail.thread.start.date: starting.")
    thread.to.dates = get.key.to.value.from.df(proj.data$get.mails(), "thread", "date")
    thread.to.start.date = lapply(thread.to.dates, function(df) {
        min(df[["data.vertices"]])
    })
    logging::logdebug("get.mail.thread.start.date: finished")
    return(thread.to.start.date)
}

#' Get the date of the last message in each mail thread based on the mail data contained
#' in the specified \code{ProjectData}
#'
#' @param proj.data the \code{ProjectData} containing the mail data
#'
#' @return a named list of end dates, where the name is the thread.
get.mail.thread.end.date = function(proj.data) {
    logging::logdebug("get.mail.thread.end.date: starting.")
    thread.to.dates = get.key.to.value.from.df(proj.data$get.mails(), "thread", "date")
    thread.to.end.date = lapply(thread.to.dates, function(df) {
        max(df[["data.vertices"]])
    })
    logging::logdebug("get.mail.thread.end.date: finished")
    return(thread.to.end.date)
}

#' Get the identifier of the mailing list from which a threat originates.
#' This identifier is part of the thread ID as produced by codeface, e.g., if the thread ID is "13#37", then 13 is the
#' ID of the mailing list.
#'
#' Older versions of codeface did not include this identifier. If the identifier is not included in the data used, a
#' warning is produced and the list will contain \code{NA} for each thread.
#'
#' @param proj.data the \code{ProjectData} containing the mail data
#'
#' @return a named list of mailing list identifiers, where the name is the thread.
get.mail.thread.originating.mailing.list = function(proj.data) {
    logging::logdebug("get.mail.thread.originating.mailing.list: starting.")
    thread.ids = unique(proj.data$get.mails()[["thread"]])
    thread.to.list = lapply(thread.ids, function(thread.name) {
        thread.id = substr(thread.name, 9, nchar(thread.name) - 1) # remove '<thread-' '>'
        if (grepl("#", thread.id, fixed = TRUE)) { # make sure that our data has the shape we expect
            mailing.list = strsplit(thread.id, "#")[[1]][1] # split at '#' and keep only first part
            return(mailing.list)
        }
        else {
            logging::logwarn("get.mail.thread.originating.mailing.list called on incompatible data")
            return(NA)
        }
    })
    names(thread.to.list) = thread.ids
    logging::logdebug("get.mail.thread.originating.mailing.list: finished")
    return(thread.to.list)
}

## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Issue Statistics --------------------------------------------------------

#' Get the number of contributors to each issue based on the issue data contained
#' in the specified \code{ProjectData}.
#'
#' The type argument specifies whether we count PRs alone, issues alone, or both (\code{"all"}).
#'
#' @param proj.data the \code{ProjectData} containing the issue data
#' @param type which issue type to consider (see \code{preprocess.issue.data}).
#'             One of \code{"issues"}, \code{"pull.requests"} or \code{"all"}
#'             [default: "all"]
#' @param use.unfiltered.data whether to use unfiltered issue data (see \code{preprocess.issue.data}) [default: FALSE]
#'
#' @return a named list of contributor counts, where the name is the issue ID.
get.issue.contributor.count = function(proj.data, type = c("all", "issues", "pull.requests"),
                                       use.unfiltered.data = FALSE) {
    type = match.arg(type)
    logging::logdebug("get.issue.contributor.count: starting.")
    df = preprocess.issue.data(proj.data, type = type, retained.cols = c("issue.id", "author.email"),
                               use.unfiltered.data = use.unfiltered.data)
    issue.id.to.events = get.key.to.value.from.df(df, "issue.id", "author.email")
    issue.id.to.contributor.count = lapply(issue.id.to.events, function(df) {
        length(unique(df[["data.vertices"]]))
    })
    logging::logdebug("get.issue.contributor.count: finished")
    return(issue.id.to.contributor.count)
}

#' Get the number of events for each issue based on the issue data contained
#' in the specified \code{ProjectData}.
#'
#' The type argument specifies whether we count PRs alone, issues alone, or both (\code{"all"}).
#'
#' @param proj.data the \code{ProjectData} containing the issue data
#' @param type which issue type to consider (see \code{preprocess.issue.data}).
#'             One of \code{"issues"}, \code{"pull.requests"} or \code{"all"}
#'             [default: "all"]
#' @param use.unfiltered.data whether to use unfiltered issue data (see \code{preprocess.issue.data}) [default: FALSE]
#'
#' @return a named list of event counts, where the name is the issue ID.
get.issue.event.count = function(proj.data, type = c("all", "issues", "pull.requests"), use.unfiltered.data = FALSE) {
    type = match.arg(type)
    logging::logdebug("get.issue.event.count: starting.")
    df = preprocess.issue.data(proj.data, type = type, retained.cols = c("issue.id", "event.id"),
                               use.unfiltered.data = use.unfiltered.data)
    issue.id.to.events = get.key.to.value.from.df(df, "issue.id", "event.id")
    issue.id.to.event.count = lapply(issue.id.to.events, function(df) {
        ## one event might show up multiple times (i.e. 'mentioned' also triggers 'subscribed'),
        ## so we count the number of distinct event IDs
        length(unique(df[["data.vertices"]]))
    })
    logging::logdebug("get.issue.event.count: finished")
    return(issue.id.to.event.count)
}

#' Get the number of 'commented' events for each issue based on the issue data contained
#' in the specified \code{ProjectData}.
#'
#' The type argument specifies whether we count PRs alone, issues alone, or both (\code{"all"}).
#'
#' @param proj.data the \code{ProjectData} containing the issue data
#' @param type which issue type to consider (see \code{preprocess.issue.data}).
#'             One of \code{"issues"}, \code{"pull.requests"} or \code{"all"}
#'             [default: "all"]
#'
#' @return a named list of comment counts, where the name is the issue ID.
get.issue.comment.count = function(proj.data, type = c("all", "issues", "pull.requests")) {
    type = match.arg(type)
    logging::logdebug("get.issue.comment.count: starting.")
    df = preprocess.issue.data(proj.data, type = type, retained.cols = c("issue.id", "event.name"))
    issue.id.to.events = get.key.to.value.from.df(df, "issue.id", "event.name")
    issue.id.to.comment.count = lapply(issue.id.to.events, function(df) {
        event.names = df[["data.vertices"]]
        return(length(event.names[event.names == "commented"]))
    })
    logging::logdebug("get.issue.comment.count: finished")
    return(issue.id.to.comment.count)
}

#' Get the date each issue was opened, based on the issue data contained
#' in the specified \code{ProjectData}.
#'
#' The type argument specifies whether we count PRs alone, issues alone, or both (\code{"all"}).
#'
#' @param proj.data the \code{ProjectData} containing the issue data
#' @param type which issue type to consider (see \code{preprocess.issue.data}).
#'             One of \code{"issues"}, \code{"pull.requests"} or \code{"all"}
#'             [default: "all"]
#'
#' @return a named list of dates, where the name is the issue ID.
get.issue.opened.date = function(proj.data, type = c("all", "issues", "pull.requests")) {
    type = match.arg(type)
    logging::logdebug("get.issue.opened.date: starting.")
    df = preprocess.issue.data(proj.data, type = type, retained.cols = c("issue.id", "creation.date"))
    issue.id.to.dates = get.key.to.value.from.df(df, "issue.id", "creation.date")
    issue.id.to.start.date = lapply(issue.id.to.dates, function(df) {
        min(df[["data.vertices"]]) # values should all be the same
    })
    logging::logdebug("get.issue.opened.date: finished")
    return(issue.id.to.start.date)
}

#' Get the date each issue was closed, based on the issue data contained
#' in the specified \code{ProjectData}, or \code{NA} if the issue is still open.
#'
#' The type argument specifies whether we count PRs alone, issues alone, or both (\code{"all"}).
#'
#' @param proj.data the \code{ProjectData} containing the issue data
#' @param type which issue type to consider (see \code{preprocess.issue.data}).
#'             One of \code{"issues"}, \code{"pull.requests"} or \code{"all"}
#'             [default: "all"]
#'
#' @return a named list of dates, where the name is the issue ID.
get.issue.closed.date = function(proj.data, type = c("all", "issues", "pull.requests")) {
    type = match.arg(type)
    logging::logdebug("get.issue.closed.date: starting.")
    df = preprocess.issue.data(proj.data, type = type, retained.cols = c("issue.id", "closing.date"))
    issue.id.to.dates = get.key.to.value.from.df(df, "issue.id", "closing.date")
    issue.id.to.closed.date = lapply(issue.id.to.dates, function(df) {
        min(df[["data.vertices"]]) # values should all be the same
    })
    logging::logdebug("get.issue.closed.date: finished")
    return(issue.id.to.closed.date)
}

#' Get the date of the last activity in each issue based on the issue data contained
#' in the specified \code{ProjectData}.
#'
#' The type argument specifies whether we count PRs alone, issues alone, or both (\code{"all"}).
#'
#' @param proj.data the \code{ProjectData} containing the issue data
#' @param type which issue type to consider (see \code{preprocess.issue.data}).
#'             One of \code{"issues"}, \code{"pull.requests"} or \code{"all"}
#'             [default: "all"]
#' @param use.unfiltered.data whether to use unfiltered issue data (see \code{preprocess.issue.data}) [default: FALSE]
#'
#' @return a named list of dates, where the name is the issue ID.
get.issue.last.activity.date = function(proj.data, type = c("all", "issues", "pull.requests"),
                                        use.unfiltered.data = FALSE) {
    type = match.arg(type)
    logging::logdebug("get.issue.last.activity.date: starting.")
    df = preprocess.issue.data(proj.data, type = type, retained.cols = c("issue.id", "date"),
                               use.unfiltered.data = use.unfiltered.data)
    issue.id.to.dates = get.key.to.value.from.df(df, "issue.id", "date")
    issue.id.to.end.date = lapply(issue.id.to.dates, function(df) {
        max(df[["data.vertices"]])
    })
    logging::logdebug("get.issue.last.activity.date: finished")
    return(issue.id.to.end.date)
}

#' Get the title of each issue based on the issue data contained
#' in the specified \code{ProjectData}.
#'
#' The type argument specifies whether we count PRs alone, issues alone, or both (\code{"all"}).
#'
#' @param proj.data the \code{ProjectData} containing the issue data
#' @param type which issue type to consider (see \code{preprocess.issue.data}).
#'             One of \code{"issues"}, \code{"pull.requests"} or \code{"all"}
#'             [default: "all"]
#'
#' @return a named list of dates, where the name is the issue ID.
get.issue.title = function(proj.data, type = c("all", "issues", "pull.requests")) {
    type = match.arg(type)
    logging::logdebug("get.issue.title: starting.")
    df = preprocess.issue.data(proj.data, type = type, retained.cols = c("issue.id", "issue.title"))
    issue.id.to.title = get.key.to.value.from.df(df, "issue.id", "issue.title")
    issue.id.to.title.only = lapply(issue.id.to.title, function(df) {
        ## as a result of get.key.to.value.from.df, the "issue.title" column should be duplicated as "data.vertices".
        ## The title should be the same in every row, so we can just use the first row.
        df[[1,"data.vertices"]] # data frames resulting from get.key.to.value.from.df always have at least one row
    })
    logging::logdebug("get.issue.title: finished")
    return(issue.id.to.title.only)
}

#' Get whether a PR is open, has been merged, or has been closed without merging.
#'
#' @param proj.data the \code{ProjectData} containing the issue data
#' @param use.unfiltered.data whether to use unfiltered issue data (see \code{preprocess.issue.data}) [default: TRUE]
#'
#' @return a named list of dates, where the name is the issue ID.
get.pr.open.merged.or.closed = function(proj.data, use.unfiltered.data = TRUE) {
    logging::logdebug("get.pr.open.merged.or.closed: starting.")
    df = preprocess.issue.data(proj.data, type = "pull.requests", use.unfiltered.data = use.unfiltered.data,
                               retained.cols = c("issue.id", "issue.state", "event.name"))
    issue.id.to.events = get.key.to.value.from.df(df, "issue.id", "event.name")
    issue.id.to.state = lapply(issue.id.to.events, function(df) {
        return(if ("open" %in% df[["issue.state"]] || "reopened" %in% df[["issue.state"]]) "open"
               else if ("merged" %in% df[["event.name"]]) "merged"
               else "closed")
    })
    logging::logdebug("get.pr.open.merged.or.closed: finished")
    return(issue.id.to.state)
}

#' Get whether each issue is a pull request, based on the issue data contained in the specified
#' \code{ProjectData}.
#'
#' @param proj.data the \code{ProjectData} containing the issue data
#'
#' @return a named list of logical values, where the name is the issue ID.
get.issue.is.pull.request = function(proj.data) {
    logging::logdebug("get.issue.is.pull.request: starting.")
    issue.data = proj.data$get.issues()
    issue.id.to.is.pr = as.list(mask.pull.requests(issue.data))
    names(issue.id.to.is.pr) = issue.data[["issue.id"]]
    logging::logdebug("get.issue.is.pull.request: finished")
    return(issue.id.to.is.pr)
}

## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Commit-Message Functionality ------------------------------------------

#' Apply preprocessing steps to commit messages of given commits. Preprocessing steps are always performed in
#' the following order: \code{"lowercase"} -> \code{"stopwords"} -> \code{"punctuation"} -> \code{"whitespaces"}
#'
#' \code{"lowercase"} transforms all upper case characters into their lowercase counterparts
#' \code{"stopwords"} removes all stopwords using a list of stopwords
#'                    for the english language provided by the package \code{tm}
#' \code{"punctuation"} removes all punctuation, as described in the ASCII \code{[:punct:]} class,
#'                     using the r-base \code{regex} functionality. This includes standard punctuation
#'                     characters such as ",", ".", ":", etc. but also dashes, parentheses, mathematical
#'                     symbols and special characters used in programming, such as "$", "#", or "&".
#'                     Intra-word dashes are kept.
#' \code{"whitespaces"} removes superflous whitespace characters, such as "\t" or "\n", and replaces
#'                      them with single whitespaces
#'
#' @param proj.data the \code{ProjectData} containing the commit-message data
#' @param commit.hashes the commit hashes that should be considered, if \code{NULL} all commits are considered
#'                      [default: NULL]
#' @param preprocessing the preprocessing steps to be executed
#'                      [default: c("lowercase", "punctuation", "stopwords", "whitespaces")]
#'
#' @return a dataframe containing the hashes and corresponding preprocesessed messages
get.preprocessed.commit.messages = function(proj.data,
                                            commit.hashes = NULL,
                                            preprocessing = c("lowercase",
                                                              "punctuation",
                                                              "stopwords",
                                                              "whitespaces")) {
    preprocessing = match.arg.or.default(preprocessing, several.ok = TRUE)
    preprocessed.messages = create.empty.data.frame(c("hash", "preprocessed.message"))
    ## get commit-message data of the given hashes
    ## if no hashes are given consider all commits
    commit.message.data = proj.data$get.commit.messages()
    if (!is.null(commit.hashes)) {
        commit.message.data = commit.message.data[commit.message.data[["hash"]] %in% commit.hashes, ]
    }

    ## if data is empty, abort process
    if (nrow(commit.message.data) < 1) {
        return(preprocessed.messages)
    }

    ## create a corpus with all selected commit messages
    if (proj.data$get.project.conf.entry("commit.messages") == "message") {
        messages = paste(commit.message.data[["title"]], commit.message.data[["message"]])
    } else {
        messages = commit.message.data[["title"]]
    }
    corpus = tm::Corpus(tm::VectorSource(messages))

    ## preprocessing steps
    if ("lowercase" %in% preprocessing) {
        ## convert to lowercase
        corpus = tm::tm_map(corpus, tm::content_transformer(tolower))
    }
    if ("stopwords" %in% preprocessing) {
        ## remove stopwords
        corpus = tm::tm_map(corpus, tm::removeWords, tm::stopwords("english"))
    }
    if ("punctuation" %in% preprocessing) {
        ## remove punctuation
        corpus = tm::tm_map(corpus, tm::removePunctuation, preserve_intra_word_dashes = TRUE)
    }
    if ("whitespaces" %in% preprocessing) {
        ## remove excess whitespaces
        corpus = tm::tm_map(corpus, tm::stripWhitespace)
    }

    ## trim leading and trailing spaces
    corpus = tm::tm_map(corpus, trimws)

    ## create output dataframe
    preprocessed.messages = data.frame(hash = commit.message.data[["hash"]],
                                       preprocessed.message = corpus$content)

    return(preprocessed.messages)
}

#' Apply stemming to commit messages of given commits. Preprocessing will be executed as part of this.
#' Preprocessing steps are always performed in the following order:
#' \code{"lowercase"} -> \code{"stopwords"} -> \code{"punctuation"} -> \code{"whitespaces"}
#'
#' @param proj.data the \code{ProjectData} containing the commit-message data
#' @param commit.hashes the commit hashes that should be considered, if \code{NULL} all commits are considered
#'                      [default: NULL]
#' @param preprocessing the preprocessing steps to be executed
#'                      [default: c("lowercase", "punctuation", "stopwords", "whitespaces")]
#'
#' @return a dataframe containing the hashes and corresponding stemmed messages
get.stemmed.commit.messages = function(proj.data,
                                       commit.hashes = NULL,
                                       preprocessing = c("lowercase", "punctuation", "stopwords", "whitespaces")) {
    ## apply preprocessing
    preprocessed.messages = get.preprocessed.commit.messages(proj.data, commit.hashes, preprocessing)
    ## build corpus
    corpus = tm::Corpus(tm::VectorSource(preprocessed.messages[, "preprocessed.message"]))
    ## apply stemming
    corpus = tm::tm_map(corpus, tm::stemDocument)
    ## create output dataframe
    stemmed.messages = data.frame(hash = preprocessed.messages[["hash"]],
                                  stemmed.message = corpus$content)
    return(stemmed.messages)
}

#' Apply tokenization to commit messages of given commits. This function does not allow for preprocessing,
#' since it is supposed to extract all tokens from the text as is and preprocessing steps change the
#' resulting tokens. The text is split into tokens at any whitespace character.
#' Special characters have no impact and are treated the same as any other non-whitespace character.
#'
#' @param proj.data the \code{ProjectData} containing the commit-message data
#' @param commit.hashes the commit hashes that should be considered, if \code{NULL} all commits are considered
#'                      [default: NULL]
#'
#' @return a list of vectors containing the tokens from the commit messages
get.tokenized.commit.messages = function(proj.data, commit.hashes = NULL) {
    ## get commit-message data of the given hashes
    ## if no hashes are given consider all commits
    commit.message.data = proj.data$get.commit.messages()
    if (!is.null(commit.hashes)) {
        commit.message.data = commit.message.data[commit.message.data[["hash"]] %in% commit.hashes, ]
    }
    if (proj.data$get.project.conf.entry("commit.messages") == "message") {
        messages = paste(commit.message.data[["title"]], commit.message.data[["message"]])
    } else {
        messages = commit.message.data[["title"]]
    }
    tokens = lapply(messages, tm::Boost_tokenizer)

    return(tokens)
}

#' Apply lemmatization to commit messages of given commits. Preprocessing will be executed as part of this.
#' Preprocessing steps are always performed in the following order:
#' \code{"lowercase"} -> \code{"stopwords"} -> \code{"punctuation"} -> \code{"whitespaces"}
#'
#' @param proj.data the \code{ProjectData} containing the commit-message data
#' @param commit.hashes the commit hashes that should be considered, if \code{NULL} all commits are considered
#'                      [default: NULL]
#' @param preprocessing the preprocessing steps to be executed
#'                      [default: c("lowercase", "punctuation", "stopwords", "whitespaces")]
#'
#' @return a dataframe containing the hashes and corresponding lemmatized messages
get.lemmatized.commit.messages = function(proj.data,
                                          commit.hashes = NULL,
                                          preprocessing = c("lowercase", "punctuation", "stopwords", "whitespaces")) {
    ## apply preprocessing
    preprocessed.messages = get.preprocessed.commit.messages(proj.data, commit.hashes, preprocessing)
    ## build corpus
    corpus = tm::Corpus(tm::VectorSource(preprocessed.messages[, "preprocessed.message"]))
    ## apply lemmatization
    corpus = tm::tm_map(corpus, textstem::lemmatize_strings)
    ## create output dataframe
    lemmatized.messages = data.frame(hash = preprocessed.messages[["hash"]],
                                     lemmatized.message = corpus$content)
    return(lemmatized.messages)
}

#' Get commit messages that match given strings.
#'
#' @param proj.data the \code{ProjectData} containing the commit-message data
#' @param commit.hashes the commit hashes that should be considered, if \code{NULL} all commits are considered
#'                      [default: NULL]
#' @param strings the strings that are searched for
#' @param match the function that describes how many of the strings need to be in a message in order for
#'              that message to be returned. Can be any function that takes a list of logical values and
#'              returns a single logical value, such as \code{any}, \code{all} or anything in between [default: any]
#' @param ignore.case whether the case should be ignored in the search [default: TRUE]
#'
#' @return a dataframe containing the hashes and corresponding matching messages
get.commit.messages.by.strings = function(proj.data, commit.hashes = NULL, strings, match = any, ignore.case = TRUE) {
    messages = create.empty.data.frame(c("hash", "message"))
    ## get commit-message data of the given hashes
    ## if no hashes are given consider all commits
    commit.message.data = proj.data$get.commit.messages()
    if (!is.null(commit.hashes)) {
        commit.message.data = commit.message.data[commit.message.data[["hash"]] %in% commit.hashes, ]
    }

    ## prepare the dataframe for searching commit messages by merging the 'title' and 'message' columns if desired
    if (proj.data$get.project.conf.entry("commit.messages") == "message") {
        commit.message.data[["message"]] = paste(commit.message.data[["title"]], commit.message.data[["message"]])
    } else {
        commit.message.data[["message"]] = commit.message.data[["title"]]
    }
    commit.message.data = commit.message.data[, c("hash", "message")]
    ## filter the messages by searching for all the keywords in 'strings'
    ## and applying the match function once per message
    messages = commit.message.data[unlist(parallel::mclapply(commit.message.data[["message"]], function(msg) {
                                       match(lapply(strings, function(word) {
                                           return (grepl(word, msg, ignore.case = ignore.case))
                                       }))
                                   })), ]
    return(messages)
}


#' Count tokens in given commit messages.
#'
#' @param proj.data the \code{ProjectData} containing the commit-message data
#' @param commit.hashes the commit hashes that should be considered, if \code{NULL} all commits are considered
#'                      [default: NULL]
#'
#' @return a dataframe containing the hashes and corresponding token counts
get.commit.message.counts = function(proj.data, commit.hashes = NULL) {
    ## get commit-message data of the given hashes
    ## if no hashes are given consider all commits
    commit.message.data = proj.data$get.commit.messages()
    if (!is.null(commit.hashes)) {
        commit.message.data = commit.message.data[commit.message.data[["hash"]] %in% commit.hashes, ]
    }
    ## get tokens
    tokens = get.tokenized.commit.messages(proj.data, commit.hashes)

    hashes = commit.message.data[["hash"]]
    counts = unlist(lapply(tokens, length))
    messages = data.frame(hash = hashes,
                          count = counts)
    return(messages)
}


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Commit-Message Tags -----------------------------------------------------

#' Extract trailer tags (Signed-off-by, Reviewed-by, Acked-by, ...) from
#' commit message bodies.
#'
#' Scan each commit's message body line by line for trailer-style lines
#' ("Tag-Name: ...") and extract them into a data.frame -- one row per
#' (commit, tag) pair. Commits with multiple tags produce multiple
#' rows; commits with no matching tags are simply absent from the result.
#'
#' Two line shapes are recognized:
#'   1. "Tag-Name: Some Person <email@example.com>"  -> name + e-mail
#'   2. "Tag-Name: <rest>" with no <...>              -> rest is classified
#'      as either an e-mail (if it looks like a bare e-mail address, for
#'      example, "Cc: name@example.domain.org") or a name (everything else,
#'      for example, "Cc: Some Person"), with the other field left \code{NA}.
#'
#' @param commits data.frame with one row per commit
#' @param hash.col name of the column holding the commit hash
#'                 [default: "hash"]
#' @param body.col name of the column holding the commit-message body
#'                 [default: "body"]
#' @param allow.no.email if \code{TRUE}, also capture trailer-shaped
#'                        lines that have no <e-mail> (see shape 2 above).
#'                        If \code{FALSE}, only lines with an explicit <e-mail>
#'                        are captured [default: TRUE]
#'
#' @return data.frame with columns "commit.hash", "tag", "name", "email"
extract.commit.message.tags = function(commits, hash.col = "hash", body.col = "body",
                                       allow.no.email = TRUE) {

  # Line has an explicit <e-mail>: "Tag-Name: Some Person <email@example.com>"
  # group 1 = tag name, group 2 = person name, group 3 = e-mail
  with.email.pattern = "^\\s*([A-Za-z][A-Za-z0-9-]*)\\s*:\\s*(.*?)\\s*<([^<>]+)>\\s*$"

  # Fallback: any other trailer-shaped line "Tag-Name: <rest>"
  # group 1 = tag name, group 2 = rest of the line
  no.email.pattern = "^\\s*([A-Za-z][A-Za-z0-9-]*)\\s*:\\s*(.+?)\\s*$"

  # A bare address with no surrounding <>, for example, "name@example.domain.org"
  bare.email.pattern = "^[^\\s<>]+@[^\\s<>]+$"

  empty.result = data.frame(commit.hash = character(0), tag = character(0),
                            name = character(0), email = character(0))

  # For each commit, extract all matching lines and return a data.frame of (commit, tag, name, e-mail) rows
  result.list = parallel::mclapply(seq_len(nrow(commits)), function(i) {

    # Get the commit hash and message body for this row
    hash = commits[[hash.col]][i]
    body = commits[[body.col]][i]

    # Return empty if the body is 'NA' or empty
    if (is.na(body) || !nzchar(body)) {
      return(empty.result)
    }

    # Split the body into lines and match each line against the two patterns
    lines = strsplit(body, "\r?\n")[[1]]

    # --- lines with an explicit <e-mail> ---
    with.email = str.match.vectorized(lines, with.email.pattern, n.groups = 4) # groups: full match, tag, name, email
    has.email = !is.na(with.email[, 1])

    rows.with.email = if (any(has.email)) {
      data.frame(
        commit.hash = hash,
        tag         = with.email[has.email, 2],
        name        = with.email[has.email, 3],
        email       = with.email[has.email, 4]
      )
    } else {
      empty.result
    }

    # --- remaining lines: trailer-shaped but no <e-mail> ---
    rows.no.email = if (allow.no.email) {
      remaining = lines[!has.email]
      no.email = str.match.vectorized(remaining, no.email.pattern, n.groups = 3) # groups: full match, tag, rest
      valid.rows = !is.na(no.email[, 1])

      if (any(valid.rows)) {
        tag.names = no.email[valid.rows, 2]
        rest      = no.email[valid.rows, 3]
        is.bare   = grepl(bare.email.pattern, rest)

        data.frame(
          commit.hash = hash,
          tag         = tag.names,
          name        = ifelse(is.bare, NA_character_, rest),
          email       = ifelse(is.bare, rest, NA_character_)
        )
      } else {
        empty.result
      }
    } else {
      empty.result
    }

    # Combine the two sets of rows and return
    rbind(rows.with.email, rows.no.email)
  })

  # Combine all commits' results into a single data.frame
  return(data.table::rbindlist(result.list))
}


#' Get summary statistics about extracted commit-message tags.
#'
#' Report how often each tag occurs, how many have an e-mail vs. not, and
#' how many distinct commits/people are involved per tag.
#'
#' @param commit.message.tags data.frame as returned by \code{extract.commit.message.tags()}
#'
#' @return a data.frame with the per-tag counts
#'
#' @seealso extract.commit.message.tags
get.commit.message.tag.statistics = function(commit.message.tags) {

  # Convert to data.table for efficient processing
  commit.message.tags.dt = data.table::as.data.table(commit.message.tags)

  # Compute counts per tag: total occurrences, distinct commits, with/without e-mail, distinct people
  tag.counts = commit.message.tags.dt[, .(
    occurrences      = .N,
    distinct.commits = data.table::uniqueN(commit.hash),
    with.email       = sum(!is.na(email)),
    without.email    = sum(is.na(email)),
    distinct.people  = data.table::uniqueN(name)
  ), by = tag]

  # Sort by descending occurrence count
  data.table::setorder(tag.counts, -occurrences)

  logging::loginfo("Total tag occurrences: %d (across %d distinct commits)",
                   nrow(commit.message.tags), data.table::uniqueN(commit.message.tags[["commit.hash"]]))

  return(tag.counts)
}


#' Canonicalize spelling variants/typos of trailer tags of commit messages.
#'
#' Real-world commit trailers are full of typos and one-off variants of
#' the same tag (e.g., "Sigend-off-by", "Singed-off-by", "Reviewd-by",
#' "Ackec-by" all mean the same thing as "Signed-off-by", "Reviewed-by",
#' "Acked-by"). This groups them together automatically using greedy
#' frequency-ordered clustering:
#'
#'   1. Tags are first merged case-insensitively (as in
#'      \code{get.commit.message.tag.statistics()}).
#'   2. Unique tags are sorted by occurrence count, most common first.
#'   3. Processing in that order, each tag is compared (after stripping
#'      non-letters and lowercasing) to every canonical "cluster
#'      representative" chosen so far, using normalized string similarity
#'      \code{(1 - edit_distance / max(length))}. If the best match is
#'      \code{>= similarity.threshold}, the tag is folded into that
#'      representative.
#'      Otherwise the tag becomes a new cluster representative itself.
#'
#' Processing by descending frequency (rather than picking a fixed set of
#' "seed" tags above some count) matters: a common typo (e.g.,
#' "Signed-of-by" appearing 125 times) must never become its own
#' unmergeable anchor just because it's frequent -- it should still fold
#' into the far more common correct spelling ("Signed-off-by") if it is
#' similar enough, since the true canonical spelling is processed first
#' and made available as a match target.
#'
#' This is intentionally conservative: compound tags like
#' "Reported-and-tested-by" are similar enough to "Reported-by" in
#' meaning but NOT in edited string form, so they score low similarity
#' and are correctly left alone rather than merged.
#'
#' @param commit.message.tags data.frame as returned by \code{extract.commit.message.tags()}
#' @param similarity.threshold similarity cutoff for folding a tag into its
#'                             nearest existing cluster representative
#'                             [default: 0.7]
#'
#' @return \code{commit.message.tags} with an added \code{tag.clean} column holding the
#'         canonical spelling for each row
#'
#' @seealso extract.commit.message.tags
#' @seealso get.commit.message.tag.statistics
canonicalize.commit.message.tags = function(commit.message.tags, similarity.threshold = 0.7) {

  # Internal helper function to normalize a tag string for comparison:
  # lowercase and strip non-letters
  normalize = function(x) {
    x = tolower(x)
    x = gsub("[^a-z]", "", x)
    return(x)
  }

  # Convert to data.table for efficient processing
  commit.message.tags.dt = data.table::as.data.table(commit.message.tags)
  commit.message.tags.dt[["tag.key"]] = tolower(trimws(commit.message.tags.dt[["tag"]]))

  # Compute the most common display form of each 'tag.key' and
  # the number of occurrences per 'tag.key'
  tag.counts = commit.message.tags.dt[, .(
    tag.display = as.character(names(sort(table(tag), decreasing = TRUE)))[1],
    n           = .N
  ), by = tag.key]

  # Sort by descending occurrence count
  data.table::setorder(tag.counts, -n)

  # Create a normalized version of the display tag for similarity comparisons
  tag.counts[["tag.norm"]] = normalize(tag.counts[["tag.display"]])

  cluster.reps.norm = character(0)
  cluster.reps.display = character(0)
  tag.clean = character(nrow(tag.counts))

  # Iterate over each tag in order of descending frequency and assign it to a cluster
  for (i in seq_len(nrow(tag.counts))) {

    # Get the normalized and display forms of the current tag
    norm.i = tag.counts[["tag.norm"]][i]
    disp.i = tag.counts[["tag.display"]][i]

    # If there are no existing cluster representatives, this tag becomes the first representative
    if (length(cluster.reps.norm) == 0) {
      cluster.reps.norm = norm.i
      cluster.reps.display = disp.i
      tag.clean[i] = disp.i
      next
    }

    # Compute the normalized edit distance to each existing cluster representative
    dists = utils::adist(norm.i, cluster.reps.norm)[1, ]
    max.len = pmax(nchar(norm.i), nchar(cluster.reps.norm))
    similarity = 1 - dists / pmax(max.len, 1)
    best = which.max(similarity)

    # If the best match is above the similarity threshold, assign this tag to that cluster
    if (similarity[best] >= similarity.threshold) {
      tag.clean[i] = cluster.reps.display[best]
    } else {
      # Otherwise, this tag becomes a new cluster representative
      cluster.reps.norm = c(cluster.reps.norm, norm.i)
      cluster.reps.display = c(cluster.reps.display, disp.i)
      tag.clean[i] = disp.i
    }
  }

  tag.counts[["tag.clean"]] = tag.clean

  # Merge the cleaned tag names back into the original commit-message tags data.table
  result = merge(commit.message.tags.dt, tag.counts[, .(tag.key, tag.clean)],
                 by = "tag.key", all.x = TRUE, sort = FALSE)
  result[["tag.key"]] = NULL

  return(result)
}

