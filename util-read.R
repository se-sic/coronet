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
## Copyright 2016-2018 by Claus Hunsen <hunsen@fim.uni-passau.de>
## Copyright 2017 by Raphael Nömmer <noemmer@fim.uni-passau.de>
## Copyright 2017-2018 by Christian Hechtl <hechtl@fim.uni-passau.de>
## Copyright 2017 by Felix Prasse <prassefe@fim.uni-passau.de>
## Copyright 2017-2018 by Thomas Bock <bockthom@fim.uni-passau.de>
## Copyright 2018 by Jakob Kronawitter <kronawij@fim.uni-passau.de>
## All Rights Reserved.


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Libraries ---------------------------------------------------------------

requireNamespace("logging") # for logging
requireNamespace("parallel") # for parallel computation
requireNamespace("plyr")
requireNamespace("digest") # for sha1 hashing of IDs
requireNamespace("sqldf") # for SQL-selections on data.frames


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Constants ---------------------------------------------------------------

## The following definition of column names for each individual data source corresponds to the individual extraction
## process of the tool 'codeface-extraction' (https://github.com/se-passau/codeface-extraction; use commit 0700f94 or
## compatible later commit).

## column names of a dataframe containing authors (see file 'authors.list' and function \code{read.authors})
AUTHORS.LIST.COLUMNS = c(
    "author.id", "author.name", "author.email"
)

## declare the datatype for each column in the constant 'AUTHORS.LIST.COLUMNS'
AUTHORS.LIST.DATA.TYPES = c(
    "character", "character", "character"
)

## column names of a dataframe containing commits (see file 'commits.list' and function \code{read.commits})
COMMITS.LIST.COLUMNS = c(
    "commit.id", # id
    "date", "author.name", "author.email", # author information
    "committer.date", "committer.name", "committer.email", # committer information
    "hash", "changed.files", "added.lines", "deleted.lines", "diff.size", # commit information
    "file", "artifact", "artifact.type", "artifact.diff.size" ## commit-dependency information
)

## declare the datatype for each column in the constant 'COMMITS.LIST.COLUMNS'
COMMITS.LIST.DATA.TYPES = c(
    "character",
    "POSIXct", "character", "character",
    "POSIXct", "character", "character",
    "character", "numeric", "numeric", "numeric", "numeric",
    "character", "character", "character", "numeric"
)

## column names of a dataframe containing issues (see file 'issues.list' and function \code{read.issues})
ISSUES.LIST.COLUMNS = c(
    "issue.id", "issue.title", "issue.type", "issue.state", "issue.resolution", "creation.date", "closing.date", "issue.components", # issue information
    "event.type", # event type
    "author.name", "author.email", # auhtor information
    "event.date", "event.info.1", "event.info.2" # event details
)

## declare the datatype for each column in the constant 'ISSUES.LIST.COLUMNS'
ISSUES.LIST.DATA.TYPES = c(
    "character", "character", "character", "character" ,"character","POSIXct", "POSIXct", "character",
    "character",
    "character", "character",
    "POSIXct", "character", "character"
)

## column names of a dataframe containing mails (see file 'mails.list' and function \code{read.mails})
MAILS.LIST.COLUMNS = c(
    "author.name", "author.email", # author information
    "message.id", "date", "date.offset", "subject", # meta information
    "thread" # thread ID
)

## declare the datatype for each column in the constant 'MAILS.LIST.COLUMNS'
MAILS.LIST.DATA.TYPES = c(
    "character", "character",
    "character", "POSIXct", "numeric", "character",
    "numeric"
)

## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Empty dataframe creation-------------------------------------------------

#' Create an empty dataframe which has the same shape as a dataframe containing authors. The dataframe has the column
#' names and column datatypes defined in \code{AUTHORS.LIST.COLUMNS} and \code{AUTHORS.LIST.DATA.TYPES}, respectively.
#'
#' @return the empty dataframe
create.empty.authors.list = function() {
    return (create.empty.data.frame(AUTHORS.LIST.COLUMNS, AUTHORS.LIST.DATA.TYPES))
}

#' Create an empty dataframe which has the same shape as a dataframe containing commits. The dataframe has the column
#' names and column datatypes defined in \code{COMMITS.LIST.COLUMNS} and \code{COMMITS.LIST.DATA.TYPES}, respectively.
#'
#' @return the empty dataframe
create.empty.commits.list = function() {
    return (create.empty.data.frame(COMMITS.LIST.COLUMNS, COMMITS.LIST.DATA.TYPES))
}

#' Create an empty dataframe which has the same shape as a dataframe containing issues. The dataframe has the column
#' names and column datatypes defined in \code{ISSUES.LIST.COLUMNS} and \code{ISSUES.LIST.DATA.TYPES}, respectively.
#'
#' @return the empty dataframe
create.empty.issues.list = function() {
    return (create.empty.data.frame(ISSUES.LIST.COLUMNS, ISSUES.LIST.DATA.TYPES))
}

#' Create an empty dataframe which has the same shape as a dataframe containing mails. The dataframe has the column
#' names and column datatypes defined in \code{MAILS.LIST.COLUMNS} and \code{MAILS.LIST.DATA.TYPES}, respectively.
#'
#' @return the empty dataframe
create.empty.mails.list = function() {
    return (create.empty.data.frame(MAILS.LIST.COLUMNS, MAILS.LIST.DATA.TYPES))
}


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Commit data -------------------------------------------------------------

#' Read the commits from the 'commits.list' file.
#'
#' @param data.path the path to the commit list
#' @param artifact the artifact whose commits are read
#'
#' @return the read commits
read.commits = function(data.path, artifact) {
    logging::logdebug("read.commits: starting.")

    file = file.path(data.path, "commits.list")

    ## read data.frame from disk (as expected from save.list.to.file) [can be empty]
    commit.data = try(read.table(file, header = FALSE, sep = ";", strip.white = TRUE,
                                 encoding = "UTF-8"), silent = TRUE)

    ## handle the case that the list of commits is empty
    if (inherits(commit.data, "try-error")) {
        logging::logwarn("There are no commits available for the current environment.")
        logging::logwarn("Datapath: %s", data.path)

        # return a dataframe with the correct columns but zero rows
        return(create.empty.commits.list())
    }

    ## assign prepared column names to the dataframe
    colnames(commit.data) = COMMITS.LIST.COLUMNS

    ## remove duplicated lines (even if they contain different commit ids but the same commit hash)
    commit.data = commit.data[rownames(unique(commit.data[, -1])), ]

    ## aggregate lines which are identical except for the "artifact.diff.size" column (ignoring the commit id)
    ## 1) select columns which have to be identical
    primary.columns = COMMITS.LIST.COLUMNS[!(COMMITS.LIST.COLUMNS %in% c("commit.id", "artifact.diff.size"))]
    ## 2) aggregate "artifact.diff.size" for identical rows of the selected columns
    commit.data.without.id = aggregate(commit.data["artifact.diff.size"],
                                       commit.data[primary.columns],
                                       function(sizes) { as.integer(round(mean(sizes))) })
    ## 3) keep only one commit id for identical rows of the selected columns
    commit.data.without.artifact.diff.size = aggregate(commit.data["commit.id"],
                                                       commit.data[primary.columns],
                                                       min)
    ## 4) merge the data again to have both "commit.id" and "artifact.diff.size" in one data.frame again
    commit.data = merge(commit.data.without.id, commit.data.without.artifact.diff.size)
    ## 5) reorder the columns of the data.frame as their order might be changed during aggregating and merging
    commit.data = commit.data[, COMMITS.LIST.COLUMNS]

    ## rewrite data.frame when we want file-based data
    ## (we have proximity-based data as foundation)
    if (artifact == "file") {
        ## aggregate diff size by hash and file
        commit.data = sqldf::sqldf("SELECT *, SUM(`artifact.diff.size`) AS diffsum
                                    FROM `commit.data`
                                    GROUP BY `hash`, `file`
                                    ORDER BY `date`, `author.name`, `commit.id`, `file`, `artifact`")

        ## fix column class for diffsum
        commit.data["diffsum"] = as.numeric(commit.data[["diffsum"]])

        ## copy columns to match proper layout for further analyses
        commit.data["artifact"] = commit.data[["file"]]
        commit.data["artifact.type"] = "File"
        commit.data["artifact.diff.size"] = commit.data[["diffsum"]]
        commit.data["diffsum"] = NULL # remove
    }

    ## rewrite data.frame when we want function-based data
    ## (we have proximity-based data as foundation)
    if (artifact == "function") {
        ## artifact = file name + "::" . function name
        artifacts.new = paste(commit.data[["file"]], commit.data[["artifact"]], sep = "::")

        ## clean up empty artifacts and File_Level artifact
        artifacts.new = gsub("^::$", "", artifacts.new)
        artifacts.new = gsub("^(.*)::File_Level$", "File_Level", artifacts.new)

        ## insert new artifact names into commit table
        commit.data["artifact"] = artifacts.new
    }

    ## Commits to files that are not tracked by Codeface have the empty string in the file and artifact column.
    ## To better indicate this, the 'artifact' and 'file' column value is changed to 'untracked.file'.
    commit.data["file"] = ifelse(commit.data[["file"]] == "", UNTRACKED.FILE, commit.data[["file"]])

    ## copy the file column if file level analysis is performed
    if (artifact == "file") {
        commit.data["artifact"] = commit.data[["file"]]
    }

    ## convert dates and sort by them
    commit.data[["date"]] = get.date.from.string(commit.data[["date"]])
    commit.data[["committer.date"]] = get.date.from.string(commit.data[["committer.date"]])
    commit.data = commit.data[order(commit.data[["date"]], decreasing = FALSE), ] # sort!

    ## set pattern for commit ID for better recognition
    commit.data[["commit.id"]] = sprintf("<commit-%s>", commit.data[["commit.id"]])
    row.names(commit.data) = seq_len(nrow(commit.data))

    ## store the commit data
    logging::logdebug("read.commits: finished.")
    return(commit.data)
}


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Synchronicity data ------------------------------------------------------

#' Read the synchronicity data from file. The name of the file follows
#' the following pattern: 'commit_sync_analysis_artifact_time.window.dat',
#' where artifact and time.window are the given variables.
#'
#' @param data.path the path to the synchronicity data
#' @param artifact the artifact whose synchronicity data get read
#' @param time.window the time window of the data to be read
#'
#' @return the read synchronicity data
read.synchronicity = function(data.path, artifact, time.window) {
    logging::logdebug("read.synchronicity: starting.")

    ## check time.window
    allowed.time.windows = c(1, 5, 10, 15)
    stopifnot((time.window) %in% allowed.time.windows)

    ## construct file
    file.name = sprintf("commit_sync_analysis_%ss_%s.dat", artifact, time.window)
    file = file.path(data.path, file.name)

    ## handle the case that the synchronicity data is empty
    if (!file.exists(file)) {
        logging::logwarn("There are no synchronicity data available for the current environment.")
        logging::logwarn("Datapath: %s", data.path)
        return(data.frame())
    }

    ## load commit.ids object
    load(file = file)
    synchronous.commits = data.frame(hash = commit.hashes[["synchronous"]], synchronous = TRUE)
    nonsynchronous.commits = data.frame(hash = commit.hashes[["non.synchronous"]], synchronous = FALSE)

    ## construct data.frame
    synchronicity = plyr::rbind.fill(synchronous.commits, nonsynchronous.commits)

    ## store the synchronicity data
    logging::logdebug("read.synchronicity: finished.")
    return(synchronicity)
}


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Mail data ---------------------------------------------------------------

#' Read the mail data from the 'emails.list' file.
#'
#' @param data.path the path to the mail data
#'
#' @return the read mail data
read.mails = function(data.path) {
    logging::logdebug("read.mails: starting.")

    ## get file name of commit data
    file = file.path(data.path, "emails.list")

    ## read data.frame from disk (as expected from save.list.to.file) [can be empty]
    mail.data = try(read.table(file, header = FALSE, sep = ";", strip.white = TRUE,
                               encoding = "UTF-8"), silent = TRUE)

    ## handle the case that the list of mails is empty
    if (inherits(mail.data, "try-error")) {
        logging::logwarn("There are no mails available for the current environment.")
        logging::logwarn("Datapath: %s", data.path)
        return(create.empty.mails.list())
    }


    colnames(mail.data) = MAILS.LIST.COLUMNS

    ## set pattern for thread ID for better recognition
    mail.data[["thread"]] = sprintf("<thread-%s>", mail.data[["thread"]])

    ## set proper artifact type for proper vertex attribute 'artifact.type'
    mail.data["artifact.type"] = "Mail"

    ## remove mails without a proper date as they mess up directed mail-based networks
    ## this basically only applies for project-level analysis
    empty.dates = which(mail.data[["date"]] == "" | is.na(mail.data[["date"]]))
    if (length(empty.dates) > 0)
        mail.data = mail.data[-empty.dates, ]

    ## convert dates and sort by them
    mail.data[["date"]] = get.date.from.string(mail.data[["date"]])
    mail.data = mail.data[order(mail.data[["date"]], decreasing = FALSE), ] # sort!

    ## remove all mails with dates before 1990-01-01 00:00:00
    break.date = get.date.from.string("1970-01-01 00:00:00")
    break.to.cut = mail.data[["date"]] < break.date
    mail.data = mail.data[!break.to.cut, ]
    if (sum(break.to.cut) > 0) {
        logging::logwarn(
            "Removed %s e-mail(s) after reading data file due to obiously wrong dates (before %s).",
            sum(break.to.cut), break.date
        )
    }

    ## store the mail data
    logging::logdebug("read.mails: finished.")
    return(mail.data)
}


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Author data -------------------------------------------------------------

#' Read the author data from the 'authors.list' file.
#'
#' @param data.path the path to the author data
#'
#' @return the read author data
read.authors = function(data.path) {
    logging::logdebug("read.authors: starting.")

    ## get file name of commit data
    file = file.path(data.path, "authors.list")

    ## read data.frame from disk (as expected from save.list.to.file) [can be empty]
    authors.df = try(read.table(file, header = FALSE, sep = ";", strip.white = TRUE,
                                encoding = "UTF-8"), silent = TRUE)

    ## break if the list of authors is empty
    if (inherits(authors.df, "try-error")) {
        logging::logerror("There are no authors available for the current environment.")
        logging::logwarn("Datapath: %s", data.path)
        stop("Stopped due to missing authors.")
    }

    ## if there is no third column, we need to add e-mail-address dummy data (NAs)
    if (ncol(authors.df) != length(AUTHORS.LIST.COLUMNS)) {
        authors.df[3] = NA
    }
    colnames(authors.df) = AUTHORS.LIST.COLUMNS

    ## store the ID--author mapping
    logging::logdebug("read.authors: finished.")
    return(authors.df)
}


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## PaStA data --------------------------------------------------------------

#' Read and parse the PaStA data from the 'mbox-result' file.
#' The form in the file is : <message-id> <possibly another message.id> ... => commit.hash commit.hash2 ....
#' The parsed form is a data frame with message IDs as keys, commit hashes as values, and a revision set id.
#' If the message ID does not get mapped to a commit hash, the value for the commit hash is \code{NA}.
#'
#' @param data.path the path to the PaStA data
#'
#' @return the read and parsed PaStA data
read.pasta = function(data.path) {
    # constant for seperating keys and value
    SEPERATOR = " => "
    KEY.SEPERATOR = " "

    ## get file name of PaStA data
    filepath = file.path(data.path, "mbox-result")

    ## read data from disk [can be empty]
    lines = suppressWarnings(try(readLines(filepath), silent = TRUE))

    ## handle the case if the list of PaStA items is empty
    if (inherits(lines, "try-error")) {
        logging::logwarn("There are no PaStA data available for the current environment.")
        logging::logwarn("Datapath: %s", data.path)
        return(data.frame())
    }

    result.list = parallel::mcmapply(lines, seq_along(lines), SIMPLIFY = FALSE, FUN = function(line, line.id) {
        #line = lines[i]
        if ( nchar(line) == 0 ) {
            return(NULL)
        }

        if (!grepl("<", line)) {
            logging::logwarn("Faulty line: %s", line)
            return(NULL)
        }

        # 1) split at arrow
        # 2) split keys
        # 3) split values
        # 4) insert all key-value pairs by iteration (works also if there is only one key)
        if (grepl(SEPERATOR, line)) {
            line.split = unlist(strsplit(line, SEPERATOR))
            keys = line.split[1]
            values = line.split[2]
            keys.split = unlist(strsplit(keys, KEY.SEPERATOR))
            values.split = unlist(strsplit(values, KEY.SEPERATOR))
        } else {
            keys.split = unlist(strsplit(line, KEY.SEPERATOR))
            values.split = NA
        }

        # Transform data to data.frame
        df = merge(keys.split, values.split)
        colnames(df) = c("message.id", "commit.hash")
        df["revision.set.id"] = sprintf("<revision-set-%s>", line.id)
        return(df)
    })
    result.df = plyr::rbind.fill(result.list)
    logging::logdebug("read.pasta: finished.")
    return(result.df)
}

## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Issue data --------------------------------------------------------------

#' Read and parse the issue data from the 'issues.list' file.
#'
#' @param data.path the path to the issue data
#'
#' @return the read and parsed issue data
read.issues_old = function(data.path) {
    logging::logdebug("read.issues: starting.")

    ## get file name of issue data
    filepath = file.path(data.path, "issues.list")

    ## read issues from disk [can be empty]
    issue.data = try(read.table(filepath, header = FALSE, sep = ";", strip.white = TRUE,
                                encoding = "UTF-8"), silent = TRUE)

    ## handle the case that the list of commits is empty
    if (inherits(issue.data, "try-error")) {
        logging::logwarn("There are no Github issue data available for the current environment.")
        logging::logwarn("Datapath: %s", data.path)
        return(create.empty.issues.list())
    }

    ## set proper column names
    colnames(issue.data) = ISSUES.LIST.COLUMNS

    ## set pattern for issue ID for better recognition
    issue.data[["issue.id"]] = sprintf("<issue-%s>", issue.data[["issue.id"]])

    ## set proper artifact type for proper vertex attribute 'artifact.type'
    issue.data["artifact.type"] = "IssueEvent"

    ## convert 'is.pull.request' column to logicals
    issue.data[["is.pull.request"]] = as.logical(issue.data[["is.pull.request"]])

    ## convert dates and sort by 'date' column
    issue.data[["date"]] = get.date.from.string(issue.data[["date"]])
    issue.data[["creation.date"]] = get.date.from.string(issue.data[["creation.date"]])
    issue.data[["closing.date"]][ issue.data[["closing.date"]] == "" ] = NA
    issue.data[["closing.date"]] = get.date.from.string(issue.data[["closing.date"]])
    issue.data = issue.data[order(issue.data[["date"]], decreasing = FALSE), ] # sort!

    ## generate a unique event ID from issue ID, author, and date
    issue.data[["event.id"]] = sapply(
        paste(issue.data[["issue.id"]], issue.data[["author.name"]], issue.data[["date"]], sep = "_"),
        function(event) { digest::digest(event, algo="sha1", serialize = FALSE) }
    )

    logging::logdebug("read.issues: finished.")
    return(issue.data)
}

read.issues = function(data.path) {
    logging::logdebug("read.issues_new: starting.")

    ## get file name of issue data
    filepath = file.path(data.path, "new_issues.list")

    ## read issues from disk [can be empty]
    issue.data = try(read.table(filepath, header = FALSE, sep = ";", strip.white = TRUE,
                                encoding = "UTF-8"), silent = TRUE)

    ## handle the case that the list of commits is empty
    if (inherits(issue.data, "try-error")) {
        logging::logwarn("There are no Github issue data available for the current environment.")
        logging::logwarn("Datapath: %s", data.path)
        return(data.frame())
    }

    ## set proper column names
    colnames(issue.data) = c(
        "issue.id", "issue.title", "issue.type", "issue.state", "issue.resolution", "creation.date", "closing.date",
        "issue.components", "event.type", "author.name", "author.email", "event.date", "event.info.1", "event.info.2"
    )

    ## set pattern for issue ID for better recognition
    issue.data[["issue.id"]] = sprintf("<issue-%s>", issue.data[["issue.id"]])

    ## set proper artifact type for proper vertex attribute 'artifact.type'
    issue.data["artifact.type"] = "IssueEvent"

    ## convert issue 'type', 'resolution' and 'components' to vectors
    issue.data[["issue.type"]] = as.vector(issue.data[["issue.type"]])
    issue.data[["issue.resolution"]] = as.vector(issue.data[["issue.resolution"]])
    issue.data[["issue.components"]] = as.vector(issue.data[["issue.components"]])

    ## convert 'event.info.2' for created and comment events to vector
    if (issue.data[["event.type"]] == "created" || issue.data[["event.type"]] == "commented"){
        issue.data[["event.info.1"]] = as.vector(issue.data[["event.info.2"]])
    }

    ## convert dates and sort by 'date' column
    issue.data[["event.date"]] = get.date.from.string(issue.data[["event.date"]])
    issue.data[["creation.date"]] = get.date.from.string(issue.data[["creation.date"]])
    issue.data[["closing.date"]][ issue.data[["closing.date"]] == "" ] = NA
    issue.data[["closing.date"]] = get.date.from.string(issue.data[["closing.date"]])
    issue.data = issue.data[order(issue.data[["event.date"]], decreasing = FALSE), ] # sort!

    ## generate a unique event ID from issue ID, author, and date
    issue.data[["event.id"]] = sapply(
        paste(issue.data[["issue.id"]], issue.data[["author.name"]], issue.data[["event.date"]], sep = "_"),
        function(event) { digest::digest(event, algo="sha1", serialize = FALSE) }
    )

    logging::logdebug("read.issues: finished.")
    return(issue.data)
}
