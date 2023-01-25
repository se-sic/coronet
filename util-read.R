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
## Copyright 2016-2019 by Claus Hunsen <hunsen@fim.uni-passau.de>
## Copyright 2017 by Raphael Nömmer <noemmer@fim.uni-passau.de>
## Copyright 2017-2018 by Christian Hechtl <hechtl@fim.uni-passau.de>
## Copyright 2020-2022 by Christian Hechtl <hechtl@cs.uni-saarland.de>
## Copyright 2017 by Felix Prasse <prassefe@fim.uni-passau.de>
## Copyright 2017-2018 by Thomas Bock <bockthom@fim.uni-passau.de>
## Copyright 2018 by Jakob Kronawitter <kronawij@fim.uni-passau.de>
## Copyright 2018-2019 by Anselm Fehnker <fehnker@fim.uni-passau.de>
## Copyright 2020-2021, 2023 by Niklas Schneider <s8nlschn@stud.uni-saarland.de>
## Copyright 2021 by Johannes Hostert <s8johost@stud.uni-saarland.de>
## Copyright 2021 by Mirabdulla Yusifli <s8miyusi@stud.uni-saarland.de>
## Copyright 2022 by Jonathan Baumann <joba00002@stud.uni-saarland.de>
## Copyright 2022-2023 by Maximilian Löffler <s8maloef@stud.uni-saarland.de>
## All Rights Reserved.

## Note:
## The definition of column names for each individual data source used in this file corresponds to the individual
## extraction process of the tool 'codeface-extraction' (https://github.com/se-sic/codeface-extraction; use
## commit 0700f94 or a compatible later commit).


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Libraries ---------------------------------------------------------------

requireNamespace("logging") # for logging
requireNamespace("parallel") # for parallel computation
requireNamespace("plyr")
requireNamespace("digest") # for sha1 hashing of IDs
requireNamespace("sqldf") # for SQL-selections on data.frames
requireNamespace("data.table") # for faster data.frame processing

## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Helper functions --------------------------------------------------------

#' Remove the "deleted user" or the author with empty name "" from a data frame.
#'
#' @param data the data from which to remove the "deleted user" and author with empty name.
#' @param columns the columns in which to search for the "deleted user" and author with empty name.
#'                [default: c("author.name")]
#' 
#' @return the data frame without the rows in which the author name is "deleted user" or ""
remove.deleted.and.empty.user = function(data, columns = c("author.name")) {
    if (!all(columns %in% colnames(data))) {
        logging::logerror("The given columns are not present in the data.frame.")
        stop("Stopped due to invalid column names.")    
    }

    ## loop over the given columns and remove all rows in which the author name is "deleted user" or ""
    for (column in columns) {
        data = data[tolower(data[, column]) != "deleted user" & data[column] != "", ]
    }   
    return(data)
}

## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Main data sources -------------------------------------------------------

## * Commit data -----------------------------------------------------------

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
    "character", "integer", "integer", "integer", "integer",
    "character", "character", "character", "integer"
)

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
    if (inherits(commit.data, "try-error") || nrow(commit.data) < 1) {
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

    ## Commits to files that are not tracked by Codeface have the empty string in the file and artifact column.
    ## To better indicate this, the 'file' column value is changed to 'untracked.file'.
    commit.data["file"] = ifelse(commit.data[["file"]] == "", UNTRACKED.FILE, commit.data[["file"]])

    ## rewrite data.frame when we want file-based data
    ## (we have proximity-based data as foundation)
    if (artifact == "file") {
        ## aggregate diff size by hash and file
        commit.data = sqldf::sqldf("SELECT *, SUM(`artifact.diff.size`) AS diffsum
                                    FROM `commit.data`
                                    GROUP BY `hash`, `file`
                                    ORDER BY `date`, `author.name`, `commit.id`, `file`, `artifact`")

        ## fix column class for diffsum
        commit.data["diffsum"] = as.integer(commit.data[["diffsum"]])

        ## copy columns to match proper layout for further analyses
        commit.data["artifact"] = commit.data[["file"]]
        commit.data["artifact.type"] = ifelse(commit.data[["file"]] == UNTRACKED.FILE,
                                              UNTRACKED.FILE.EMPTY.ARTIFACT.TYPE,
                                              "File")
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

    ## Commits to files that are not tracked by Codeface have the empty string in the file, artifact, and
    ## artifact-type column. To better indicate this, the correpsonding column values are adapted.
    commit.data["artifact"] = ifelse(commit.data[["artifact"]] == "",
                                     UNTRACKED.FILE.EMPTY.ARTIFACT,
                                     commit.data[["artifact"]])
    commit.data["artifact.type"] = ifelse(commit.data[["artifact.type"]] == "",
                                          UNTRACKED.FILE.EMPTY.ARTIFACT.TYPE,
                                          commit.data[["artifact.type"]])

    commit.data = remove.deleted.and.empty.user(commit.data, c("author.name", "committer.name")) # filter deleted user

    ## convert dates and sort by them
    commit.data[["date"]] = get.date.from.string(commit.data[["date"]])
    commit.data[["committer.date"]] = get.date.from.string(commit.data[["committer.date"]])
    commit.data = commit.data[order(commit.data[["date"]], decreasing = FALSE), ] # sort!

    ## set pattern for commit ID for better recognition
    commit.data[["commit.id"]] = format.commit.ids(commit.data[["commit.id"]])
    row.names(commit.data) = seq_len(nrow(commit.data))

    ## check that dataframe is of correct shape
    verify.data.frame.columns(commit.data, COMMITS.LIST.COLUMNS, COMMITS.LIST.DATA.TYPES)

    ## store the commit data
    logging::logdebug("read.commits: finished.")
    return(commit.data)
}

#' Create an empty dataframe which has the same shape as a dataframe containing commits. The dataframe has the column
#' names and column datatypes defined in \code{COMMITS.LIST.COLUMNS} and \code{COMMITS.LIST.DATA.TYPES}, respectively.
#'
#' @return the empty dataframe
create.empty.commits.list = function() {
    return (create.empty.data.frame(COMMITS.LIST.COLUMNS, COMMITS.LIST.DATA.TYPES))
}

## * Mail data -------------------------------------------------------------

## column names of a dataframe containing mails (see file 'mails.list' and function \code{read.mails})
MAILS.LIST.COLUMNS = c(
    "author.name", "author.email", # author information
    "message.id", "date", "date.offset", "subject", # meta information
    "thread", # thread ID
    "artifact.type" # artifact type
)

## declare the datatype for each column in the constant 'MAILS.LIST.COLUMNS'
MAILS.LIST.DATA.TYPES = c(
    "character", "character",
    "character", "POSIXct", "integer", "character",
    "character",
    "character"
)

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
    if (inherits(mail.data, "try-error") || nrow(mail.data) < 1) {
        logging::logwarn("There are no mails available for the current environment.")
        logging::logwarn("Datapath: %s", data.path)
        return(create.empty.mails.list())
    }

    ## set proper artifact type for proper vertex attribute 'artifact.type'
    mail.data["artifact.type"] = "Mail"

    colnames(mail.data) = MAILS.LIST.COLUMNS

    ## set pattern for thread ID for better recognition
    mail.data[["thread"]] = sprintf("<thread-%s>", mail.data[["thread"]])

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
    mail.data = remove.deleted.and.empty.user(mail.data) # filter deleted user

    ## check that dataframe is of correct shape
    verify.data.frame.columns(mail.data, MAILS.LIST.COLUMNS, MAILS.LIST.DATA.TYPES)

    ## store the mail data
    logging::logdebug("read.mails: finished.")
    return(mail.data)
}

#' Create an empty dataframe which has the same shape as a dataframe containing mails. The dataframe has the column
#' names and column datatypes defined in \code{MAILS.LIST.COLUMNS} and \code{MAILS.LIST.DATA.TYPES}, respectively.
#'
#' @return the empty dataframe
create.empty.mails.list = function() {
    return (create.empty.data.frame(MAILS.LIST.COLUMNS, MAILS.LIST.DATA.TYPES))
}

## * Issue data ------------------------------------------------------------

## column names of a dataframe containing issues (see file 'issues.list' and function \code{read.issues})
ISSUES.LIST.COLUMNS = c(
    "issue.id", "issue.title", "issue.type", "issue.state", "issue.resolution", "creation.date", "closing.date", "issue.components", # issue information
    "event.name", # event type
    "author.name", "author.email", # auhtor information
    "date", "event.info.1", "event.info.2", "event.id", # event details
    "issue.source", # source information
    "artifact.type" # artifact type
)

## declare the datatype for each column in the constant 'ISSUES.LIST.COLUMNS'
ISSUES.LIST.DATA.TYPES = c(
    "character", "character", "list()", "character", "list()", "POSIXct", "POSIXct", "list()",
    "character",
    "character", "character",
    "POSIXct", "character", "list()", "character",
    "character",
    "character"
)

#' Read and parse the issue data from the 'issues.list' file.
#'
#' Note: The dates in the \code{"date"} column may be remapped to the creation date of the corresponding issue,
#' especially for \code{"commit_added"} events. This happens when the event has happened before the issue creation date.
#' The original date of these events can always be found in the \code{"event.info.2"} column.
#'
#' @param data.path the path to the issue data
#' @param issues.sources the sources of the issue data. One or both of \code{"jira"} and \code{"github"}.
#'
#' @return the read and parsed issue data
read.issues = function(data.path, issues.sources = c("jira", "github")) {
    logging::logdebug("read.issues: starting.")

    ## check arguments
    issues.sources = match.arg(arg = issues.sources, several.ok = TRUE)

    ## read data from chosen sources
    issue.data = lapply(issues.sources, function(issue.source) {

        ## get file name of source issue data
        filepath = file.path(data.path, sprintf("issues-%s.list", issue.source))

        ## read source issues from disk [can be empty]
        source.data = try(read.table(filepath, header = FALSE, sep = ";", strip.white = TRUE,
                                    encoding = "UTF-8"), silent = TRUE)

        ## handle the case that the list of issues is empty
        if (inherits(source.data, "try-error") || nrow(source.data) < 1) {
            logging::logwarn("There are no %s issue data available for the current environment.", issue.source)
            logging::logwarn("Datapath: %s", data.path)
            return(create.empty.issues.list())
        }

        ## create (now empty) column 'event.id' to properly set column names
        ## (this column is reset later)
        source.data[["event.id"]] = NA

        ## add source column to data
        source.data["issue.source"] = issue.source

        ## set proper artifact type for proper vertex attribute 'artifact.type'
        source.data["artifact.type"] = "IssueEvent"

        ## set proper column names
        colnames(source.data) = ISSUES.LIST.COLUMNS
        return(source.data)
    })

    ## combine issue data from all sources
    issue.data = do.call(rbind, issue.data)

    ## if no chosen source is present exit early by returning the (combined) empty issues list
    if (nrow(issue.data) < 1) {
        return(issue.data)
    }

    ## set pattern for issue ID for better recognition
    issue.data[["issue.id"]] = sprintf("<issue-%s-%s>", issue.data[["issue.source"]], issue.data[["issue.id"]])

    ## properly parse and store data in list-type columns
    issue.data[["issue.type"]] = I(unname(lapply(issue.data[["issue.type"]], jsonlite::fromJSON, simplifyVector = FALSE)))
    issue.data[["issue.resolution"]] = I(unname(lapply(issue.data[["issue.resolution"]], jsonlite::fromJSON, simplifyVector = FALSE)))
    issue.data[["issue.components"]] = I(unname(lapply(issue.data[["issue.components"]], jsonlite::fromJSON, simplifyVector = FALSE)))
    issue.data[["event.info.2"]] = I(unname(lapply(issue.data[["event.info.2"]], jsonlite::fromJSON, simplifyVector = FALSE)))

    ## convert dates and sort by 'date' column
    issue.data[["date"]] = get.date.from.string(issue.data[["date"]])
    issue.data[["creation.date"]] = get.date.from.string(issue.data[["creation.date"]])
    issue.data[["closing.date"]] = get.date.from.string(issue.data[["closing.date"]])

    if (nrow(issue.data) > 0) {
        ## fix all dates to be after the creation date
        ## violations can happen for "commit_added" events if the commit was made before the PR was opened
        ## the original date for "commit_added" events is stored in "event.info.2" in any case
        commit.added.events = issue.data[["event.name"]] == "commit_added"
        issue.data[commit.added.events, "event.info.2"] = get.date.string(issue.data[commit.added.events, "date"])
        commit.added.events.before.creation = commit.added.events &
            !is.na(issue.data["creation.date"]) & (issue.data["date"] < issue.data["creation.date"])
        issue.data[commit.added.events.before.creation, "date"] = issue.data[commit.added.events.before.creation, "creation.date"]
        issue.data = remove.deleted.and.empty.user(issue.data) # filter deleted user
        issue.data = issue.data[order(issue.data[["date"]], decreasing = FALSE), ] # sort!
    }

    ## generate a unique event ID from issue ID, author, and date
    issue.data[["event.id"]] = sapply(
        paste(issue.data[["issue.id"]], issue.data[["author.name"]], issue.data[["date"]], sep = "_"),
        function(event) { digest::digest(event, algo="sha1", serialize = FALSE) }
    )

    ## check that dataframe is of correct shape
    verify.data.frame.columns(issue.data, ISSUES.LIST.COLUMNS, ISSUES.LIST.DATA.TYPES)

    logging::logdebug("read.issues: finished.")
    return(issue.data)
}

#' Create an empty dataframe which has the same shape as a dataframe containing issues. The dataframe has the column
#' names and column datatypes defined in \code{ISSUES.LIST.COLUMNS} and \code{ISSUES.LIST.DATA.TYPES}, respectively.
#'
#' @return the empty dataframe
create.empty.issues.list = function() {
    return (create.empty.data.frame(ISSUES.LIST.COLUMNS, ISSUES.LIST.DATA.TYPES))
}


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Additional data sources -------------------------------------------------

## * Author data -----------------------------------------------------------



## column names of a data frame containing bot information (see file
## 'bots.list' and function \code{read.bot.list})
BOT.LIST.COLUMNS = c(
    "author.name", "author.email", ## author
    "is.bot" ## whether this is a bot
)

#' Read the bot classification from the 'bots.list' file.
#'
#' @param data.path the path to the commit-messages list
#'
#' @return a data frame with author.name, author.email, and a (potentially NA) boolean whether this is a bot,
#'         or \code{NULL} if the above file is not present.
read.bot.info = function(data.path) {
    logging::logdebug("read.bot.info: starting.")

    ## read the file with the bot info
    file = file.path(data.path, "bots.list")

    bot.data = try(read.table(file, header = FALSE, sep = ";", strip.white = TRUE,
                                         encoding = "UTF-8"), silent = TRUE)

    ## handle the case that the bot info is empty
    if (inherits(bot.data, "try-error")) {
        logging::logwarn("There is no bot information available for the current environment.")
        logging::logwarn("Datapath: %s", data.path)

        ## return NULL. Creating an empty dataframe is not possible
        ## because no type information about bot information is present
        return(NULL)
    }

    ## set column names for new data frame
    colnames(bot.data) = BOT.LIST.COLUMNS
    bot.data["is.bot"] = sapply(bot.data[["is.bot"]], function(x) switch(x, Bot = TRUE, Human = FALSE, NA))

    ## check that dataframe is of correct shape
    verify.data.frame.columns(bot.data, BOT.LIST.COLUMNS)

    logging::logdebug("read.bot.info: finished.")
    return(bot.data)
}


## column names of a dataframe containing authors (see file 'authors.list' and function \code{read.authors})
AUTHORS.LIST.COLUMNS = c(
    "author.id", "author.name", "author.email", "is.bot"
)

## column names of a dataframe containing authors, before adding bot data.
AUTHORS.LIST.COLUMNS.WITHOUT.BOTS = AUTHORS.LIST.COLUMNS[1:3]

## declare the datatype for each column in the constant 'AUTHORS.LIST.COLUMNS'
AUTHORS.LIST.DATA.TYPES = c(
    "character", "character", "character", "logical"
)

#' Read the author data from the 'authors.list' file.
#'
#' @param data.path the path to the author data
#'
#' @return the read author data
read.authors = function(data.path) {
    logging::logdebug("read.authors: starting.")

    ## get file name of commit data
    file = file.path(data.path, "authors.list")

    ## read data.frame from disk (as expected from save.list.to.file)
    authors.df = try(read.table(file, header = FALSE, sep = ";", strip.white = TRUE,
                                encoding = "UTF-8"), silent = TRUE)


    ## break if the list of authors is empty
    if (inherits(authors.df, "try-error") || nrow(authors.df) < 1) {
        logging::logerror("There are no authors available for the current environment.")
        logging::logwarn("Datapath: %s", data.path)
        stop("Stopped due to missing authors.")
    }

    ## if there is no third column, we need to add e-mail-address dummy data
    if (ncol(authors.df) != length(AUTHORS.LIST.COLUMNS.WITHOUT.BOTS)) {
        authors.df[3] = ""
    }
    colnames(authors.df) = AUTHORS.LIST.COLUMNS.WITHOUT.BOTS

    bot.data = read.bot.info(data.path)
    if (!is.null(bot.data)) {
        authors.df = merge(authors.df, bot.data, by = c("author.name", "author.email"), all.x = TRUE, sort = FALSE)
        authors.df = authors.df[order(authors.df[["author.id"]]), ] # re-order after read
        row.names(authors.df) = seq_len(nrow(authors.df))
    } else {
        ## if bot data is not available, add NA data, which is what would have happened
        ## if the file was empty
        authors.df[["is.bot"]] = NA
    }
    ## re-order the columns
    authors.df = authors.df[, AUTHORS.LIST.COLUMNS]
    authors.df = remove.deleted.and.empty.user(authors.df)

    ## assure type correctness
    authors.df[["author.id"]] = as.character(authors.df[["author.id"]])

    ## check that dataframe is of correct shape
    verify.data.frame.columns(authors.df, AUTHORS.LIST.COLUMNS, AUTHORS.LIST.DATA.TYPES)

    ## store the ID--author mapping
    logging::logdebug("read.authors: finished.")
    return(authors.df)
}

#' Create an empty dataframe which has the same shape as a dataframe containing authors. The dataframe has the column
#' names and column datatypes defined in \code{AUTHORS.LIST.COLUMNS} and \code{AUTHORS.LIST.DATA.TYPES}, respectively.
#'
#' @return the empty dataframe
create.empty.authors.list = function() {
    return (create.empty.data.frame(AUTHORS.LIST.COLUMNS, AUTHORS.LIST.DATA.TYPES))
}


## * Gender data ------------------------------------------------------------

## column names of a dataframe containing gender data (see function \code{read.gender})
GENDER.LIST.COLUMNS = c(
    "author.name", "gender"
)

## declare the datatype for each column in the constant 'GENDER.LIST.COLUMNS'
GENDER.LIST.DATA.TYPES = c(
    "character", "character"
)

## declare predefined values for the gender column
GENDER.LIST.VALUES = c(
    "male", "female", "unknown"
)

#' Read and parse the gender data from the 'gender' file.
#' The parsed form is a data frame with author.name as key, gender as value.
#'
#' @param data.path the path to the gender data
#'
#' @return the read and parsed gender data
read.gender = function(data.path) {

    ## get file name of the gender data
    file = file.path(data.path, "gender.list")

    ## read data.frame from disk (as expected from save.list.to.file) [can be empty]
    ## comment char is set to empty string as the names of developers can contain the
    ## char '#'. This does not affect the other data sources as all names there are
    ## in "".
    gender.data = try(read.table(file, header = FALSE, sep = ";", strip.white = TRUE,
                                 encoding = "UTF-8", comment.char = ""), silent = TRUE)


    ## handle the case if the list of items is empty
    if (inherits(gender.data, "try-error") || nrow(gender.data) < 1) {
        logging::logwarn("There are no gender data available for the current environment.")
        logging::logwarn("Datapath: %s", data.path)
        return(create.empty.gender.list())
    }

    colnames(gender.data) = GENDER.LIST.COLUMNS

    ## check whether there are undefined gender labels
    undefined.labels = setdiff(gender.data[["gender"]], GENDER.LIST.VALUES)

    if (length(undefined.labels) > 0){
        ## find authors who have undefined gender labels
        undefined.labels.authors = filter(gender.data, gender %in% undefined.labels)
        logging::logwarn(sprintf("Undefined gender labels. %s cannot be used. Only %s are allowed.
                                 The following authors have undefined labels: %s ",
                                 paste(shQuote(undefined.labels), collapse = ","),
                                 paste(shQuote(GENDER.LIST.VALUES), collapse = ", "),
                                 paste(shQuote(undefined.labels.authors[["author.name"]]), collapse = ",")))

        ## replace all undefined labels with 'unknown'
        gender.data[["gender"]][gender.data[["gender"]] %in% undefined.labels] = "unknown"
        logging::logwarn("Undefined gender labels have been replaced with 'unknown'.")
    }

    ## replace all 'unknown' values with NA
    gender.data[["gender"]][gender.data[["gender"]] == "unknown"] = NA

    gender.data = gender.data[order(gender.data[["author.name"]]), ] # re-order after read

    ## remove rownames
    rownames(gender.data) = NULL

    ## check that dataframe is of correct shape
    verify.data.frame.columns(gender.data, GENDER.LIST.COLUMNS, GENDER.LIST.DATA.TYPES)

    logging::logdebug("read.gender: finished.")
    return(gender.data)

}

#' Create an empty dataframe which has the same shape as a dataframe containing gender data.
#' The dataframe has the column names and column datatypes defined in \code{GENDER.LIST.COLUMNS}
#' and \code{GENDER.LIST.DATA.TYPES}, respectively.
#'
#' @return the empty dataframe
create.empty.gender.list = function() {
    return (create.empty.data.frame(GENDER.LIST.COLUMNS, GENDER.LIST.DATA.TYPES))
}


## * Commit message data ---------------------------------------------------

## column names of a dataframe containing commit messages (see file
## 'commitMessages.list' and function \code{read.commit.messages})
COMMIT.MESSAGE.LIST.COLUMNS = c(
    "commit.id", # id
    "hash", "title", "message"
)

## declare the datatype for each column in the constant 'COMMIT.MESSAGE.LIST.COLUMNS'
COMMIT.MESSAGE.LIST.DATA.TYPES = c(
    "character",
    "character", "character", "character"
)

## declare the constant (5 spaces) which is used by codeface to separate lines in
## commit messages
COMMIT.MESSAGE.LINE.SEP.CODEFACE = paste0(rep(" ", 5), collapse = "")
## declare the constant to how line breaks should look like in the data
COMMIT.MESSAGE.LINE.SEP.REPLACE = "\n"

#' Read the commit messages from the 'commitMessages.list' file.
#' Turn line breaks represented with five spaces into \n line breaks and
#' ignore initial spaces. Also remove spaces at the beginning and the end of
#' the message.
#'
#' @param data.path the path to the commit-messages list
#'
#' @return a data frame with id, hash, title and message body´
read.commit.messages = function(data.path) {
    logging::logdebug("read.commit.messages: starting.")

    ## read the file with the commit messages
    file = file.path(data.path, "commitMessages.list")

    commit.message.data = try(read.table(file, header = FALSE, sep = ";", strip.white = TRUE,
                                         encoding = "UTF-8"), silent = TRUE)

    ## handle the case that the list of commits is empty
    if (inherits(commit.message.data, "try-error") || nrow(commit.message.data) < 1) {
        logging::logwarn("There are no commit messages available for the current environment.")
        logging::logwarn("Datapath: %s", data.path)

        ## return a dataframe with the correct columns but zero rows
        return(create.empty.commit.message.list())
    }

    ## set column names for new data frame; unprocessed data only has three columns so omit the "title" column
    colnames(commit.message.data) = COMMIT.MESSAGE.LIST.COLUMNS[COMMIT.MESSAGE.LIST.COLUMNS != "title"]
    ## split the message string with the new line symbol
    message.split = strsplit(commit.message.data[["message"]], COMMIT.MESSAGE.LINE.SEP.CODEFACE)

    ## prepare the 'message.split' object so that it contains a two-element vector for each commit
    message.split.df = lapply(message.split, function(tuple) {
        ## clear the message from empty lines
        lines = tuple[tuple != ""]

        ## remove spaces before first line
        lines = gsub("^\\s+", "", lines)
        ## remove spaces at the end of the message
        lines = gsub("\\s+$", "", lines)

        ## set title and message empty in case there was no actual commit message or it was consisting of spaces only
        title = ""
        message = ""

        ## if there is only one line, create an empty body
        if (length(lines) == 1) {
            title = lines[[1]]
        }
        ## if there are more than two lines, merge all except for the first one
        else if (length(lines) >= 2) {
            title = lines[[1]]
            ## use an ascii line break instead
            message = paste(tail(lines, -1), collapse = COMMIT.MESSAGE.LINE.SEP.REPLACE)
        }

        return(data.table::data.table(title = title, message = message))
    })

    ## convert to a data.table with two columns
    message.split.df = data.table::rbindlist(message.split.df)

    ## create a data frame containing all four necessary columns
    commit.message.data["title"] = message.split.df[["title"]] # title
    commit.message.data["message"] = message.split.df[["message"]] # message
    ## reorder columns because they are added alphabetically
    commit.message.data = commit.message.data[, COMMIT.MESSAGE.LIST.COLUMNS]

    ## Make commit.id have numeric type and set row names
    commit.message.data[["commit.id"]] = format.commit.ids(commit.message.data[["commit.id"]])
    row.names(commit.message.data) = seq_len(nrow(commit.message.data))

    ## check that dataframe is of correct shape
    verify.data.frame.columns(commit.message.data, COMMIT.MESSAGE.LIST.COLUMNS, COMMIT.MESSAGE.LIST.DATA.TYPES)

    logging::logdebug("read.commit.messages: finished.")
    return(commit.message.data)
}

#' Create a empty dataframe which has the same shape as a dataframe containing commit messages.
#' The dataframe has the column names and column datatypes defined in \code{COMMIT.MESSAGE.LIST.COLUMNS} and
#' \code{COMMIT.MESSAGE.LIST.DATA.TYPES}, respectively.
#'
#' @return the empty dataframe
create.empty.commit.message.list = function() {
    return (create.empty.data.frame(COMMIT.MESSAGE.LIST.COLUMNS, COMMIT.MESSAGE.LIST.DATA.TYPES))
}

## * PaStA data ------------------------------------------------------------

## column names of a dataframe containing PaStA data (see function \code{read.pasta})
PASTA.LIST.COLUMNS = c(
    "message.id", "commit.hash", "revision.set.id"
)

## declare the datatype for each column in the constant 'PASTA.LIST.COLUMNS'
PASTA.LIST.DATA.TYPES = c(
    "character", "character", "character"
)

#' Read and parse the PaStA data from the 'patch-groups' file.
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
    filepath = file.path(data.path, "patch-groups")

    ## read data from disk [can be empty]
    lines = suppressWarnings(try(readLines(filepath), silent = TRUE))

    ## handle the case if the list of PaStA items is empty
    if (inherits(lines, "try-error") || length(lines) < 1) {
        logging::logwarn("There are no PaStA data available for the current environment.")
        logging::logwarn("Datapath: %s", data.path)
        return(create.empty.pasta.list())
    }

    result.list = parallel::mcmapply(lines, seq_along(lines), SIMPLIFY = FALSE, FUN = function(line, line.id) {
        #line = lines[i]
        if ( nchar(line) == 0 ) {
            return(NULL)
        }

        if (!grepl("<", line)) {
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

    ## check that dataframe is of correct shape
    verify.data.frame.columns(result.df, PASTA.LIST.COLUMNS, PASTA.LIST.DATA.TYPES)

    logging::logdebug("read.pasta: finished.")
    return(result.df)
}

#' Create an empty dataframe which has the same shape as a dataframe containing PaStA data.
#' The dataframe has the column names and column datatypes defined in \code{PASTA.LIST.COLUMNS}
#' and \code{PASTA.LIST.DATA.TYPES}, respectively.
#'
#' @return the empty dataframe
create.empty.pasta.list = function() {
    return (create.empty.data.frame(PASTA.LIST.COLUMNS, PASTA.LIST.DATA.TYPES))
}

## * Synchronicity data ----------------------------------------------------

## column names of a dataframe containing synchronicity data (see function \code{read.synchronicity})
SYNCHRONICITY.LIST.COLUMNS = c(
    "hash", "synchronicity"
)

## declare the datatype for each column in the constant 'SYNCHRONICITY.LIST.COLUMNS'
SYNCHRONICITY.LIST.DATA.TYPES = c(
    "character", "logical"
)

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
    if (!file.exists(file) || file.info(file)$size == 0) {
        logging::logwarn("There are no synchronicity data available for the current environment.")
        logging::logwarn("Datapath: %s", data.path)
        return(create.empty.synchronicity.list())
    }

    ## load commit.ids object
    load(file = file)
    synchronous.commits = data.frame(hash = commit.hashes[["synchronous"]], synchronicity = TRUE)
    nonsynchronous.commits = data.frame(hash = commit.hashes[["non.synchronous"]], synchronicity = FALSE)

    ## construct data.frame
    synchronicity = plyr::rbind.fill(synchronous.commits, nonsynchronous.commits)

    ## ensure proper column names
    colnames(synchronicity) = SYNCHRONICITY.LIST.COLUMNS

    ## check that dataframe is of correct shape
    verify.data.frame.columns(synchronicity, SYNCHRONICITY.LIST.COLUMNS, SYNCHRONICITY.LIST.DATA.TYPES)

    ## store the synchronicity data
    logging::logdebug("read.synchronicity: finished.")
    return(synchronicity)
}

#' Create an empty dataframe which has the same shape as a dataframe containing synchronicity data.
#' The dataframe has the column names and column datatypes defined in \code{SYNCHRONICITY.LIST.COLUMNS}
#' and \code{SYNCHRONICITY.LIST.DATA.TYPES}, respectively.
#'
#' @return the empty dataframe
create.empty.synchronicity.list = function() {
    return (create.empty.data.frame(SYNCHRONICITY.LIST.COLUMNS, SYNCHRONICITY.LIST.DATA.TYPES))
}


## * Custom timestamps for splitting

#' Read custom event timestamps from a file in \code{.list} format.
#'
#' @param data.path the path of the directory containing the file
#' @param file.name the name of the file
#'
#' @return the read timestamps
read.custom.event.timestamps = function(data.path, file.name) {
    logging::logdebug("read.custom.event.timestamps: starting.")

    file = file.path(data.path, file.name)

    ## read data.frame from disk (as expected from save.list.to.file) [can be empty]
    custom.event.timestamps.table = try(read.table(file, header = FALSE, sep = ";", strip.white = TRUE,
                                        encoding = "UTF-8"), silent = TRUE)

    ## handle the case that the list of commits is empty
    if (inherits(custom.event.timestamps.table, "try-error") || nrow(custom.event.timestamps.table) < 1) {
        logging::logwarn("There are no custom timestamps available at the given path.")
        logging::logwarn("Datapath: %s", data.path)

        ## return an empty list
        return(list())
    }
    timestamps = as.list(custom.event.timestamps.table[[2]])
    names(timestamps) = custom.event.timestamps.table[[1]]

    ## convert all timestamps to POSIXct format
    posix.timestamps = get.date.from.string(timestamps)

    ## if a timestamp is malformatted get.date.from.string returns a NA
    if (any(is.na(posix.timestamps))) {
        error.message = sprintf("Input timestamps are not in POSIXct format (YYYY-mm-DD HH:MM:SS).")
        logging::logerror(error.message)
        stop(error.message)
    }

    ## Sort the timestamps
    if (length(timestamps) != 0) {
        timestamps = timestamps[order(unlist(posix.timestamps))]
    }

    logging::logdebug("read.custom.event.timestamps: finished.")
    return (timestamps)
}

## Helper functions --------------------------------------------------------

## declare a global format for the commit.id column in several data frames
COMMIT.ID.FORMAT = "<commit-%s>"

#' Format a vector of commit ids into a global format
#'
#' @param commit.ids a vector containing all the commit ids to be formatted
#'
#' @return a vector with the formatted commit ids
format.commit.ids = function(commit.ids) {
    return (sprintf(COMMIT.ID.FORMAT, commit.ids))
}


