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
## Copyright 2017-2019 by Thomas Bock <bockthom@fim.uni-passau.de>
## Copyright 2020 by Thomas Bock <bockthom@cs-uni-saarland.de>
## Copyright 2017 by Raphael Nömmer <noemmer@fim.uni-passau.de>
## Copyright 2017-2018 by Christian Hechtl <hechtl@fim.uni-passau.de>
## Copyright 2020 by Christian Hechtl <hechtl@cs.uni-saarland.de>
## Copyright 2017 by Felix Prasse <prassefe@fim.uni-passau.de>
## Copyright 2017 by Ferdinand Frank <frankfer@fim.uni-passau.de>
## Copyright 2018-2019 by Jakob Kronawitter <kronawij@fim.uni-passau.de>
## Copyright 2019-2020 by Anselm Fehnker <anselm@muenster.de>
## Copyright 2020-2021 by Niklas Schneider <s8nlschn@stud.uni-saarland.de>
## Copyright 2021 by Johannes Hostert <s8johost@stud.uni-saarland.de>
## All Rights Reserved.


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Libraries ---------------------------------------------------------------

requireNamespace("R6") # for R6 classes
requireNamespace("logging") # for logging
requireNamespace("parallel") # for parallel computation


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Constants ---------------------------------------------------------------

## untracked file
UNTRACKED.FILE = "<untracked.file>"

## the empty string which resides in the artifact column when artifact == feature or artifact == function
## in the 'ProjectConf'
UNTRACKED.FILE.EMPTY.ARTIFACT = ""
UNTRACKED.FILE.EMPTY.ARTIFACT.TYPE = ""

## base artifacts (which one actually applies, depends on the artifact parameter in the 'ProjectConf')
BASE.ARTIFACTS = c(
    "Base_Feature", ## when artifact == feature
    "File_Level",   ## when artifact == function
    UNTRACKED.FILE  ## when artifact == file
)

## mapping of data source to artifact column (for commits: filter artifacts based on the configuration options
## 'commits.filter.base.artifact' and 'commits.filter.untracked.files' of the corresponding 'ProjectConf' object)
DATASOURCE.TO.ARTIFACT.FUNCTION = list(
    "commits" = "get.commits.filtered",
    "mails"   = "get.mails",
    "issues"  = "get.issues.filtered"
)

## mapping of data source to artifact column
DATASOURCE.TO.ARTIFACT.COLUMN = list(
    "commits" = "artifact",
    "mails"   = "thread",
    "issues"  = "issue.id"
)

## the maximum time difference between subsequent mails of a patchstack
PATCHSTACK.MAIL.DECAY.THRESHOLD = "30 seconds"


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## ProjectData -------------------------------------------------------------

#' The class \code{ProjectData} provides convenient data handling for a project's data
#' in a lazy fashion.
#'
#' To configure an object of this data-handling class, a configuration object of class
#' \code{ProjectConf} must be passed to the initialization function. Data paths and
#' other configuration options are read from it.
#'
#' The following main data sources are handled:
#' - commits,
#' - mails, and
#' - issues.
#'
#' Furthermore, the following additional and orthogonal data sources are handled:
#' - synchronicity data and
#' - PaStA data.
#'
#' @seealso ProjectConf
ProjectData = R6::R6Class("ProjectData",

    ## * private -----------------------------------------------------------

    private = list(

        ## * * configuration -----------------------------------------------

        project.conf = NULL, # list

        ## * * raw data ----------------------------------------------------

        ## commits and commit data
        commits.filtered = NULL, # data.frame
        commits = NULL, # data.frame
        commit.messages = NULL, # data.frame
        ## mails
        mails = NULL, # data.frame
        mails.patchstacks = NULL, # list
        ## issues
        issues = NULL, #data.frame
        issues.filtered = NULL, #data.frame
        ## authors
        authors = NULL, # data.frame
        ## additional data sources
        synchronicity = NULL, # data.frame
        pasta = NULL, # data.frame
        pasta.mails = NULL, # data.frame
        pasta.commits = NULL, # data.frame
        ## timestamps of mail, issue and commit data
        data.timestamps = NULL, #data.frame

        ## * * commit filtering --------------------------------------------

        #' Filter commits by potentially removing commits to untracked files or to the base artifact (see parameters).
        #'
        #' @param commits the data.frame of commits on which filtering will be applied
        #' @param remove.untracked.files flag whether untracked files are kept or removed
        #' @param remove.base.artifact flag whether the base artifact is kept or removed
        #'
        #' @return the commits after all filters have been applied
        filter.commits = function(commits, remove.untracked.files, remove.base.artifact) {
            logging::logdebug("filter.commits: starting.")

            ## filter out the untracked files
            if (remove.untracked.files) {
                commits = subset(commits, file != UNTRACKED.FILE)
            }

            ## filter out the base artifacts (i.e., Base_Feature, File_Level)
            if (remove.base.artifact) {
                commits = subset(commits, !(artifact %in% BASE.ARTIFACTS))
            }

            logging::logdebug("filter.commits: finished.")
            return(commits)
        },

        ## * * mail filtering ----------------------------------------------

        #' Filter patchstack mails from the mails that are currently cached in the field \code{mails} and return them.
        #' Store detected patchstacks in the field \code{patchstack.mails}. They are used later in the
        #' function \code{filter.pasta.data} to also accommodate for the deleted mails in the PaStA data.
        #'
        #' In a thread, a patchstack spans the first sequence of mails where each mail has been authored by the thread
        #' creator and has been sent within a short time window (see \code{PATCHSTACK.MAIL.DECAY.THRESHOLD}) after the
        #' preceding mail.
        #' The mails spanned by a patchstack are called 'patchstack mails'.
        #'
        #' For each patchstack, all patchstack mails but the first one are filtered.
        #'
        #' @return the mail data after filtering patchstack mails
        filter.patchstack.mails = function() {
            logging::logdebug("filter.patchstack.mails: starting.")

            ## return immediately if no mails are available
            if (nrow(private$mails) == 0) {
                private$mails.patchstacks = NULL
                return(private$mails)
            }

            ## retrieve mails grouped by thread IDs
            thread.data = self$group.authors.by.data.column("mails", "thread")

            ## extract the patchstack mails and the filtered mails for each thread
            result = parallel::mclapply(thread.data, function(thread) {

                ## ensure that all mails within the thread are ordered correctly
                thread = thread[order(thread["date"]), ]

                running = TRUE
                i = 1

                ## find the largest index 'i' for which holds that each mail up to index 'i' has been authored by the
                ## thread creator and that all mails up to index 'i' have been received within a succesive time window
                ## of 'PATCHSTACK.MAIL.DECAY.THRESHOLD'
                while (i < nrow(thread) && running) {
                    if (thread[1, "author.name"] == thread[i + 1, "author.name"] &&
                        thread[i + 1, "date"] - thread[i, "date"] <=
                        lubridate::as.duration(PATCHSTACK.MAIL.DECAY.THRESHOLD)) {
                        i = i + 1
                    } else {
                        running = FALSE
                    }
                }

                ## return the mails of the thread with all patchstack mails but the first one being removed
                return (list(keep = thread[setdiff(seq_len(nrow(thread)), seq_len(i)[-1]), ],
                             patchstack = thread[seq_len(i), ]))
            })

            ## override thread data with filtered thread data
            thread.data = lapply(result, function(x) x[["keep"]])

            ## flatten the list of mail-dataframes (i.e. thread.data) to a single mail-dataframe
            mails = plyr::rbind.fill(thread.data)

            ## Retrieve patchstacks from the result above which are used to manipulate the PaStA data. This needs to be
            ## done because the PaStA data relates to some of the filtered mails and must be adjusted accordingly.
            patchstacks = lapply(result, function(x) x[["patchstack"]])

            ## only patchstacks that contain at least two mails are considered patchstacks
            patchstacks = patchstacks[lapply(patchstacks, nrow) > 1]

            ## store patchstack information
            private$mails.patchstacks = patchstacks

            logging::logdebug("filter.patchstack.mails: finished.")
            return(mails)
        },

        ## * * commit message data ------------------------------------------

        #' Add the columns \code{title} and \code{message} to commits using the selected
        #' configuration option of \code{commit.messages} and the results of the function \code{get.commit.messages}.
        update.commit.message.data = function() {
            logging::loginfo("Merging commit messages into commit data.")

            if (!is.null(private$commits)) {
                ## get commit messages
                commit.messages = private$commit.messages

                ## now there are only three columns left: commit.id, title, message
                ## check whether to include only title or also the messages
                if (private$project.conf$get.value("commit.messages") == "title") {
                    commit.messages = commit.messages[ , colnames(commit.messages) != "message"]
                }

                ## get a vector with the column names in the right order
                col.names = unique(c(colnames(private$commits), colnames(commit.messages)))
                ## merge them into the commit data
                private$commits = merge(private$commits, commit.messages,
                                        by = c("commit.id", "hash"), all.x = TRUE, sort = FALSE)
                ## adjust the column order
                private$commits = private$commits[col.names]
            }

            logging::logwarn("There might be commit message data that does not appear in the commit data.
                              To clean this up you can call the function 'cleanup.commit.message.data()'.")
        },

        ## * * PaStA data --------------------------------------------------

        #' Use the information about the deleted patchstack mails that are stored in the field \code{patchstack.mails}
        #' to also filter out PaStA information that relates to the deleted mails.
        #'
        #' The PaStA information is not discarded completely however but instead is gathered for each patchstack and is
        #' assigned to the first mail in each patchstack because this very first mail has not been filtered and
        #' represents the patchstack.
        #'
        #' @return the filtered PaStA data
        filter.pasta.data = function() {
            logging::logdebug("filter.pasta.data: starting.")

            new.pasta = parallel::mclapply(private$mails.patchstacks, function(patchstack) {

                ## get all PaStA data that relates to the current patchstack (do not drop data.frame structure!)
                pasta.tmp = private$pasta[private$pasta[["message.id"]] %in% patchstack[["message.id"]], , drop = FALSE]

                ## override all old message IDs with the message ID of the first mail in the patchstack since it
                ## is the only one that is kept (if any data is available in 'pasta.tmp')
                if (nrow(pasta.tmp) > 0) {
                    pasta.tmp["message.id"] = patchstack[1, "message.id"]
                }

                return(pasta.tmp)
            })
            ## combine new re-written PaStA data
            new.pasta = plyr::rbind.fill(new.pasta)

            ## remove potential duplicates
            new.pasta = unique(new.pasta)

            ## remove old items from PaStA data
            ## 1) flatten the list of mail-dataframes (i.e. patchstacks) to a single mail-dataframe
            patchstack.mails = plyr::rbind.fill(private$mails.patchstacks)
            ## 2) delete any PaStA information that relate to message IDs of mails that will be discarded
            pasta = private$pasta[!(private$pasta[["message.id"]] %in% patchstack.mails[["message.id"]]), ]

            ## append the new pasta data to the old pasta data
            pasta = plyr::rbind.fill(pasta, new.pasta)

            ## reestablish ordering using the 'revision.set.id' column of the PaStA data
            pasta = pasta[order(pasta[["revision.set.id"]]), ]

            logging::logdebug("filter.pasta.data: finished.")
            return(pasta)
        },

        #' Aggregate PaStA data for convenient merging to main data sources.
        #'
        #' In detail, the given PaStA data is independently aggregated by both the
        #' columns \code{commit.hash} and \code{message.id}. The resulting data.frames
        #' are stored in the fields \code{pasta.commits} and \code{pasta.mails},
        #' respectively.
        #'
        #' **Note**: The column \code{commit.hash} gets renamed to \code{hash} to match
        #' the corresponding column in the commit data (see \code{read.commits}).
        aggregate.pasta.data = function() {
            logging::logdebug("aggregate.pasta.data: starting.")

            ## check for data first
            if (nrow(private$pasta) == 0) {
                ## take (empty) input data and no rows from it
                private$pasta.mails = create.empty.pasta.list()
                private$pasta.commits = create.empty.pasta.list()
            } else {
                ## compute aggregated data.frames for easier merging
                ## 1) define group function (determines result in aggregated data.frame cells)
                group.fun = unname # unique
                ## 2) aggregate by message ID
                group.col = "message.id"
                private$pasta.mails = aggregate(
                    as.formula(sprintf(". ~ %s", group.col)), private$pasta,
                    group.fun, na.action = na.pass
                )
                ## 3) aggregate by commit hash
                group.col = "commit.hash"
                private$pasta.commits = aggregate(
                    as.formula(sprintf(". ~ %s", group.col)), private$pasta,
                    group.fun, na.action = na.pass
                )
            }

            ## set column names consistent to main data
            colnames(private$pasta.mails) = c("message.id", "pasta", "revision.set.id")
            colnames(private$pasta.commits) = c("hash", "pasta", "revision.set.id")

            logging::logdebug("aggregate.pasta.data: finished.")
        },

        #' Update the PaStA-related columns \code{pasta} and \code{revision.set.id} that are appended to \code{commits}
        #' using the currently available PaStA data from the field \code{pasta.commits}.
        update.pasta.commit.data = function() {
            logging::logdebug("update.pasta.commit.data: starting.")

            ## return immediately if no commits available
            if (!is.null(private$commits)) {

                ## remove previous PaStA data
                private$commits["pasta"] = NULL
                private$commits["revision.set.id"] = NULL

                ## merge PaStA data
                private$commits = merge(private$commits, private$pasta.commits,
                                    by = "hash", all.x = TRUE, sort = FALSE)

                ## sort by date again because 'merge' disturbs the order
                private$commits = private$commits[order(private$commits[["date"]], decreasing = FALSE), ]

                ## remove duplicated revision set ids
                private$commits[["revision.set.id"]] = sapply(private$commits[["revision.set.id"]], function(rev.id) {
                    return(unique(rev.id))
                })
            }

            logging::logdebug("update.pasta.commit.data: finished.")
        },

        #' Update the PaStA-related columns \code{pasta} and \code{revision.set.id} that are appended to \code{mails}
        #' using the currently available PaStA data from the field \code{pasta.mails}.
        update.pasta.mail.data = function() {
            logging::logdebug("update.pasta.mail.data: starting.")

            ## return immediately if no mails available
            if (!is.null(private$mails)) {

                ## remove previous PaStA data
                private$mails["pasta"] = NULL
                private$mails["revision.set.id"] = NULL

                ## merge PaStA data
                private$mails = merge(private$mails, private$pasta.mails,
                                      by = "message.id", all.x = TRUE, sort = FALSE)

                ## sort by date again because 'merge' disturbs the order
                private$mails = private$mails[order(private$mails[["date"]], decreasing = FALSE), ]

                ## remove duplicated revision set ids
                private$mails[["revision.set.id"]] = sapply(private$mails[["revision.set.id"]], function(rev.id) {
                    return(unique(rev.id))
                })
            }

            logging::logdebug("update.pasta.mail.data: finished.")
        },

        #' Recompute the values of the cached fields \code{pasta.mails} and \code{pasta.commits} using the currrently
        #' available PaStA information of the field \code{pasta} and also assign/update this PaStA information to
        #' \code{mails} and \code{commits}.
        #'
        #' This method should be called whenever the field \code{pasta} is changed.
        update.pasta.data = function() {
            logging::logdebug("update.pasta.data: starting.")

            ## filter patchstack mails from PaStA data if configured
            if (private$project.conf$get.value("mails.filter.patchstack.mails")) {
                private$pasta = private$filter.pasta.data()
            }

            ## aggregate by message IDs and commit hashes
            private$aggregate.pasta.data()

            ## update mail data by attaching PaStA data
            if (!is.null(private$mails)) {
                private$update.pasta.mail.data()
            }

            ## update commit data by attaching PaStA data
            if (!is.null(private$commits)) {
                private$update.pasta.commit.data()
            }

            logging::logwarn("There might be PaStA data that does not appear in the mail or commit data.
                              To clean this up you can call the function 'cleanup.pasta.data()'.")
            logging::logdebug("update.pasta.data: finished.")
        },

        ## * * synchronicity data ------------------------------------------

        #' Update the column \code{synchronicity} that is appended to commits using the currently available
        #' synchronicity data from the field \code{synchronicity}.
        #'
        #' This method should be called whenever the field \code{synchronicity} is changed.
        update.synchronicity.data = function() {
            logging::logdebug("update.synchronicity.data: starting.")

            ## update commit data by attaching synchronicity data
            if (!is.null(private$commits)) {
                ## remove previous synchronicity data
                private$commits["synchronicity"] = NULL

                ## merge synchronicity data
                private$commits = merge(private$commits, private$synchronicity,
                                    by = "hash", all.x = TRUE, sort = FALSE)

                ## sort by date again because 'merge' disturbs the order
                private$commits = private$commits[order(private$commits[["date"]], decreasing = FALSE), ]

            }

            logging::logwarn("There might be synchronicity data that does not appear in the commit data.
                              To clean this up you can call the function 'cleanup.synchronicity.data()'.")
            logging::logdebug("update.synchronicity.data: finished.")
        },

        ## * * timestamps --------------------------------------------------

        #' Call the getters of the specified data sources in order to
        #' initialize the sources and extract the timestamps.
        #'
        #' @param data.sources the data sources to be prepared
        prepare.timestamps = function(data.sources) {
            for (source in data.sources) {
                self[[ paste0("get.", source) ]]()
            }
        },

        #' Extract the earliest and the latest date from the specified data source
        #' and store it to the timestamps data.frame.
        #'
        #' @param source the specified data source
        extract.timestamps = function(source) {
            ## initialize data structure for timestamp
            if (is.null(private$data.timestamps)) {
                private$data.timestamps = data.frame(start = numeric(0), end = numeric(0))
            }

            ## collect minimum and maximum date for data source
            ## 1) if we have data available
            if (nrow(private[[source]]) > 0) {
                source.date.min = min(private[[source]][, "date"])
                source.date.max = max(private[[source]][, "date"])
            }
            ## NAs otherwise
            else {
                source.date.min = NA
                source.date.max = NA
            }

            ## remove old line if existing
            private$data.timestamps = subset(
                private$data.timestamps,
                !(rownames(private$data.timestamps) == source)
            )

            ## store the data in the timestamp data set
            private$data.timestamps = rbind(
                private$data.timestamps,
                data.frame(
                    start = source.date.min,
                    end = source.date.max,
                    row.names = source
                )
            )
        }
    ),

    ## * public ------------------------------------------------------------

    public = list(
        #' The constructor of the class.
        #'
        #' @param project.conf the given \code{ProjectConf} object for this instance of the class
        initialize = function(project.conf) {

            ## check arguments
            private$project.conf = verify.argument.for.parameter(project.conf, "ProjectConf", "ProjectData$new")

            ## if we have a direct subclass of ProjectData here,
            ## log this accordingly
            if (class(self)[1] == "ProjectData") {
                logging::loginfo("Initialized data object %s", self$get.class.name())
            }
        },

        ## * * printing ----------------------------------------------------

        #' The toString method of the class.
        get.class.name = function() {
            return(
                sprintf("ProjectData<%s>", private$project.conf$get.value("repo"))
            )
        },

        ## * * resetting environment ---------------------------------------

        #' Reset the current environment in order to rebuild it.
        #' Has to be called whenever the project configuration or data gets
        #' changed.
        reset.environment = function() {
            private$commits.filtered = NULL
            private$commits = NULL
            private$commit.messages = NULL
            private$mails = NULL
            private$issues = NULL
            private$issues.filtered = NULL
            private$authors = NULL
            private$synchronicity = NULL
            private$pasta = NULL
            private$pasta.mails = NULL
            private$pasta.commits = NULL
            private$data.timestamps = NULL
        },

        ## * * configuration -----------------------------------------------

        #' Get the current project configuration.
        #'
        #' @return the \code{ProjectConf} object of the current instance of the class
        get.project.conf = function() {
            return(private$project.conf)
        },

        #' Set the current project configuration to the given one.
        #'
        #' @param project.conf the new project configuration.
        #' @param reset.environment parameter to determine whether the environment
        #'                          has to be reset or not
        set.project.conf = function(project.conf, reset.environment = FALSE) {
            private$project.conf = verify.argument.for.parameter(project.conf, "ProjectConf",
                                                                 "ProjectData$set.project.conf")

            if (reset.environment) {
                self$reset.environment()
            }
        },

        #' Get  a value of the project configuration
        #'
        #' @return the value of the given entry name
        get.project.conf.entry = function(entry) {
            return(private$project.conf$get.value(entry))
        },

        #' Set  a value of the project configuration and reset the environment
        set.project.conf.entry = function(entry, value) {
            private$project.conf$update.value(entry, value)
            self$reset.environment()
        },

        #' Update the project configuration based on the given list
        #' of values and reset the environment afterwards
        #'
        #' @param updated.values the new values for the project configuration
        update.project.conf = function(updated.values = list()) {
            private$project.conf$update.values(updated.values = updated.values)
            self$reset.environment()
        },

        ## * * backups -----------------------------------------------------

        #' Backup the current environment to a file on the disk.
        #'
        #' @param file the path to the backup file
        save.to.disk = function(file) {
            save(self, file = file)
        },

        ## * * path retrieval ----------------------------------------------

        #' Get the absolute path to the project's result folder.
        #'
        #' @return the path to the result folder
        get.data.path = function() {
            data.path = private$project.conf$get.value("datapath")
            return(data.path)
        },

        #' Get the absolute path to the range's result folder for synchronicity data.
        #'
        #' @return the path to the synchronicity files
        get.data.path.synchronicity = function() {
            data.path = private$project.conf$get.value("datapath.synchronicity")
            return(data.path)
        },

        #' Get the absolute path to the result folder for PaStA data.
        #'
        #' @return the path to the PaStA data
        get.data.path.pasta = function() {
            data.path = private$project.conf$get.value("datapath.pasta")
            return(data.path)
        },

        #' Get the absolute path to the result folder for issue data.
        #'
        #' @return the path to the issue data
        get.data.path.issues = function() {
            data.path = private$project.conf$get.value("datapath.issues")
            return(data.path)
        },

        ## * * raw data ----------------------------------------------------

        #' Return the commits retrieved by the method \code{get.commits} by removing untracked files and removing the
        #' base artifact (if configured in the \code{project.conf}, see parameters \code{commits.filter.untracked.files}
        #' and \code{commits.filter.base.artifact}).
        #'
        #' This method caches the filtered commits to the field \code{commits.filtered}.
        #'
        #' @return the commits retrieved by the method \code{get.commits} after all filters have been applied
        #'
        #' @seealso get.commits.filtered.uncached
        get.commits.filtered = function() {
            if (is.null(private$commits.filtered)) {
                private$commits.filtered = private$filter.commits(
                    self$get.commits(),
                    private$project.conf$get.value("commits.filter.untracked.files"),
                    private$project.conf$get.value("commits.filter.base.artifact")
                )
            }
            return(private$commits.filtered)
        },

        #' Return the commits retrieved by the method \code{get.commits} by removing untracked files and removing the
        #' base artifact (see parameters).
        #'
        #' This method does not use caching. If you want to use caching, please use the method
        #' \code{get.commits.filtered} instead.
        #'
        #' @param remove.untracked.files flag whether untracked files are kept or removed
        #' @param remove.base.artifact flag whether the base artifact is kept or removed
        #'
        #' @return the commits retrieved by the method \code{get.commits} after all filters have been applied
        #'
        #' @seealso get.commits.filtered
        get.commits.filtered.uncached = function(remove.untracked.files, remove.base.artifact) {
            return (private$filter.commits(self$get.commits(), remove.untracked.files, remove.base.artifact))
        },

        #' Get the list of commits which have the artifact kind configured in the \code{project.conf}.
        #' If the list of commits is not cached in the field \code{commits}, call the read method first.
        #' If configured in the \code{project.conf}, add PaStA and synchronicity data.
        #'
        #' @return the list of commits
        get.commits = function() {
            logging::loginfo("Getting commit data.")

            ## if commits are not read already, do this
            if (is.null(private$commits)) {
                commit.data = read.commits(self$get.data.path(), private$project.conf$get.value("artifact"))

                ## only consider commits that have the artifact type configured in the 'project.conf' or commits to
                ## untracked files
                commit.data = subset(commit.data, artifact.type %in%
                                         c(private$project.conf$get.value("artifact.codeface"),
                                           UNTRACKED.FILE.EMPTY.ARTIFACT.TYPE))

                ## Add PaStA and synchronicity data (if configured in the 'project.conf') and save the commit data to
                ## the field 'commits' afterwards
                self$set.commits(commit.data)
            }
            private$extract.timestamps(source = "commits")

            return(private$commits)
        },

        #' Set the commit list of the project to a new one.
        #' Add PaStA and synchronicity data if configured in the \code{project.conf}
        #' as well as commit message data.
        #'
        #' @param commit.data the new list of commits
        set.commits = function(commit.data) {
            logging::loginfo("Setting commit data.")

            if (is.null(commit.data)) {
                commit.data = create.empty.commits.list()
            }

            ## store commit data
            private$commits = commit.data

            ## add commit message data if wanted
            if (private$project.conf$get.value("commit.messages") != "none") {
                if (is.null(private$commit.messages)) {
                    ## get data that has been cached before
                    self$get.commit.messages()
                } else {
                    ## update the commit message data
                    private$update.commit.message.data()
                }

            }

            ## add synchronicity data if wanted
            if (private$project.conf$get.value("synchronicity")) {
                if (is.null(private$synchronicity)) {
                    ## get data (no assignment because we just want to trigger anything synchronicity-related)
                    self$get.synchronicity()
                } else {
                    ## update all synchronicity-related data
                    private$update.synchronicity.data()
                }
            }

            ## add PaStA data if wanted
            if (private$project.conf$get.value("pasta")) {
                if (is.null(private$pasta)) {
                    ## get data (no assignment because we just want to trigger anything PaStA-related)
                    self$get.pasta()
                } else {
                    ## update all PaStA-related data
                    private$update.pasta.data()
                }
            }

            ## sort by date
            private$commits = private$commits[order(private$commits[["date"]], decreasing = FALSE), ]

            ## remove cached data for filtered commits as these need to be re-computed after
            ## changing the data
            private$commits.filtered = NULL
        },

        #' Get the list of commits which have the artifact kind configured in the \code{project.conf}.
        #' If the list of commits is not cached in the field \code{commit.messages}, call the read method first.
        #'
        #' @return the list of commit messages
        get.commit.messages = function() {
            logging::loginfo("Getting commit messages.´")

            if (private$project.conf$get.value("commit.messages") == "title" |
                private$project.conf$get.value("commit.messages") == "message") {
                ## if commit messages are not read already, do this
                if (is.null(private$commit.messages)) {
                    commit.message.data = read.commit.messages(self$get.data.path())

                    ## cache the result
                    private$commit.messages = commit.message.data

                    private$update.commit.message.data()
                }
            } else {
                logging::logwarn("You have set the ProjectConf parameter 'commit.messages' to 'none'! Ignoring...")
                ## mark commit messages data as empty
                self$set.commit.messages(NULL)
            }

            return(private$commit.messages)
        },

        #' Set the commit message data to the given new data and, if configured in the field \code{project.conf},
        #' also update it for the commit data.
        #'
        #' @param data the new commit message data
        set.commit.messages = function(data) {
            logging::loginfo("Setting commit messages data.")

            if (is.null(data)) {
                data = create.empty.commit.message.list()
            }

            ## set the actual data
            private$commit.messages = data

            ## add commit message data to the commit data if configured
            if (private$project.conf$get.value("commit.messages") == "title" |
                private$project.conf$get.value("commit.messages") == "message") {
                update.commit.message.data()
            }
        },

        #' Remove lines in the commit message data that contain commit hashes
        #' that don't appear in the commit data.
        cleanup.commit.message.data = function() {
            logging::loginfo("Cleaning up commit message data")

            ## remove commit hashes that don't appear in the commit data
            if (!is.null(private$commits)) {
                commit.message.hashes = private$commit.messages[["hash"]]
                commit.message.hashes.contained = private$commit.messages[["hash"]] %in% private$commits[["hash"]]
                commit.hashes.to.eliminate = commit.message.hashes[!commit.message.hashes.contained]
                commit.hashes.to.eliminate = commit.hashes.to.eliminate[!is.na(commit.hashes.to.eliminate)]
                rows.to.remove = private$commit.messages[["hash"]] %in% commit.hashes.to.eliminate
                private$commit.messages = private$commit.messages[!rows.to.remove, ]
            }
        },

        #' Get the synchronicity data. If it is not already stored in the ProjectData, this function triggers a read in
        #' from disk.
        #'
        #' @return the synchronicity data
        get.synchronicity = function() {
            logging::loginfo("Getting synchronicity data.")

            ## if synchronicity data are to be read, do this
            if (private$project.conf$get.value("synchronicity")) {
                ##  if data are not read already, read them
                if (is.null(private$synchronicity)) {
                    private$synchronicity = read.synchronicity(
                        self$get.data.path.synchronicity(),
                        private$project.conf$get.value("artifact"),
                        private$project.conf$get.value("synchronicity.time.window")
                    )

                    ## no read of commit data needed here!

                    ## update all synchronicity-related data
                    private$update.synchronicity.data()
                }
            } else {
                logging::logwarn("You have not set the ProjectConf parameter 'synchronicity' to 'TRUE'! Ignoring...")
                ## mark synchronicity data as empty
                self$set.synchronicity(NULL)
            }

            return(private$synchronicity)
        },

        #' Set the synchronicity data to the given new data and,
        #' if configured in the field \code{project.conf},
        #' also update it for the commit data.
        #'
        #' @param data the new synchronicity data
        set.synchronicity = function(data) {
            logging::loginfo("Setting synchronicity data.")

            if (is.null(data)) {
                data = create.empty.synchronicity.list()
            }

            ## set the actual data
            private$synchronicity = data

            ## add synchronicity data to the commit data if configured
            if (private$project.conf$get.value("synchronicity")) {

                ## no read of commit data needed here!

                ## update all synchronicity-related data
                private$update.synchronicity.data()
            }
        },

        #' Remove lines in the synchronicity data that contain commit hashes
        #' that don't appear in the commit data.
        cleanup.synchronicity.data = function() {
            logging::loginfo("Cleaning up synchronicity data")

            ## remove commit hashes that don't appear in the commit data
            if (!is.null(private$commits)) {
                synchronicity.hashes = private$synchronicity[["hash"]]
                synchronicity.hashes.contained = private$synchronicity[["hash"]] %in% private$commits[["hash"]]
                commit.hashes.to.eliminate = synchronicity.hashes[!synchronicity.hashes.contained]
                commit.hashes.to.eliminate = commit.hashes.to.eliminate[!is.na(commit.hashes.to.eliminate)]
                rows.to.remove = private$synchronicity[["hash"]] %in% commit.hashes.to.eliminate
                private$synchronicity = private$synchronicity[!rows.to.remove, ]
            }
        },

        #' Get the PaStA data. If it is not already stored in the ProjectData, this function triggers a read in
        #' from disk.
        #'
        #' @return the PaStA data
        get.pasta = function() {
            logging::loginfo("Getting PaStA data.")

            ## if PaStA data are to be read, do this
            if (private$project.conf$get.value("pasta")) {
                ## if data are not read already, read them
                if (is.null(private$pasta)) {
                    ## read PaStA data from disk
                    private$pasta = read.pasta(self$get.data.path.pasta())

                    ## read mail data if filtering patchstack mails
                    if (is.null(private$mails)
                        && private$project.conf$get.value("mails.filter.patchstack.mails")) {
                        ## just triggering read-in, no storage
                        self$get.mails()
                    } else {
                        ## update all PaStA-related data
                        private$update.pasta.data()
                    }
                }
            } else {
                logging::logwarn("You have not set the ProjectConf parameter 'pasta' to 'TRUE'! Ignoring...")
                ## mark PaStA data as empty
                self$set.pasta(NULL)
            }

            return(private$pasta)
        },

        #' Set the PaStA data to the given new data and,
        #' if configured in the field \code{project.conf},
        #' also update it for the mail and commit data.
        #'
        #' @param data the new PaStA data
        set.pasta = function(data) {
            logging::loginfo("Setting PaStA data.")

            if (is.null(data)) {
                data = create.empty.pasta.list()
            }

            ## set the actual data
            private$pasta = data

            ## add PaStA data to commit and mail data if configured
            if (private$project.conf$get.value("pasta")) {

                ## read mail data if filtering patchstack mails
                if (is.null(private$mails) &&
                    private$project.conf$get.value("mails.filter.patchstack.mails")) {
                    ## just triggering read-in, no storage
                    self$get.mails()

                } else {
                    ## update all PaStA-related data
                    private$update.pasta.data()

                }
            }
        },

        #' Remove lines in the PaStA data that contain message ids or commit hashes
        #' that don't appear in the commit or mail data.
        cleanup.pasta.data = function() {
            logging::loginfo("Cleaning up PaStA data")

            ## remove message ids that don't appear in the mail data
            if (!is.null(private$mails)) {
                rev.id.contained = private$pasta[["revision.set.id"]] %in% private$mails[["revision.set.id"]]
                private$pasta = private$pasta[rev.id.contained, ]
            }

            ## remove commit hashes that don't appear in the commit data
            if (!is.null(private$commits)) {
                pasta.commit.hashes = unlist(private$pasta[["commit.hash"]])
                commit.hashes.contained = unlist(private$pasta[["commit.hash"]]) %in% private$commits[["hash"]]
                commit.hashes.to.eliminate = pasta.commit.hashes[!commit.hashes.contained]
                commit.hashes.to.eliminate = commit.hashes.to.eliminate[!is.na(commit.hashes.to.eliminate)]
                rows.to.remove = unlist(private$pasta[["commit.hash"]]) %in% commit.hashes.to.eliminate
                private$pasta = private$pasta[!rows.to.remove, ]
            }

            ## update pasta data again
            private$update.pasta.data()
        },

        #' Get the mail data.
        #' If it does not already exist call the read method.
        #' Call the setter function to set the data and add PaStA
        #' data if configured in the field \code{project.conf}.
        #'
        #' @return the mail data
        get.mails = function() {
            logging::loginfo("Getting e-mail data.")

            ## if mails are not read already, do this
            if (is.null(private$mails)) {
                mails.read = read.mails(self$get.data.path())

                self$set.mails(mails.read)
            }
            private$extract.timestamps(source = "mails")

            return(private$mails)
        },

        #' Set the mail data to the given new data and add PaStA data
        #' if configured in the field \code{project.conf}.
        #'
        #' @param mail.data the new mail data
        set.mails = function(mail.data) {
            logging::loginfo("Setting e-mail data.")

            if (is.null(mail.data)) {
                mail.data = create.empty.mails.list()
            }

            ## store mail data
            private$mails = mail.data

            ## filter patchstack mails and store again
            if (private$project.conf$get.value("mails.filter.patchstack.mails")) {
                private$mails = private$filter.patchstack.mails()
            }

            ## add PaStA data if wanted
            if (private$project.conf$get.value("pasta")) {
                if (is.null(private$pasta)) {
                    ## get data (no assignment because we just want to trigger anything PaStA-related)
                    self$get.pasta()
                } else {
                    ## update all PaStA-related data
                    private$update.pasta.data()
                }
            }

            ## sort by date
            private$mails = private$mails[order(private$mails[["date"]], decreasing = FALSE), ]
        },

        #' Get the author data.
        #' If it does not already exist call the read method.
        #'
        #' @return the author data
        get.authors = function() {
            logging::loginfo("Getting author data.")

            ## if authors are not read already, do this
            if (is.null(private$authors)) {
                private$authors = read.authors(self$get.data.path())
            }

            return(private$authors)
        },

        #' Set the atuhor data to the given new data.
        #'
        #' @param data the new author data
        set.authors = function(data) {
            logging::loginfo("Setting author data.")
            private$authors = data
        },

        #' Get the issue data, filtered.
        #' If it does not already exist call the read method.
        #'
        #' @return the issue data
        get.issues.filtered = function() {
            logging::loginfo("Getting issue data")

            ## if issues have not been read yet do this
            if (is.null(private$issues.filtered)) {
                private$issues.filtered = self$get.issues()
                if (private$project.conf$get.value("issues.only.comments")) {
                    private$issues.filtered = private$issues.filtered[private$issues[["event.name"]] == "commented", ]
                }
            }
            return(private$issues.filtered)
        },

        #' Get the issue data, unfiltered.
        #' Unfiltered means that we ignore the "issues.only.comments" config and always return the full data
        #' If it does not already exist call the read method.
        #'
        #' @return the issue data
        get.issues = function() {
            logging::loginfo("Getting issue data")

            ## if issues have not been read yet do this
            if (is.null(private$issues)) {
                private$issues = read.issues(self$get.data.path.issues(), private$project.conf$get.value("issues.from.source"))
            }
            private$extract.timestamps(source = "issues")
            return(private$issues)
        },

        #' Set the issue data to the given new data.
        #'
        #' @param data the new issue data
        set.issues = function(data) {
            logging::loginfo("Setting issue data.")

            if (is.null(data)) {
                data = create.empty.issues.list()
            }

            private$issues = data
            private$issues.filtered = NULL
        },

        #' Get the list of artifacts from the given \code{data.source} of the project.
        #'
        #' @param data.sources The specified data source. One of \code{"commits"},
        #'                     \code{"mails"}, and \code{"issues"} or multiple of them. [default: "commits"]
        #'
        #' @return the character vector of unique artifacts (can be empty)
        get.artifacts = function(data.sources = c("commits", "mails", "issues")) {
            logging::loginfo("Getting artifact data.")

            ## get the right value when nothing is passed for 'data.source'
            ## here we need to do a trick, because 'match.arg' returns the whole choice vector instead of the first
            ## element if nothing is passed but 'several.ok' is set to 'TRUE'. With this solution we get the following
            ## behavior: if 'data.source' is missing, 'several.ok' will be 'FALSE' making the function return
            ## the first element of the choice vector. If an argument is present, 'several.ok' will be 'TRUE' allowing
            ## multiple values for the argument
            data.sources = match.arg(data.sources, several.ok = !missing(data.sources))

            artifacts = lapply(data.sources, function(data.source) {
                data.source.func = DATASOURCE.TO.ARTIFACT.FUNCTION[[data.source]]
                data.source.col = DATASOURCE.TO.ARTIFACT.COLUMN[[data.source]]
                data = self[[data.source.func]]()
                return(unique(data[[data.source.col]]))
            })

            ## make it a vector
            artifacts = unlist(artifacts)

            ## empty vector if no data exist
            if (is.null(artifacts)) {
                artifacts = character(0)
            }

            return(artifacts)
        },

        #' Get single PaStA items.
        #' For a given 'message.id', the associated 'commit.hash' is returned.
        #' For a given 'commit.hash', the associated 'message.id' or IDs are returned.
        #' If there is no assigned 'commit.hash' for the given 'message.id', "" is returned.
        #'
        #' @param message.id the message ID to get the corresponding commit hash
        #' @param commit.hash the commit hash to get the corresponding message ID
        #'
        #' @return the selected PaStA data
        get.pasta.items = function(message.id = NULL, commit.hash = NULL) {
            #if neither message.id nor commit.hash are specified break the code
            if (is.null(message.id) && is.null(commit.hash)) {
                logging::logwarn("Neither message.id nor commit.hash specified.")
                return()
            }

            ## get PaStA data
            self$get.pasta()

            ## if a message.id is given just return the attached list of commit hashes
            ## else gather all message.ids which contain the given commit.hash and return them
            if (!is.null(message.id)) {
                result = private$pasta[private$pasta[["message.id"]] == message.id, "commit.hash"]
                if (!(length(result) == 0) && is.na(result)) {
                    result = ""
                }
                return(result)
            } else {
                result = private$pasta[private$pasta[["commit.hash"]] == commit.hash, "message.id"]
                result = result[!is.na(result)]
                return(result)
            }
        },

        #' Get the names of all data sources that are currently cached in the
        #' ProjectData object. The possible data sources are:
        #' 'commits', 'mails', 'issues', 'authors', synchronicity', and 'pasta'.
        #' 'data.timestamps' are tested implicitly every time as they only contain
        #' the earliest and latest date of one data source.
        #'
        #' @return a vector containing all the names
        get.cached.data.sources = function() {
            data.sources = c(
                ## main data sources
                "commits",
                "mails",
                "issues",
                ## author data
                "authors",
                ## additional data sources
                "synchronicity",
                "pasta"
            )

            result = Filter(function (ds) {
                return(!is.null(private[[ds]]))
            }, data.sources)

            return(result)
        },

        #' Extract the data classes (i.e., data columns and their classes) available in
        #' the given data source.
        #'
        #' @param data.source The specified data source. One of \code{"mails"},
        #'                    \code{"commits"}, and \code{"issues"}. [default: "commits"]
        #'
        #' @return a named list of data classes, with the corresponding data columns as names
        get.data.columns.for.data.source = function(data.source = c("commits", "mails", "issues")) {

            ## check arguments
            data.source = match.arg(arg = data.source, several.ok = FALSE)

            ## get the needed data method first
            data.fun = DATASOURCE.TO.ARTIFACT.FUNCTION[[data.source]]

            ## get the column classes with corresponding names
            columns = lapply(self[[data.fun]](), class)

            return(columns)
        },

        #' Compares two ProjectData objects by first comparing the names of the
        #' cached data sources of the two.
        #' Then, it compares the ProjectConf objects and, in the end, the cached data
        #' sources.
        #'
        #' @param other.data.object the object with which to compare
        #'
        #' @return \code{TRUE} if the objects are equal and \code{FALSE} otherwise
        equals = function(other.data.object) {

            ## check whether the given object is an instance of ProjectData
            if (!("ProjectData" %in% class(other.data.object))) {
                logging::logerror("You can only compare a ProjectData object against another one.")
                return(FALSE)
            }

            ## check whether the two objects are of the same type
            if (!identical(self$get.class.name(), other.data.object$get.class.name())) {
                logging::logerror("You can only compare two instances of the same class.")
                return(FALSE)
            }

            self.data.sources = self$get.cached.data.sources()
            other.data.sources = other.data.object$get.cached.data.sources()

            ## compare the list of cached data sources
            if (!identical(self.data.sources, other.data.sources)) {
                return(FALSE)
            }

            ## compare the two ProjectConf objects
            if (!identical(self$get.project.conf()$get.conf.as.string(),
                           other.data.object$get.project.conf()$get.conf.as.string())) {
                return(FALSE)
            }

            ## compare the cached data sources
            for (source in self.data.sources) {
                function.call = paste0("get.", source)
                if (!identical(self[[function.call]](), other.data.object[[function.call]]())) {
                    return(FALSE)
                }
            }

            return(TRUE)
        },

        ## * * data cutting ------------------------------------------------

        #' Get the timestamps (earliest and latest date of activity) of the specified
        #' data sources.
        #'
        #' If there are no data available for a data source, the result indicates NA.
        #'
        #' @param data.sources The specified data sources. One of \code{"mails"},
        #'                     \code{"commits"}, and \code{"issues"}.
        #' @param simple If TRUE, return the overall latest start and earliest end date
        #'               across all data sources in a one-row data.frame; otherwise, return
        #'               the first and last activities of all data sources individually.
        #'               Can be overridden by \code{outermost}.
        #' @param outermost Whether the very first and the very last activity across all data
        #'                  sources is to be returned in a one-row data.frame; ignored otherwise.
        #'                  This overrides any value given via \code{simple}.
        #'
        #' @return A data.frame with the timestamps of each data source as columns "start" and "end",
        #'         possibly with the data source as corresponding row name
        get.data.timestamps = function(data.sources = c("mails", "commits", "issues"), simple = FALSE,
                                       outermost = FALSE) {

            ## check arguments
            data.sources = match.arg(arg = data.sources, several.ok = TRUE)

            ## read all data sources and prepare list of timestamps
            private$prepare.timestamps(data.sources = data.sources)

            ## get the needed subset of timestamp data
            subset.timestamps = private$data.timestamps[data.sources, ]

            ## get the proper subset of timestamps for returning
            if (outermost) {
                ## get minimum start date and maximum end date across data sources
                timestamps = data.frame(
                    start = min(subset.timestamps[, "start"], na.rm = TRUE),
                    end = max(subset.timestamps[, "end"], na.rm = TRUE)
                )
            } else if (simple) {
                ## get maximum start date and minimum end date across data sources
                timestamps = data.frame(
                    start = max(subset.timestamps[, "start"], na.rm = TRUE),
                    end = min(subset.timestamps[, "end"], na.rm = TRUE)
                )
            } else {
                ## select the complete raw data
                timestamps = subset.timestamps
            }

            return(timestamps)
        },

        #' Cut the specified data sources to the same date range depending on the extracted
        #' timestamps.
        #'
        #' @param data.sources the specified data sources
        #'
        #' @return the RangeData object with cut data sources
        get.data.cut.to.same.date = function(data.sources = c("mails", "commits", "issues")) {
            ## check arguments
            data.sources = match.arg(arg = data.sources, several.ok = TRUE)

            ## get the timestamp data as vector
            timestamps.df = self$get.data.timestamps(data.sources = data.sources , simple = TRUE)
            timestamps = c(start = timestamps.df[, "start"], end = timestamps.df[, "end"] + 1)

            ## check consistency
            if (timestamps["start"] > timestamps["end"]) {
                logging::logwarn("The datasources don't overlap. The result will be empty!")
            }

            ## split data based on the timestamps and get the single result
            result = split.data.time.based(self, bins = timestamps)[[1]]

            return(result)
        },

        ## * * processed data ----------------------------------------------

        #' Group the authors of the given \code{data.source} by the given \code{group.column}.
        #' For each group, the column \code{"author.name"} is duplicated and prepended to each
        #' group's data as first column (see below for details).
        #'
        #' Example: To obtain the authors who touched the same source-code artifact,
        #' call \code{group.authors.by.data.column("commits", "artifact")}.
        #'
        #' @param data.source The specified data source. One of \code{"commits"},
        #'                    \code{"mails"}, and \code{"issues"}. [default: "commits"]
        #' @param group.column The column to group the authors of the given \code{data.source} by
        #'                     [default: "artifact"]
        #'
        #' @return a list mapping each distinct item in \code{group.column} to all corresponding
        #'         data items from \code{data.source}, with the column \code{"author.name"} duplicated
        #'         as first column (with name \code{"data.vertices"})
        #'
        #' @seealso ProjectData$group.data.by.column
        group.authors.by.data.column = function(data.source = c("commits", "mails", "issues"),
                                                group.column = "artifact") {
            logging::loginfo("Grouping authors by data column.")

            ## store the authors per group that is determined by 'group.column'
            mylist = self$group.data.by.column(data.source, group.column, "author.name")

            return(mylist)
        },

        #' Group the authors of the given \code{data.source} by the given \code{group.column}.
        #' For each group, the column \code{"author.name"} is duplicated and prepended to each
        #' group's data as first column (see below for details).
        #'
        #' Example: To obtain the authors who touched the same source-code artifact,
        #' call \code{group.authors.by.data.column("commits", "artifact")}.
        #'
        #' Note: This method is a delegate for \code{ProjectData$group.authors.by.data.column}.
        #' It is deprecated and may be removed in any later version.
        #'
        #' @param data.source The specified data source. One of \code{"commits"},
        #'                    \code{"mails"}, and \code{"issues"}. [default: "commits"]
        #' @param group.column The column to group the authors of the given \code{data.source} by
        #'                     [default: "artifact"]
        #'
        #' @return a list mapping each distinct item in \code{group.column} to all corresponding
        #'         data items from \code{data.source}, with the column \code{"author.name"} duplicated
        #'         as first column (with name \code{"data.vertices"})
        #'
        #' @seealso ProjectData$group.data.by.column
        get.artifact2author = function(data.source = c("commits", "mails", "issues"), group.column) {
            logging::logwarn("The method 'ProjectData$get.artifact2author' is deprecated!")
            return(self$group.authors.by.data.column(data.source, group.column))
        },


        #' Group the artifacts of the given \code{data.source} by the given \code{group.column}.
        #' For each group, the column \code{artifact.column} is duplicated and prepended to each group's
        #' data as first column (see below for details).
        #'
        #' Example: To obtain the artifacts that have been touched by each author,
        #' call \code{group.artifacts.by.data.column("commits", "author.name")}.
        #'
        #' @param data.source The specified data source. One of \code{"commits"},
        #'                    \code{"mails"}, and \code{"issues"}. [default: "commits"]
        #' @param group.column The column to group the artifacts of the given \code{data.source} by
        #'                     [default: "author.name"]
        #' @param artifact.column The column that gets duplicated as first column \code{data.vertices}.
        #'                        If \code{NULL}, the column is automatically determined based on the
        #'                        given \code{data.source} (see \code{DATASOURCE.TO.ARTIFACT.COLUMN}
        #'                        for details). [default: DATASOURCE.TO.ARTIFACT.COLUMN[[data.source]]]
        #'
        #' @return a list mapping each distinct item in \code{group.column} to all corresponding
        #'         data items from \code{data.source}, with \code{artifact.column} duplicated as first
        #'         column (with name \code{"data.vertices"})
        #'
        #' @seealso ProjectData$group.data.by.column
        group.artifacts.by.data.column = function(data.source = c("commits", "mails", "issues"),
                                                  group.column = "author.name",
                                                  artifact.column = DATASOURCE.TO.ARTIFACT.COLUMN[[data.source]]) {
            logging::loginfo("Grouping artifacts by data column.")

            ## determine the artifact column if not given explicitly
            if (is.null(artifact.column)) {
                artifact.column = DATASOURCE.TO.ARTIFACT.COLUMN[[data.source]]
            }

            ## store the artifacts per group that is determined by 'group.column'
            mylist = self$group.data.by.column(data.source, group.column, artifact.column)

            return(mylist)
        },

        #' Group the artifacts of the given \code{data.source} by the given \code{group.column}.
        #' For each group, the column \code{artifact.column} is duplicated and prepended to each group's
        #' data as first column (see below for details).
        #'
        #' Example: To obtain the artifacts that have been touched by each author,
        #' call \code{group.artifacts.by.data.column("commits", "author.name")}.
        #'
        #' Note: This method is a delegate for \code{ProjectData$group.artifacts.by.data.column}.
        #' It is deprecated and may be removed in any later version.
        #'
        #' @param data.source The specified data source. One of \code{"commits"},
        #'                    \code{"mails"}, and \code{"issues"}. [default: "commits"]
        #' @param group.column The column to group the artifacts of the given \code{data.source} by
        #'                     [default: "author.name"]
        #' @param artifact.column The column that gets duplicated as first column \code{data.vertices}.
        #'                        If \code{NULL}, the column is automatically determined based on the
        #'                        given \code{data.source} (see \code{DATASOURCE.TO.ARTIFACT.COLUMN}
        #'                        for details). [default: DATASOURCE.TO.ARTIFACT.COLUMN[[data.source]]]
        #'
        #' @return a list mapping each distinct item in \code{group.column} to all corresponding
        #'         data items from \code{data.source}, with \code{artifact.column} duplicated as first
        #'         column (with name \code{"data.vertices"})
        #'
        #' @seealso ProjectData$group.data.by.column
        get.author2artifact = function(data.source = c("commits", "mails", "issues"),
                                                  group.column = "author.name",
                                                  artifact.column = DATASOURCE.TO.ARTIFACT.COLUMN[[data.source]]) {
            logging::logwarn("The method 'ProjectData$get.author2artifact' is deprecated!")
            return(self$group.artifacts.by.data.column(data.source, group.column, artifact.column))
        },

        #' Group the data items of the given \code{data.source} by the given \code{group.column}.
        #' For each group, the column \code{data.column} is duplicated and prepended to each group's
        #' data as first column (see \code{get.key.to.value.from.df} for details).
        #'
        #' Example: To obtain the authors who touched the same source-code artifact,
        #' call \code{group.data.by.column("commits", "artifact", "author.name")}.
        #'
        #' @param data.source The specified data source. One of \code{"commits"},
        #'                    \code{"mails"}, and \code{"issues"}. [default: "commits"]
        #' @param group.column The column to group the data of the given \code{data.source} by
        #' @param data.column The column that gets duplicated as first column \code{data.vertices}
        #'
        #' @return a list mapping each distinct item in \code{group.column} to all corresponding
        #'         data items from \code{data.source}, with \code{data.column} duplicated as first
        #'         column (with name \code{"data.vertices"})
        #'
        #' @seealso get.key.to.value.from.df
        group.data.by.column = function(data.source = c("commits", "mails", "issues"),
                                        group.column, data.column) {
            logging::loginfo("Grouping artifacts by data column.")

            ## check given data source
            data.source = match.arg.or.default(data.source, several.ok = FALSE)
            data.source.func = DATASOURCE.TO.ARTIFACT.FUNCTION[[data.source]]

            ## get the key-value mapping/list for the given parameters
            mylist = get.key.to.value.from.df(self[[data.source.func]](), group.column, data.column)

            return(mylist)
        },

        #' Get the list of authors for the specified data sources.
        #'
        #' *Note*: The constant \code{DATASOURCE.TO.ARTIFACT.FUNCTION} denotes the mapping between
        #' data source and the method which is retrieving the data for each data source.
        #'
        #' @param data.sources the data sources from which the authors should be retrieved,
        #'                    can be either \code{"commits"}, \code{"mails"}, or \code{"issues"},
        #'                    or any combination of them [default: c("commits", "mails", "issues")]
        #'
        #' @return a data.frame of unique author names (columns \code{name} and \code{author.email}),
        #'         extracted from the specified data source
        get.authors.by.data.source = function(data.sources = c("commits", "mails", "issues")) {

            data.sources = match.arg.or.default(data.sources, several.ok = TRUE)

            ## retrieve author names from chosen data source
            data = lapply(data.sources, function(data.source){
                data.source.func = DATASOURCE.TO.ARTIFACT.FUNCTION[[data.source]]
                data.source.authors = self[[data.source.func]]()[c("author.name", "author.email")]
                return (data.source.authors)
            })

            data = plyr::rbind.fill(data)

            ## remove duplicates
            data = unique(data)

            return (data)
        }
    )
)


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## RangeData ---------------------------------------------------------------

#' The class \code{RangeData} is a subclass of \code{ProjectData} and is used to
#' represent only a subset of the data (i.e., a range) of a complete
#' \code{ProjectData} object.
#'
#' @seealso ProjectData
#' @seealso ProjectConf
RangeData = R6::R6Class("RangeData", inherit = ProjectData,

    ## * private -----------------------------------------------------------

    private = list(
        range = NULL, # character
        revision.callgraph = NA # character
    ),

    ## * public ------------------------------------------------------------

    public = list(

        #' Constructor of the class. Constructs a new instance by calling the
        #' constructor of \code{ProjectData} with the given \code{ProjectConf} object and then
        #' setting the \code{range} and the \code{revision.callgraph} to the given ones
        #' if they exist.
        #'
        #' @param project.conf the \code{ProjectConf} object for the new instance
        #' @param range the range for the new instance
        #' @param revision.callgraph the revision callgraph for the new instance
        #'                           [default: ""]
        initialize = function(project.conf, range, revision.callgraph = "") {
            ## call super constructor
            super$initialize(project.conf)

            ## check arguments
            private$range = verify.argument.for.parameter(range, "character", "RangeData$new")
            private$revision.callgraph = verify.argument.for.parameter(revision.callgraph, "character", "RangeData$new")

            ## correct revision.callgraph variable
            if (revision.callgraph == "") {
                private$revision.callgraph = NA
            }

            logging::loginfo("Initialized data object %s", self$get.class.name())
        },

        ## * * printing ----------------------------------------------------

        #' The toString method of the class.
        get.class.name = function() {
            return(
                sprintf("RangeData<%s, %s, %s>",
                        private$project.conf$get.value("repo"),
                        private$range,
                        private$revision.callgraph
                )
            )
        },

        ## * * path retrieval ----------------------------------------------

        #' Construct and return the absolute path to the range's result folder.
        #'
        #' @return the path to the range's result folder
        get.data.path = function() {
            data.path = private$project.conf$get.value("datapath")
            range = private$project.conf$get.value("ranges.paths")[[private$range]]
            return(file.path(data.path, range))
        },

        #' Construct and return the absolute path to the range's result folder for callgraphs
        #'
        #' @return the path to the range's result folder for callgraphs
        get.data.path.callgraph = function() {
            data.path = file.path(private$project.conf$get.value("datapath.callgraph"), private$revision.callgraph)
            return(data.path)
        },


        ## * * raw data ----------------------------------------------------

        #' Get the 'range' of the current instance.
        #'
        #' @return the range
        get.range = function() {
            return(private$range)
        },

        #' Get the bounds of the current instance
        #'
        #' @return Returns a vector with two entries (start, end) of type POSIXct if input was a date;
        #'         or of type character if input was a commit hash or version;
        #'         or NULL if the string could not be parsed
        get.range.bounds = function() {
            return (get.range.bounds(private$range))
        },

        #' Get the 'revision.callgraph' of the current instance
        #'
        #' @return the revision callgraph
        get.revision.callgraph = function() {
            return(private$revision.callgraph)
        },

        #' Compares two RangeData objects by first comparing the ranges. Then it compares
        #' the revision callgraphs and if they are equal, it calls the equals method of
        #' ProjectData to compare the remaining data sources.
        #'
        #' @param other.data.object the object with which to compare
        #'
        #' @return \code{TRUE} if the objects are equal and \code{FALSE} otherwise
        equals = function(other.data.object = NULL) {

            ## check whether the given object is an instance of RangeData
            if (!("RangeData" %in% class(other.data.object))) {
                logging::logerror("You can only compare a RangeData object against another one.")
                return(FALSE)
            }

            ## check whether the ranges are equal
            if (!identical(self$get.range(), other.data.object$get.range())) {
                return(FALSE)
            }

            ## check whether the revision callgraphs are equal
            if (!identical(self$get.revision.callgraph(),
                           other.data.object$get.revision.callgraph())) {
                return(FALSE)
            }

            ## check the equality of the remaining data sources
            return(super$equals(other.data.object = other.data.object))
        }
    )
)


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Helper functions --------------------------------------------------------

#' Transform the 'base data' data.frame to a list in order to execute
#' the following tasks:
#'  - split by column given by key
#'  - use value as first column in sublist items with name 'data.vertices'
#'  - append all other existing columns (including key and value)
#'  - each item in the results list gets attributes:
#'   - group.type (i.e., value) and
#'   - group.name (i.e., unique(item[[key]]))
#'
#' @param base.data the base data for the method
#' @param key the key for the result
#' @param value the value for the result
#' @param ... a possibility for further attributes to be passed
#'
#' @return the resulting list
get.key.to.value.from.df = function(base.data, key, value, ...) {
    logging::logdebug("get.key.to.value.from.df: starting.")

    ## define names for key (key) and value (value) columns
    column.key = "data.coupling"
    column.value = "data.vertices"

    ## if there is not data to subset, return am enpty list directly
    if (nrow(base.data) == 0) {
        logging::logwarn("Trying to get subset of non-existent data.")
        logging::logwarn(sprintf("Stacktrace:  %s", get.stacktrace(sys.calls())))
        logging::logdebug("get.key.to.value.from.df: finished.")
        return(list())
    }

    ## check key and value columns for existence
    if (!all(c(key, value) %in% colnames(base.data))) {
        logging::logerror("Trying to group data by non-existent columns (either '%s' or '%s').", key, value)
        logging::logerror(sprintf("Stacktrace:  %s", get.stacktrace(sys.calls())))
        logging::logdebug("get.key.to.value.from.df: stopped.")
        stop("Trying to group data by non-existent columns.")
    }

    ## re-arrange columns and use things as first columns
    cols.old = colnames(base.data)
    base.data = base.data[c(key, value, cols.old)]
    colnames(base.data) = c(column.key, column.value, cols.old)

    ## group list by key and construct a list: key -> data.frame(value, other columns)
    transform.df.per.item = function(df) {
        group = unique(df[[key]])
        ## remove key column from list of columns and keep data.frame
        df = df[, -match(c(column.key), names(df)), drop = FALSE]
        ## add group information as attributes
        attr(df, "group.type") = key
        attr(df, "group.name") = group
        return(df)
    }
    mylist = plyr::dlply(base.data, key, transform.df.per.item)

    ## remove object attributes introduced by dlply
    attr(mylist, "split_labels") = NULL
    attr(mylist, "split_type") = NULL

    logging::logdebug("get.key.to.value.from.df: finished.")

    return(mylist)
}
