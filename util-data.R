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
## Copyright 2017-2018 by Thomas Bock <bockthom@fim.uni-passau.de>
## Copyright 2017 by Raphael Nömmer <noemmer@fim.uni-passau.de>
## Copyright 2017-2018 by Christian Hechtl <hechtl@fim.uni-passau.de>
## Copyright 2017 by Felix Prasse <prassefe@fim.uni-passau.de>
## Copyright 2017 by Ferdinand Frank <frankfer@fim.uni-passau.de>
## Copyright 2018 by Jakob Kronawitter <kronawij@fim.uni-passau.de>
## All Rights Reserved.


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Libraries ---------------------------------------------------------------

requireNamespace("R6") # for R6 classes
requireNamespace("logging") # for logging
requireNamespace("parallel") # for parallel computation


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Constants ---------------------------------------------------------------

## base artifacts
BASE.ARTIFACTS = c(
    "Base_Feature",
    "File_Level"
)

## mapping of data source to artifact column
## (for commits: filter also empty, non-configured, and (potentially) base artifacts)
DATASOURCE.TO.ARTIFACT.FUNCTION = list(
    "commits" = "get.commits.filtered",
    "mails"   = "get.mails",
    "issues"  = "get.issues"
)

## mapping of data source to artifact column
DATASOURCE.TO.ARTIFACT.COLUMN = list(
    "commits" = "artifact",
    "mails"   = "thread",
    "issues"  = "issue.id"
)


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## ProjectData -------------------------------------------------------------

ProjectData = R6::R6Class("ProjectData",

    ## * private -----------------------------------------------------------

    private = list(

        ## * * configuration -----------------------------------------------

        project.conf = NULL, # list

        ## * * raw data ----------------------------------------------------

        ## commits and commit data
        commits.filtered = NULL, # data.frame
        commits = NULL, # data.frame
        synchronicity = NULL, # data.frame
        pasta = NULL, # data.frame
        ## mails
        mails = NULL, # data.frame
        ## authors
        authors = NULL, # data.frame
        ## issues
        issues = NULL, #data.frame
        ## timestamps of mail, issue and commit data
        data.timestamps = NULL, #data.frame

        ## * * filtering commits -------------------------------------------

        #' Filter commits retrieved by the \code{get.commits} method by removing untracked files and removing the base
        #' artifact (see parameters).
        #'
        #' @param remove.untracked.files configures if untracked files should be kept or removed
        #' @param remove.base.artifact configures if the base artifact should be kept or removed
        #'
        #' @return the commits retrieved by the \code{get.commits} method after all filters have been applied
        filter.commits = function(remove.untracked.files, remove.base.artifact) {
            logging::logdebug("filter.commits: starting.")

            ## get commit data
            commit.data = self$get.commits()

            ## filter out the untracked files
            if (remove.untracked.files) {
                commit.data = subset(commit.data, artifact != "untracked.file")
            }

            ## filter out the base artifacts (i.e., Base_Feature, File_Level)
            if (remove.base.artifact) {
                commit.data = subset(commit.data, !(artifact %in% BASE.ARTIFACTS))
            }

            logging::logdebug("filter.commits: finished.")
            return(commit.data)
        },

        ## * * PaStA data --------------------------------------------------

        #' Add the PaStA data to the given data.frame for further analysis.
        #'
        #' @param data the base data as data.frame to append the PaStA data to.
        #'
        #' @return the augmented data.frame
        add.pasta.data = function(data) {
            logging::loginfo("Adding PaStA data.")
            data[, "pasta"] = NA
            if ("message.id" %in% colnames(data)) {
                for (i in 1:nrow(data)) {
                    pasta.item = self$get.pasta.items(message.id = data[i, "message.id"])
                    if (!length(pasta.item) == 0) {
                        data$pasta[i] = I(list(pasta.item))
                    }
                }
            } else if ("hash" %in% colnames(data)) {
                for (i in 1:nrow(data)) {
                    pasta.item = self$get.pasta.items(commit.hash = data[i, "hash"])
                    if (!length(pasta.item) == 0) {
                        data$pasta[i] = I(list(pasta.item))
                    }
                }
            }
            return(data)
        },

        ## * * timestamps --------------------------------------------------

        #' Call the getters of the specified data sources in order to
        #' initialize the sources and extract the timestamps.
        #'
        #' @param data.sources the data sources to be prepated
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
            private$project.conf = verify.argument.for.parameter(project.conf, "ProjectConf", class(self)[1])

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
            private$synchronicity = NULL
            private$mails = NULL
            private$authors = NULL
            private$pasta = NULL
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
            private$project.conf = verify.argument.for.parameter(project.conf, "ProjectConf", class(self)[1])

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

        #' Return the commits retrieved by the \code{get.commits} method by removing untracked files and removing the
        #' base artifact (if configured in the \code{project.conf}, see parameters \code{filter.untracked.files} and
        #' \code{artifact.filter.base}). This method uses caching.
        #'
        #' @param remove.untracked.files configures if untracked files should be kept or removed
        #' @param remove.base.artifact configures if the base artifact should be kept or removed
        #'
        #' @return the commits retrieved by the \code{get.commits} method after all filters have been applied
        #'
        #' @seealso get.commits.filtered.uncached
        get.commits.filtered = function() {
            if (is.null(private$commits.filtered)) {
                private$commits.filtered = private$filter.commits(
                    private$project.conf$get.value("filter.untracked.files"),
                    private$project.conf$get.value("artifact.filter.base")
                )
            }
            return(private$commits.filtered)
        },

        #' Return the commits retrieved by the \code{get.commits} method by removing untracked files and removing the
        #' base artifact (see parameters). This method doesn't use caching. If you want to use caching, please use the
        #' \code{get.commits.filtered} method instead.
        #'
        #' @param remove.untracked.files configures if untracked files should be kept or removed
        #' @param remove.base.artifact configures if the base artifact should be kept or removed
        #'
        #' @return the commits retrieved by the \code{get.commits} method after all filters have been applied
        #'
        #' @seealso get.commits.filtered
        get.commits.filtered.uncached = function(remove.untracked.files, remove.base.artifact) {
            return (private$filter.commits(remove.untracked.files, remove.base.artifact))
        },

        #' Get the list of commits which have the artifact kind configured in the \code{project.conf}.
        #' If the list of commits is not cached, call the read method first.        #'
        #' If configured in the field \code{project.conf}, add PaStA and synchronicity data.
        #'
        #' @return the list of commits
        get.commits = function() {
            logging::loginfo("Getting commit data.")

            ## if commits are not read already, do this
            if (is.null(private$commits)) {
                commit.data = read.commits(self$get.data.path(), private$project.conf$get.value("artifact"))

                ## only process commits with the artifact listed in the configuration or missing
                commit.data = subset(commit.data, artifact.type %in%
                                         c(private$project.conf$get.value("artifact.codeface"), ""))

                ## saves the commit.data to the commits cache field after PaStA and synchronicity data is added
                self$set.commits(commit.data)
            }
            private$extract.timestamps(source = "commits")

            return(private$commits)
        },

        #' Set the commit list of the project to a new one.
        #' Add PaStA and sychronicity data if configured in the field \code{project.conf}.
        #'
        #' @param commit.data the new list of commits
        set.commits = function(commit.data) {
            logging::loginfo("Setting commit data.")

            if (!is.null(commit.data)) {

                ## append synchronicity data if wanted
                if (private$project.conf$get.value("synchronicity")) {
                    synchronicity.data = self$get.synchronicity()
                    commit.data = merge(commit.data, synchronicity.data,
                                        by = "hash", all.x = TRUE, sort = FALSE)
                }

                ## add PaStA data if wanted
                if (private$project.conf$get.value("pasta")) {
                    self$get.pasta()
                    commit.data = private$add.pasta.data(commit.data)
                }
            }

            private$commits = commit.data

            ## remove cached data for filtered commits as these need to be re-computed after changing the data
            private$commits.filtered = NULL
        },

        #' Get the synchronicity data.
        #' If it does not already exist call the read method.
        #'
        #' @return the synchronicity data
        get.synchronicity = function() {
            logging::loginfo("Getting synchronicity data.")

            ## if commits are not read already, do this
            if (is.null(private$synchronicity)) {
                private$synchronicity = read.synchronicity(
                    self$get.data.path.synchronicity(),
                    private$project.conf$get.value("artifact"),
                    private$project.conf$get.value("synchronicity.time.window")
                )
            }

            return(private$synchronicity)
        },

        #' Set the synchronicity data to the given data.
        #'
        #' @param data the new synchronicity data
        set.synchronicity = function(data) {
            logging::loginfo("Setting synchronicity data.")
            private$synchronicity = data
        },

        #' Get the PaStA data.
        #' If it does not already exist call the read method.
        #'
        #' @return the PaStA data
        get.pasta = function() {
            logging::loginfo("Getting PaStA data.")

            ## if commits are not read already, do this
            if (is.null(private$pasta)) {
                private$pasta = read.pasta(self$get.data.path.pasta())
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
            private$pasta = data
            if (private$project.conf$get.value("pasta")) {
                logging::loginfo("Updating PaStA data.")
                if (!is.null(private$commits)) {
                    self$set.commits(private$commits)
                }
                if (!is.null(private$mails)) {
                    self$set.mails(private$mails)
                }
            }
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

                self$set.mails(data = mails.read)
            }
            private$extract.timestamps(source = "mails")

            return(private$mails)
        },

        #' Set the mail data to the given new data and add PaStA data
        #' if configured in the field \code{project.conf}.
        #'
        #' @param data the new mail data
        set.mails = function(data) {
            logging::loginfo("Setting e-mail data.")
            if (is.null(data)) {
                data = data.frame()
            }
            ## add PaStA data if wanted
            if (private$project.conf$get.value("pasta")) {
                logging::loginfo("Adding PaStA data.")
                data = private$add.pasta.data(data = data)
            }

            private$mails = data
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

        #' Get the issue data.
        #' If it does not already exist call the read method.
        #'
        #' @return the issue data
        get.issues = function() {
            logging::loginfo("Getting issue data")

            ## if issues have not been read yet do this
            if (is.null(private$issues)) {
                private$issues = read.issues(self$get.data.path.issues())
            }
            private$extract.timestamps(source = "issues")

            if (private$project.conf$get.value("issues.only.comments")) {
                df = private$issues[private$issues[["event.name"]] == "commented", ]
                return(df)
            } else {
                return(private$issues)
            }
        },

        #' Set the issue data to the given new data.
        #'
        #' @param data the new issue data
        set.issues = function(data) {
            logging::loginfo("Setting issue data.")
            if (is.null(data)) data = data.frame()
            private$issues = data
        },

        #' Get the list of artifacts from the given \code{data.source} of the project.
        #'
        #' @param data.source The specified data source. One of \code{"commits"},
        #'                    \code{"mails"}, and \code{"issues"}. [default: "commits"]
        #'
        #' @return the character vector of unique artifacts (can be empty)
        get.artifacts = function(data.source = c("commits", "mails", "issues")) {
            logging::loginfo("Getting artifact data.")

            ## check given data source
            data.source = match.arg.or.default(data.source, several.ok = FALSE)
            data.source.func = DATASOURCE.TO.ARTIFACT.FUNCTION[[data.source]]
            data.source.col = DATASOURCE.TO.ARTIFACT.COLUMN[[data.source]]

            ## get actual artifact data
            data = self[[data.source.func]]()
            artifacts = unique(data[[data.source.col]])

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
            result = c()

            ## main data sources
            if (!is.null(private$commits)) {
                result = c(result, "commits")
            }
            if (!is.null(private$mails)) {
                result = c(result, "mails")
            }
            if (!is.null(private$issues)) {
                result = c(result, "issues")
            }

            ## author data
            if (!is.null(private$authors)) {
                result = c(result, "authors")
            }

            ## additional data sources
            if (!is.null(private$synchronicity)) {
                result = c(result, "synchronicity")
            }
            if (!is.null(private$pasta)) {
                result = c(result, "pasta")
            }

            return(result)
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
            timestamps = c(start = timestamps.df[, "start"], end = timestamps.df[, "end"])

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
        }
    )
)


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## RangeData ---------------------------------------------------------------

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
            private$range = verify.argument.for.parameter(range, "character", class(self)[1])
            private$revision.callgraph = verify.argument.for.parameter(revision.callgraph, "character", class(self)[1])

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
            range = private$range
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

#' Get the commit data with the specified columns for the specified project-data instance
#' as a data frame for each specified split range.
#'
#' A split interval can be set by defining the number of weeks for each requested range as a vector.
#'
#' @param project.data The project data as source for the commits
#' @param columns The commit-data columns to select and return
#' @param split A list of numerics, indicating numbers of weeks into which the selected data
#'              is to be split
#'
#' @return A data.frame indicating the selected \code{columns}, split into the given numbers
#'         of weeks (\code{split})
get.commit.data = function(project.data, columns = c("author.name", "author.email"), split = c()) {
    logging::logdebug("get.commit.data: starting.")

    ## Get commit data
    commits.df = project.data$get.commits()

    ## In case no commit data is available, return NA
    if (nrow(commits.df) == 0) {
        return(NA)
    }

    ## Make sure the hash is included in the cut columns vector for grouping
    cut.columns = columns
    if (!("hash" %in% cut.columns)) {
        cut.columns = c(cut.columns, "hash")
    }

    ## Make sure the date is included in the cut columns vector for splitting
    if (!("date" %in% cut.columns)) {
        cut.columns = c(cut.columns, "date")
    }

    ## Cut down data to needed minimum
    commits.df = commits.df[cut.columns]

    ## Group by hash to get a line per commit
    commits.df = sqldf::sqldf("SELECT * FROM `commits.df` GROUP BY `hash`")

    ## Remove hash column if not wanted as it now contains nonsensical data
    if (!("hash" %in% columns)) {
        commits.df["hash"] = NULL
    }

    ## Order commits by date column
    commits.df = commits.df[order(commits.df$date), ]

    ## Fetch the date range info
    date.first = as.Date(commits.df$date[1])
    date.last = as.Date(commits.df$date[nrow(commits.df)]) + 1 # +1 since findInterval is right-exclusive

    ## Calc the split dates depending on the specified intervals
    date.split = c(date.last)
    if (!is.null(split)) {
        for (i in 1:length(split)) {
            ## subtract split[i] number of weeks
            date.calc = date.split[i] - lubridate::weeks(split[i])

            ## Check if calculated date is still after the first commit date of the range
            if (date.calc > date.first) {
                date.split = c(date.split, date.calc)
            } else {
                date.split = c(date.split, date.first)
                break
            }
        }
    } else {
        date.split = c(date.split, date.first)
    }

    date.split = rev(date.split)

    ## Only keep the commits which were made within the specified split ranges
    ## TODO https://github.com/se-passau/codeface-extraction-r/pull/51#discussion_r132924711
    commits.df = commits.df[as.Date(commits.df$date) >= date.split[1], ]

    ## Calc group numbers for the commits by the split dates
    intervals = findInterval(as.Date(commits.df[["date"]]), date.split, all.inside = FALSE)

    ## Remove date column if not wanted
    if (!("date" %in% columns)) {
        commits.df["date"] = NULL
    }

    ## Split the commits by the calculated groups
    res = split.data.by.bins(commits.df, intervals)
    names(res) = construct.ranges(date.split)
    attr(res, "bins") = date.split

    logging::logdebug("get.commit.data: finished.")
    return(res)
}

