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
## Copyright 2016 by Wolfgang Mauerer <wolfgang.mauerer@oth-regensburg.de>
## Copyright 2017 by Raphael Nömmer <noemmer@fim.uni-passau.de>
## Copyright 2017-2018 by Christian Hechtl <hechtl@fim.uni-passau.de>
## Copyright 2020-2021, 2024 by Christian Hechtl <hechtl@cs.uni-saarland.de>
## Copyright 2017 by Felix Prasse <prassefe@fim.uni-passau.de>
## Copyright 2017-2019 by Thomas Bock <bockthom@fim.uni-passau.de>
## Copyright 2021, 2023-2024 by Thomas Bock <bockthom@cs.uni-saarland.de>
## Copyright 2018 by Barbara Eckl <ecklbarb@fim.uni-passau.de>
## Copyright 2018-2019 by Jakob Kronawitter <kronawij@fim.uni-passau.de>
## Copyright 2019 by Anselm Fehnker <fehnker@fim.uni-passau.de>
## Copyright 2020-2021 by Niklas Schneider <s8nlschn@stud.uni-saarland.de>
## Copyright 2021 by Johannes Hostert <s8johost@stud.uni-saarland.de>
## Copyright 2021 by Mirabdulla Yusifli <s8miyusi@stud.uni-saarland.de>
## Copyright 2022 by Jonathan Baumann <joba00002@stud.uni-saarland.de>
## Copyright 2024 by Leo Sendelbach <s8lesend@stud.uni-saarland.de>
## All Rights Reserved.


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Libraries ---------------------------------------------------------------

requireNamespace("R6") # for R6 classes
requireNamespace("yaml") # for Codeface configuration files
requireNamespace("logging")


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Constants ---------------------------------------------------------------

## mapping of tagging to artifact
ARTIFACT.TO.TAGGING = list(
    "feature"  = "feature",
    "featureexpression" = "feature",
    "function" = "proximity",
    "file"     = "proximity"
)

ARTIFACT.TO.ABBREVIATION = list(
    "feature"  = "f",
    "featureexpression" = "fe",
    "function" = "func",
    "file"     = "cu"
)

ARTIFACT.CODEFACE = list(
    "feature"  = "Feature",
    "featureexpression" = "FeatureExpression",
    "function" = "Function",
    "file"     = "File"
)


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Conf --------------------------------------------------------------------

#' The class \code{Conf} provides convenient handling of a predefined set of
#' configuration options or, rather, attributes.
#'
#' **Note:** This class is considered abstract and should not instantiated directly,
#' as the list of handled attributes is empty.
Conf = R6::R6Class("Conf",

    ## * private -----------------------------------------------------------

    private = list(
        ## * * attributes --------------------------------------------------
        attributes = list(),

        ## * * value checking ----------------------------------------------

        #' Check all attributes for their consistency.
        #'
        #' @return a logical indicating if the checks have been successful
        check.values = function() {
            current.values = lapply(names(private$attributes), function(att) {
                return(self$get.value(att))
            })
            names(current.values) = names(private$attributes)
            self$update.values(current.values)
        },

        #' Check whether the given 'value' is the correct datatype
        #' for the attribute 'name'.
        #'
        #' @param value the new value for the attribute
        #' @param name the name of the attribute
        #'
        #' @return a named vector of logical values, named:
        #'         - existing,
        #'         - value.not.empty,
        #'         - type,
        #'         - allowed,
        #'         - allowed.number, and
        #'         - updatable.
        #'         Does not need to contain all of the above; e.g. if \code{existing} is \code{FALSE} the result
        #'         consists of only this value.
        check.value = function(value, name) {
            if (!exists(name, where = private[["attributes"]])) {
                result = c(existing = FALSE)
            } else if (length(value) < 1){
                result = c(existing = TRUE, value.not.empty = FALSE)
            } else {
                ## check all other properties
                attribute = private[["attributes"]][[name]]
                ## if non-updatable field, return early
                if (!is.null(attribute[["updatable"]]) && !attribute[["updatable"]]) {
                    result = c(existing = TRUE, value.not.empty = TRUE, updatable = FALSE)
                } else {
                    result = c(
                        existing = TRUE,
                        value.not.empty = TRUE,
                        updatable = TRUE,
                        type = class(value) %in% attribute[["type"]],
                        ## if 'allowed' is not defined for this attribute, any
                        ## value of the correct type should be accepted.
                        allowed = is.null(attribute[["allowed"]]) ||
                            if (attribute[["type"]] == "numeric" && length(attribute[["allowed"]]) == 1) {
                                value <= attribute[["allowed"]]
                            } else {
                                all(value %in% attribute[["allowed"]])
                            },
                        allowed.number = length(value) <= attribute[["allowed.number"]]
                    )
                }
            }
            return(result)
        }
    ),

    ## * public ------------------------------------------------------------

    public = list(

        #' The constructor, automatically checking the default values.
        initialize = function() {
            # ## check the default values for validity
            # private$check.values()
        },

        ## * * printing ----------------------------------------------------

        #' Get configuration list as string.
        #'
        #' @param allowed Indicator whether to return also information on allowed values
        #'
        #' @return the configuration list as string [default: FALSE]
        get.conf.as.string = function(allowed = FALSE) {
            ## get the complete list of attributes
            attributes = private$attributes

            ## remove information on allowed values if not wanted
            if (!allowed) {
                attributes = lapply(attributes, function(att) {
                    att[["allowed"]] = NULL
                    att[["allowed.number"]] = NULL
                    att[["type"]] = NULL
                    return(att)
                })
            }

            return(get.configuration.string(attributes, title = NULL))
        },

        #' Print the private variables of the class.
        #'
        #' @param allowed Indicator whether to print information on allowed values [default: FALSE]
        print = function(allowed = FALSE) {
            logging::loginfo("Configuration:\n%s", self$get.conf.as.string(allowed))
        },

        ## * * updating ----------------------------------------------------

        #' Update the attributes of the class with the new value for the given entry.
        #'
        #' @param entry the entry name for the value
        #' @param value the new value
        update.value = function(entry, value) {
            ## construct list for updating
            updating = list(value)
            names(updating) = entry
            ## update value
            self$update.values(updating)
        },

        #' Update the attributes of the class with the new values given in the
        #' 'updated.values' list.
        #'
        #' @param updated.values the new values for the attributes to be updated [default: list()]
        update.values = function(updated.values = list()) {
            ## determine the function executed on an error
            error.function = stop

            ## check values to update
            names.to.update = c()
            for (name in names(updated.values)) {
                ## get value to update
                value = updated.values[[name]]
                ## check the value
                check = private$check.value(value = value, name = name)

                ## if all checks are passed
                if (all(check)) {
                    names.to.update = c(names.to.update, name)
                }
                ## if some check failed
                else {
                    ## get current environment for logging
                    attribute = private[["attributes"]][[name]]

                    if (!check[["existing"]]) {

                        message = paste(
                            "Updating configuration attribute '%s' failed:",
                            "A configuraton attribute with this name does not exist."
                        )
                        error.function(sprintf(message, name))

                    } else if (!check[["value.not.empty"]]) {

                        message = paste(
                            "Updating configuration attribute '%s' failed:",
                            "The provided value is empty!"
                        )
                        error.function(sprintf(message, name))

                    } else if (!check[["updatable"]]) {

                        message = paste(
                            "Updating configuration attribute '%s' failed:",
                            "The value is not updatable!"
                        )
                        error.function(message, name)

                    } else {

                        message = paste0(
                            "Updating configuration attribute '%s' failed.\n",
                            "Allowed values (%s of type '%s'): %s\n",
                            "Given value (of type '%s'): %s"
                        )
                        error.function(sprintf(
                            message,
                            name,
                            # paste(self$get.value(name), collapse = ", "),
                            attribute[["allowed.number"]],
                            attribute[["type"]],
                            if (attribute[["type"]] == "numeric" && length(attribute[["allowed"]]) == 1) {
                                paste("<=", attribute[["allowed"]])
                            } else {
                                paste(attribute[["allowed"]], collapse = ", ")
                            },
                            class(value),
                            paste(value, collapse = ", ")
                        ))

                    }
                }
            }

            ## Updating values if needed
            if (length(names.to.update) > 0) {
                logging::loginfo(
                    "Updating following configuration parameters: %s",
                    paste(names.to.update, collapse = ", ")
                )
                for (name in names.to.update) {
                    default.value = private[["attributes"]][[name]][["default"]]
                    new.value = updated.values[[name]]

                    ## check if the default value or the given new value are NA
                    ## if only one of both is NA that means that the value has to be changed
                    if (is.single.na(default.value) && !is.single.na(new.value) ||
                        !is.single.na(default.value) && is.single.na(new.value)) {
                        private[["attributes"]][[name]][["value"]] = new.value
                    } ## if the default value and the given value are the same and if the 'value' field is present
                      ## then reset the 'value' field
                    else if (is.single.na(default.value) && is.single.na(new.value) ||
                             identical(sort(new.value), sort(default.value))) {
                        if ("value" %in% names(private[["attributes"]][[name]])) {
                            private[["attributes"]][[name]][["value"]] = NULL
                        }
                    } ## otherwise proceed with updating the value
                    else {
                        private[["attributes"]][[name]][["value"]] = sort(new.value)
                    }
                }
            } else {
                logging::logwarn(
                    "Not updating any configuration parameters!"
                )
            }

            ## return invisible
            invisible()
        },

        ## * * value retrieval ---------------------------------------------

        #' Get the value whose name is given by 'name'.
        #'
        #' @param name the name of the value to be returned
        #'
        #' @return the value
        get.value = function(name) {
            value = private[["attributes"]][[name]][["value"]]

            ## if there is no value set, retrieve the default
            if (is.null(value)) {
                value = self$get.value.default(name)
            }

            return(value)
        },

        #' Get the value whose name is given by 'name'.
        #'
        #' @param name the name of the value to be returned
        #'
        #' @return the value
        get.entry = function(name) {
            return(self$get.value(name))
        },

        #' Get the value whose name is given by 'name'.
        #'
        #' @param name the name of the value to be returned
        #'
        #' @return the value
        get.variable = function(name) {
            return(self$get.value(name))
        },

        #' Get the default value for the entry whose name is given by 'var.name'.
        #'
        #' @param name the name of the entry to be returned
        #'
        #' @return the default entry value
        get.value.default = function(name) {
            value = private[["attributes"]][[name]][["default"]]

            return(value)
        }

    )
)


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## ProjectConf -------------------------------------------------------------

#' The class \code{ProjectConf} is a subclass of \code{Conf} and provides
#' convenient handling of project-related configuration attributes with the
#' class \code{ProjectData}.
#'
#' @seealso Conf
#' @seealso ProjectData
#' @seealso RangeData
ProjectConf = R6::R6Class("ProjectConf", inherit = Conf,

    ## * private -----------------------------------------------------------

    private = list(

        ## * * project info ------------------------------------------------

        data = NULL, # character
        selection.process = NULL, # character
        casestudy = NULL, # character
        artifact = NULL, # character

        ## * * attributes ---------------------------------------------------

        attributes = list(
            commits.filter.base.artifact = list(
                default = TRUE,
                type = "logical",
                allowed = c(TRUE, FALSE),
                allowed.number = 1
            ),
            commits.filter.untracked.files = list(
                default = TRUE,
                type = "logical",
                allowed = c(TRUE, FALSE),
                allowed.number = 1
            ),
            commits.locked = list(
                default = FALSE,
                type = "logical",
                allowed = c(TRUE, FALSE),
                allowed.number = 1
            ),
            commit.messages = list(
                default = "none",
                type = "character",
                allowed = c("none", "title", "message"),
                allowed.number = 1
            ),
            issues.only.comments = list(
                default = TRUE,
                type = "logical",
                allowed = c(TRUE, FALSE),
                allowed.number = 1
            ),
            filter.bots = list(
                default = FALSE,
                type = "logical",
                allowed = c(TRUE, FALSE),
                allowed.number = 1
            ),
            issues.from.source = list(
                default = "github",
                type = "character",
                allowed = c("jira", "github"),
                allowed.number = Inf
            ),
            issues.locked = list(
                default = FALSE,
                type = "logical",
                allowed = c(TRUE, FALSE),
                allowed.number = 1
            ),
            mails.filter.patchstack.mails = list(
                default = FALSE,
                type = "logical",
                allowed = c(TRUE, FALSE),
                allowed.number = 1
            ),
            mails.locked = list(
                default = FALSE,
                type = "logical",
                allowed = c(TRUE, FALSE),
                allowed.number = 1
            ),
            gender = list(
                default = FALSE,
                type = "logical",
                allowed = c(TRUE, FALSE),
                allowed.number = 1
            ),
            synchronicity = list(
                default = FALSE,
                type = "logical",
                allowed = c(TRUE, FALSE),
                allowed.number = 1
            ),
            synchronicity.time.window = list(
                default = 5,
                type = "numeric",
                allowed = c(1, 5, 10, 15),
                allowed.number = 1
            ),
            pasta = list(
                default = FALSE,
                type = "logical",
                allowed = c(TRUE, FALSE),
                allowed.number = 1
            ),
            commit.interactions = list(
                default = FALSE,
                type = "logical",
                allowed = c(TRUE, FALSE),
                allowed.number = 1
            ),
            commit.interactions.filter.global = list(
                default = TRUE,
                type = "logical",
                allowed = c(TRUE, FALSE),
                allowed.number = 1
            ),
            custom.event.timestamps.file = list(
                default = NA,
                type = "character",
                allowed.number = 1
            ),
            custom.event.timestamps.locked = list(
                default = FALSE,
                type = "logical",
                allowed = c(TRUE, FALSE),
                allowed.number = 1
            )
        ),

        ## * * revisions and ranges ----------------------------------------

        #' Change the revision names to a equal name standard.
        #'
        #' @param ranges the list of ranges to be postprocessed
        #'
        #' @return the postprocessed ranges
        postprocess.revision.list = function(ranges) {
            # remove names, e.g. "version", from release cycle names
            casestudy = private$casestudy
            to.remove = c(
                "version-", "v-", "version_", "v_", "version", "v",
                paste0(casestudy, "-"), paste0(casestudy, "-"),
                paste0(casestudy, "_"), paste0(casestudy, "_"),
                casestudy, casestudy
            )

            # run gsub for all pattern
            ranges = tolower(ranges)
            for (string in to.remove) {
                ranges = gsub(string, "", ranges)
            }

            # return simplified list of ranges
            return(ranges)
        },

        #' Change the revision names of callgraph data to a equal name standard.
        #'
        #' @param r list of revisions to be postprocessed
        #'
        #' @return list of postprocessed revisions
        postprocess.revision.list.for.callgraph.data = function(r) {
            r = gsub("version-", "", r) # remove version prefix (SQLite)
            r = gsub("OpenSSL_", "", r) # remove name prefix (OpenSSL)
            r = gsub("\\.", "_", r) # replace dots by underscores
            return(r)
        },

        ## * * path construction -------------------------------------------

        subfolder.configurations = "configurations",
        subfolder.results = "results",

        #' Construct and return the path to the configuration folder of Codeface.
        #'
        #' @param data the path to the codeface-data folder
        #' @param selection.process the selection process of the current study ('threemonth', 'releases')
        #'
        #' @return the path to the configuration folder
        get.configurations.folder = function(data, selection.process) {
            return(file.path(data, private$subfolder.configurations, selection.process))

        },

        #' Construct and return the path to a Codeface configuration.
        #'
        #' @param data the path to the codeface-data folder
        #' @param selection.process the selection process of the current study ('threemonth', 'releases')
        #' @param casestudy the current casestudy
        #' @param tagging the current tagging ('feature', 'proximity')
        #'
        #' @return the path to the configuration
        construct.conf.path = function(data, selection.process, casestudy, tagging) {
            ## construct the base name of the configuration
            conf.basename = paste(casestudy, "_", tagging, ".conf", sep = "")
            ## construct complete path
            conf.file = file.path(private$get.configurations.folder(data, selection.process), conf.basename)
            ## return path to config file
            return(conf.file)
        },

        #' Construct and return the path to the results folder of Codeface.
        #'
        #' @param data the path to the codeface-data folder
        #' @param selection.process the selection process of the current study ('threemonth', 'releases')
        #' @param casestudy the current casestudy
        #' @param suffix the suffix of the casestudy's results folder
        #' @param subfolder an optional subfolder [default: NULL]
        #'
        #' @return the path to the results folder
        #'         (i.e., "{data}/{selection.process}/{casestudy}_{suffix}[/{subfolder}]")
        get.results.folder = function(data, selection.process, casestudy, suffix, subfolder = NULL) {
            path = file.path(data, private$subfolder.results, selection.process, paste(casestudy, suffix, sep = "_"))
            if (!is.null(subfolder)) {
                path = file.path(path, subfolder)
            }
            return(path)
        }

    ),

    ## * public ------------------------------------------------------------

    public = list(

        #' Constructor of the class.
        #'
        #' @param data the path to the codeface-data folder
        #' @param selection.process the selection process of the current study ('threemonth', 'releases')
        #' @param casestudy the current casestudy
        #' @param artifact the artifact to study (one of \code{feature}, \code{function}, \code{file},
        #'                 and \code{featureexpression}) [default: "feature"]
        initialize = function(data, selection.process, casestudy, artifact = c("feature", "file",
                                                                               "function", "featureexpression")) {

            logging::loginfo("Construct project configuration: starting.")

            ## call super constructor
            super$initialize()

            ## verify arguments using match.arg
            artifact = match.arg(artifact)
            ## verify arguments
            private$data = verify.argument.for.parameter(data, "character", "ProjectConf$new")
            private$selection.process = verify.argument.for.parameter(selection.process, "character", "ProjectConf$new")
            private$casestudy = verify.argument.for.parameter(casestudy, "character", "ProjectConf$new")
            private$artifact = verify.argument.for.parameter(artifact, "character", "ProjectConf$new")

            ## convert artifact to tagging
            tagging = ARTIFACT.TO.TAGGING[[ artifact ]]
            if (is.null(tagging)) {
                logging::logerror("Artifact '%s' cannot be converted to a proper Codeface tagging! Stopping...", artifact)
                stop("Stopped due to wrong configuration parameters!")
            }
            ## construct file name for configuration
            conf.file = private$construct.conf.path(data, selection.process, casestudy, tagging)

            ## load case-study configuration from given file
            logging::loginfo("Attempting to load configuration file: %s", conf.file)
            conf = yaml::yaml.load_file(conf.file)

            ## store basic information
            conf$selection.process = selection.process
            conf$casestudy = casestudy

            ## store artifact in configuration
            conf$artifact = artifact
            conf$artifact.short = ARTIFACT.TO.ABBREVIATION[[ conf$artifact ]]
            conf$artifact.codeface = ARTIFACT.CODEFACE[[ conf$artifact ]]
            ## store path to actual Codeface data
            conf$datapath = private$get.results.folder(data, selection.process, casestudy, tagging, subfolder = tagging)
            ## store path to call graphs
            conf$datapath.callgraph = private$get.results.folder(data, selection.process, casestudy, "callgraphs")
            ## store path to synchronicity data
            conf$datapath.synchronicity = private$get.results.folder(data, selection.process, casestudy, "synchronicity")
            ## store path to PaStA data
            conf$datapath.pasta = private$get.results.folder(data, selection.process, casestudy, "pasta")
            ## store path to commit interaction data
            conf$datapath.commit.interaction =
                private$get.results.folder(data, selection.process, casestudy, tagging, subfolder = tagging)
            ## store path to gender data
            conf$datapath.gender = private$get.results.folder(data, selection.process, casestudy, "gender")
            ## store path to issue data
            conf$datapath.issues = private$get.results.folder(data, selection.process, casestudy, tagging, subfolder = tagging)

            ## READ REVISIONS META-DATA

            ## read revisions file
            revisions.file = file.path(conf$datapath, "revisions.list")
            revisions.df = try(read.table(revisions.file, header = FALSE, sep = ";", strip.white = TRUE,
                                           encoding = "UTF-8"), silent = TRUE)
            ## break if the list of revisions is empty or any other error occurs
            if (inherits(revisions.df, "try-error")) {
                logging::logerror("There are no revisions available for the current casestudy.")
                logging::logerror("Attempted to load following file: %s", revisions.file)
                stop("Stopped due to missing revisions.")
            }
            ## convert columns accordingly
            revisions.cols = c(revision = "as.character", date = "get.date.from.string")
            for (i in 1:ncol(revisions.df)) {
                revisions.df[i] = do.call(base::c, lapply(revisions.df[[i]], revisions.cols[i]))
                colnames(revisions.df)[i] = names(revisions.cols)[i]
            }
            revisions = revisions.df[["revision"]]
            revisions.dates = revisions.df[["date"]]
            if (!is.null(revisions.dates)) names(revisions.dates) = revisions
            conf[["revisions"]] = NULL

            ## change structure of values (i.e., insert 'default' sublists)
            conf = lapply(conf, function(entry) {
                return(list(value = entry, updatable = FALSE))
            })

            ## SAVE FULL CONFIGURATION OBJECT
            private$attributes = c(conf, private$attributes)

            ## construct and save revisions and ranges
            ## (this has to be done after storing conf due to the needed access to the conf object)
            self$set.revisions(revisions, revisions.dates)

            # ## logging
            # self$print(allowed = TRUE)

            logging::loginfo("Construct project configuration: finished.")
        },

        ## * * helper methods ----------------------------------------------

        #' Get the corresponding callgraph revision for the given range.
        #'
        #' @param range the range for the callgraph revisions
        #'
        #' @return the callgraph revisions
        get.callgraph.revision.from.range = function(range) {
            idx = which(self$get.value("ranges") == range)
            rev = self$get.value("revisions.callgraph")[idx + 1]
            return(rev)
        },

        ## * * updating revisions and splitting information ----------------

        #' Set the revisions and ranges for the study.
        #'
        #' @param revisions the revisions of the study
        #' @param revisions.dates the revision dates of the study
        #' @param sliding.window whether sliding window splitting is enabled or not [default: FALSE]
        set.revisions = function(revisions, revisions.dates, sliding.window = FALSE) {
            ## construct revisions for call-graph data
            revisions.callgraph = private$postprocess.revision.list.for.callgraph.data(revisions)

            ## construct ranges
            ranges = construct.ranges(revisions, sliding.window = sliding.window)

            ## assemble revision data
            rev.data = list(
                revisions = revisions,
                revisions.dates = revisions.dates,
                revisions.callgraph = revisions.callgraph,
                ranges = ranges,
                ranges.paths = generate.range.directory.names(ranges),
                ranges.callgraph = construct.ranges(revisions.callgraph, sliding.window = sliding.window)
            )
            ## change structure of values (i.e., insert 'default' sublists and set 'updatable' value)
            rev.data = lapply(rev.data, function(entry) {
                return(list(value = entry, updatable = FALSE))
            })

            ## insert new values (update if needed)
            for (name in names(rev.data)) {
                private[["attributes"]][[name]] = rev.data[[name]]
            }
        },

        #' Update the information on revisions and ranges regarding splitting.
        #'
        #' @param type either "time-based" or "activity-based", depending on splitting function
        #' @param length the string given to time-based splitting (e.g., "3 months") or the activity
        #'               amount given to acitivity-based splitting
        #' @param basis the data used as basis for splitting (either "commits", "mails", or "issues")
        #' @param sliding.window whether sliding window splitting is enabled or not
        #' @param revisions the revisions of the study
        #' @param revisions.dates the revision dates of the study
        set.splitting.info = function(type, length, basis, sliding.window, revisions, revisions.dates) {
            ## assemble splitting information
            split.info = list(
                ## basic slpitting information
                split.type = type,
                split.length = length,
                split.basis = basis,
                split.sliding.window = sliding.window,
                ## splitting information on ranges
                split.revisions = revisions,
                split.revisions.dates = revisions.dates,
                split.ranges = construct.ranges(revisions, sliding.window = sliding.window)

            )
            ## change structure of values (i.e., insert 'default' sublists and set 'updatable' value)
            split.info = lapply(split.info, function(entry) {
                return(list(value = entry, updatable = FALSE))
            })

            ## insert new values (update if needed)
            for (name in names(split.info)) {
                private[["attributes"]][[name]] = split.info[[name]]
            }
        }

    )
)


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## NetworkConf -------------------------------------------------------------

#' The class \code{NetworkConf} is a subclass of \code{Conf} and provides
#' convenient handling of configuration attributes related to network
#' construction with the class \code{NetworkBuilder}.
#'
#' @seealso Conf
#' @seealso NetworkBuilder
NetworkConf = R6::R6Class("NetworkConf", inherit = Conf,

    ## * private -----------------------------------------------------------

    private = list(

        ## * * attributes -----------------------------------------------------

        attributes = list(
            author.relation = list(
                default = "mail",
                type = "character",
                allowed = c("mail", "cochange", "issue", "commit.interaction"),
                allowed.number = Inf
            ),
            author.directed = list(
                default = FALSE,
                type = "logical",
                allowed = c(TRUE, FALSE),
                allowed.number = 1
            ),
            author.respect.temporal.order = list(
              default = NA, # default value will be determined from the 'author.directed' parameter
              type = "logical",
              allowed = c(TRUE, FALSE, NA),
              allowed.number = 1
            ),
            author.all.authors = list(
                default = FALSE,
                type = "logical",
                allowed = c(TRUE, FALSE),
                allowed.number = 1
            ),
            author.only.committers = list(
                ## FIXME rename to something like "author.only.authors.also.in.the.data.when.artifact.relation.is.used
                default = FALSE,
                type = "logical",
                allowed = c(TRUE, FALSE),
                allowed.number = 1
            ),
            artifact.relation = list(
                default = "cochange",
                type = "character",
                allowed = c("cochange", "callgraph", "mail", "issue", "commit.interaction"),
                allowed.number = Inf
            ),
            artifact.directed = list(
                default = FALSE,
                type = "logical",
                allowed = c(TRUE, FALSE),
                allowed.number = 1
            ),
            edges.for.base.artifacts = list(
                default = TRUE,
                type = "logical",
                allowed = c(TRUE, FALSE),
                allowed.number = 1
            ),
            edge.attributes = list(
                default = c(
                    "date", "artifact.type", # general
                    "message.id", "thread", # mail data
                    "hash", "file", "artifact", # commit data
                    "issue.id", "event.name" # issue data
                ),
                type = "character",
                allowed = c(
                    # the date
                    "date", "date.offset",
                    # the artifact type indicating the edge
                    "artifact.type",
                    # author information
                    "author.name", "author.email",
                    # committer information
                    "committer.date", "committer.name", "committer.email",
                    # e-mail information
                    "message.id", "thread", "subject",
                    ## commit information
                    "hash", "file", "artifact", "changed.files", "added.lines",
                    "deleted.lines", "diff.size", "artifact.diff.size", "synchronicity",
                    # PaStA information
                    "pasta",
                    # issue information
                    "issue.id", "issue.state", "creation.date", "closing.date", "is.pull.request",
                    "author.name", "author.mail", "event.date", "event.name"
                ),
                allowed.number = Inf
            ),
            simplify = list(
                default = FALSE,
                type = "logical",
                allowed = c(TRUE, FALSE),
                allowed.number = 1
            ),
            simplify.multiple.relations = list(
               default = FALSE,
               type = "logical",
               allowed = c(TRUE, FALSE),
               allowed.number = 1
           ),
            skip.threshold = list(
                default = Inf,
                type = "numeric",
                allowed = Inf,
                allowed.number = 1
            ),
            unify.date.ranges = list(
                default = FALSE,
                type = "logical",
                allowed = c(TRUE, FALSE),
                allowed.number = 1
            )
        )

    ),

    ## * public ------------------------------------------------------------

    public = list(

        #' The constructor, automatically checking the default values.
        initialize = function() {
            logging::loginfo("Construct network configuration: starting.")

            ## call super constructor
            super$initialize()

            logging::loginfo("Construct network configuration: finished.")
        },

        #' Update the attributes of the class with the new values given in the
        #' 'updated.values' list.
        #'
        #' @param updated.values the new values for the attributes to be updated [default: list()]
        update.values = function(updated.values = list()) {
            super$update.values(updated.values = updated.values)

            ## 1) "date" and "artifact.type" always as edge attribute
            name = "edge.attributes"
            private[["attributes"]][[name]][["value"]] = unique(c(
                "date",
                "artifact.type",
                self$get.value(name)
            ))

            ## return invisible
            invisible()
        },

        #' Clears the edge attributes by setting the attribute to just the mandatory
        #' 'date' value. This can be used to get rid of all the default values at once.
        clear.edge.attributes = function() {
            logging::loginfo("Clearing edge.attributes.")
            self$update.values(list(edge.attributes = c("date")))
        }

    )
)


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Helper functions --------------------------------------------------------

#' Constructs a string representing a configuration (i.e., a potentially nested list).
#'
#' @param conf the configuration list to represent as string
#' @param title the title line right before the constructed string [default: deparse(substitute(conf))]
#'
#' @return a string representing the status of \code{conf}, including newline characters and
#'         pretty-printed list style
get.configuration.string = function(conf, title = deparse(substitute(conf))) {
    fill = "↳ "
    front = "- "
    indentation.item = "  "

    construct.configuration.string = function(struct, title, depth = 0) {
        ## construct the indentation
        indentation = if (depth > 0) rep(indentation.item, depth - 1) else ""

        output = c()

        len = length(struct)

        ## add current title
        if (!is.null(title)) {
            if (depth > 0) output = c(output, indentation)
            output = c(output, paste0(front, title))

            if (mode(struct) == "list") {
                updatable = struct[["updatable"]]
                if (!is.null(updatable)) {
                    struct[["updatable"]] = NULL
                    if (!updatable) {
                        output = c(output, " [final]")
                        len = len - 1
                    }
                }
                output = c(output, "\n")
            }
        }

        if (is.atomic(struct) && len > 0) {
            if (len == 1) {
                field = paste0(" = ", paste(struct, collapse = ", "), "\n")
            } else if (len > 7) {
                entries = paste0(indentation, indentation.item, indentation.item, struct)
                field = paste0(
                    " = [\n",
                    paste(entries, collapse = ", \n"),
                    "\n",
                    if (depth > 0) indentation,
                    if (depth > 0) indentation.item,
                    "]\n"
                )
            } else {
                field = paste0(" = [", paste(struct, collapse = ", "), "]\n")
            }
            output = c(output, field)
        }

        if (mode(struct) == "list" && len > 0) {
            structnames = names(struct)
            if (is.null(structnames)) structnames = rep("", len)

            noname = structnames == ""
            structnames[noname] = sprintf("[[%s]]", seq_len(len)[noname])

            for (i in seq_len(len)) {
                item = struct[[i]]
                label = structnames[i]
                entry = construct.configuration.string(
                    struct = item,
                    title = label,
                    depth = depth + 1
                )
                output = c(output, entry)
            }
        }

        output = paste(output, collapse = "")
        return(output)
    }

    return(construct.configuration.string(conf, title))
}

#' Generate the directory names for Codeface ranges. That is, commit hashes forming the range are shortened
#' to a length of six characters and the range name is prepended by a consecutive range number.
#'
#' The function is partly adapted from
#' https://github.com/siemens/codeface@57bfbab58f75a91effb431842d01c76627071134 and
#' https://github.com/siemens/codeface/commit/cd7b68c65ff7ae113e6c75275ce3798004ce7b09.
#'
#' @param ranges the (revision) ranges of the project
generate.range.directory.names = function(ranges) {
    range.numbers = seq_along(ranges)

    directory.names = mapply(range.numbers, ranges, SIMPLIFY = FALSE, FUN = function(range.number, range) {
        revisions = strsplit(range, "-")[[1]]

        if (nchar(revisions[1]) == 40) {
            revisions[1] = substr(revisions[1], 0, 6)
            revisions[2] = substr(revisions[2], 0, 6)
        }
	return(paste0(formatC(range.number, width = 3, flag = "0"), "--", revisions[1], "-", revisions[2]))
    })

    names(directory.names) = ranges
    return(directory.names)
}
