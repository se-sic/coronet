## (c) Claus Hunsen, 2016, 2017
## hunsen@fim.uni-passau.de
## (c) Raphael Nömmer, 2017
## noemmer@fim.uni-passau.de
## (c) Christian Hechtl, 2017
## hechtl@fim.uni-passau.de


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
            self$update.values(current.values, stop.on.error = TRUE)
        },

        #' Check whether the given 'value' is the correct datatype
        #' for the attribute 'name'.
        #'
        #' @param value the new value for the attribute
        #' @param name the name of the attribute
        #'
        #' @return a named list of logical values, named:
        #'         - existing,
        #'         - type,
        #'         - allowed, and
        #'         - allowed.number.
        check.value = function(value, name) {
            browser(expr = name == "revisions")
            if (!exists(name, where = private[["attributes"]])) {
                result = c(existing = FALSE)
            } else {
                ## check all other properties
                attribute = private[["attributes"]][[name]]
                ## if non-updatable field, return early
                if (!is.null(attribute[["updatable"]]) && !attribute[["updatable"]]) {
                    result = c(existing = TRUE, updatable = FALSE)
                } else {
                    result = c(
                        existing = TRUE,
                        updatable = TRUE,
                        type = class(value) %in% attribute[["type"]],
                        allowed =
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
            private$check.values()
        },

        ## * * printing ----------------------------------------------------

        #' Get configuration list as string.
        #'
        #' @param allowed Indicator whether to return also information on allowed values
        #'
        #' @return the configuration list as string
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
        #' @param allowed Indicator whether to print information on allowed values
        print = function(allowed = FALSE) {
            logging::loginfo("Network configuration:\n%s", self$get.conf.as.string(allowed))
        },

        ## * * updating ----------------------------------------------------

        #' Update the attributes of the class with the new value for the given entry.
        #'
        #' @param entry the entry name for the value
        #' @param value the new value
        #' @param error call stop() on an error? [default: FALSE]
        update.value = function(entry, value, stop.on.error = FALSE) {
            ## construct list for updating
            updating = list(value)
            names(updating) = entry
            ## update value
            self$update.values(updating)
        },

        #' Update the attributes of the class with the new values given in the
        #' 'updated.values' list.
        #'
        #' @param updated.values the new values for the attributes to be updated
        #' @param error call stop() on an error? [default: FALSE]
        update.values = function(updated.values = list(), stop.on.error = FALSE) {
            ## determine the function executed on an error
            error.function = ifelse(stop.on.error, stop, logging::logwarn)

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
                            "Updating network-configuration attribute '%s' failed:",
                            "A network-configuraton attribute with this name does not exist."
                        )
                        error.function(sprintf(message, name))

                    } else if (!check[["updatable"]]) {

                        message = paste(
                            "Updating network-configuration attribute '%s' failed:",
                            "The value is not updatable!"
                        )
                        error.function(message, name)

                    } else {
                        message = paste0(
                            "Updating network-configuration attribute '%s' failed.",
                            if (!stop.on.error) " The failure is ignored!\n",
                            # "Current value: %s\n",
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
                    private[["attributes"]][[name]][["value"]] = updated.values[[name]]
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
## NetworkConf -------------------------------------------------------------

NetworkConf = R6::R6Class("NetworkConf", inherit = Conf,

    ## * private -----------------------------------------------------------

    private = list(

        ## * * attributes -----------------------------------------------------

        attributes = list(
            author.relation = list(
                default = "mail",
                type = "character",
                allowed = c("mail", "cochange", "issue"),
                allowed.number = 1
            ),
            author.directed = list(
                default = FALSE,
                type = "logical",
                allowed = c(TRUE, FALSE),
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
                allowed = c("cochange", "callgraph", "mail", "issue"),
                allowed.number = 1
            ),
            artifact.directed = list(
                default = FALSE,
                type = "logical",
                allowed = c(TRUE, FALSE),
                allowed.number = 1
            ),
            edge.attributes = list(
                default = c(
                    "date", # general
                    "message.id", "thread", # mail data
                    "hash", "file", "artifact.type", "artifact", # commit data
                    "issue.id", "event.name" # issue data
                ),
                type = "character",
                allowed = c(
                    # the date
                    "date",
                    # author information
                    "author.name", "author.email",
                    # e-mail information
                    "message.id", "thread", "subject",
                    ## commit information
                    "hash", "file", "artifact.type", "artifact", "changed.files", "added.lines",
                    "deleted.lines", "diff.size", "artifact.diff.size", "synchronicity",
                    # pasta information
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
            # private$check.values()
        },

        #' Update the attributes of the class with the new values given in the
        #' 'updated.values' list.
        #'
        #' @param updated.values the new values for the attributes to be updated
        #' @param error call stop() on an error? [default: FALSE]
        update.values = function(updated.values = list(), stop.on.error = FALSE) {
            super$update.values(updated.values = updated.values, stop.on.error = stop.on.error)

            ## 1) "date" always as edge attribute
            name = "edge.attributes"
            private[["attributes"]][[name]][["value"]] = unique(c(self$get.value(name), "date"))

            ## return invisible
            invisible()
        }

    )
)


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## ProjectConf -------------------------------------------------------------

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
            artifact.filter.base = list(
                default = TRUE,
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
            )
        ),

        ## * * revisions and ranges ----------------------------------------

        #' Change the revision names to a equal name standard.
        #'
        #' @param ranges the list of ranges to be postprocessed
        #'
        #' @return the postprocessed ranges
        postprocess.revision.list = function(ranges) {
            # remove names ,e.g. "version", from release cycle names
            casestudy = private$casestudy
            to.remove = c(
                "version-", "v-","version_", "v_","version", "v",
                paste0(casestudy, "-"), paste0(casestudy,"-"),
                paste0(casestudy, "_"), paste0(casestudy,"_"),
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
        #' @param subfolder an optional subfolder
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
        #' @param artifact the artifact to study ('feature','function','file')
        initialize = function(data, selection.process, casestudy, artifact = "feature") {
            super$initialize()

            if (!missing(data) && is.character(data)) {
                private$data <- data
            }
            if (!missing(selection.process) && is.character(selection.process)) {
                private$selection.process <- selection.process
            }
            if (!missing(casestudy) && is.character(casestudy)) {
                private$casestudy <- casestudy
            }
            if (!missing(artifact) && is.character(artifact)) {
                private$artifact <- artifact
            }

            logging::loginfo("Construct configuration: starting.")

            ## convert artifact to tagging
            tagging = ARTIFACT.TO.TAGGING[[ artifact ]]
            if (is.null(tagging)) {
                logging::logerror("Artifact '%s' cannot be converted to a proper Codeface tagging! Stopping...", artifact)
                stop("Stopped due to wrong configuration parameters!")
            }
            ## construct file name for configuration
            conf.file = private$construct.conf.path(data, selection.process, casestudy, tagging)

            ## load case-study confuration from given file
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
            ## store path to pasta data
            conf$datapath.pasta = private$get.results.folder(data, selection.process, casestudy, "pasta")
            ## store path to issue data
            conf$datapath.issues = private$get.results.folder(data, selection.process, casestudy, tagging, subfolder = tagging)

            ## READ REVISIONS META-DATA

            ## read revisions file
            revisions.file = file.path(conf$datapath, "revisions.list")
            revisions.df <- try(read.table(revisions.file, header = FALSE, sep = ";", strip.white = TRUE,
                                           encoding = "UTF-8"), silent = TRUE)
            ## break if the list of revisions is empty or any other error occurs
            if (inherits(revisions.df, 'try-error')) {
                logging::logerror("There are no revisions available for the current casestudy.")
                logging::logerror("Attempted to load following file: %s", revisions.file)
                stop("Stopped due to missing revisions.")
            }
            ## convert columns accordingly
            revisions.cols = c(revision = "as.character", date = "as.POSIXct")
            for (i in 1:ncol(revisions.df)) {
                revisions.df[i] = do.call(c, lapply(revisions.df[[i]], revisions.cols[i]))
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

            logging::loginfo("Construct configuration: finished.")
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
        #' @param sliding.window whether sliding window splitting is enabled or not
        #'                       default: 'FALSE'
        set.revisions = function(revisions, revisions.dates, sliding.window = FALSE) {
            ## construct revisions for call-graph data
            revisions.callgraph = private$postprocess.revision.list.for.callgraph.data(revisions)

            ## assemble revision data
            rev.data = list(
                revisions = revisions,
                revisions.dates = revisions.dates,
                revisions.callgraph = revisions.callgraph,
                ranges = construct.ranges(revisions, sliding.window = sliding.window),
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
        #' @param sliding.window whether sliding window splitting is enabled or not [default: FALSE]
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
## Helper functions --------------------------------------------------------

#' Construct the range strings.
#'
#' @param revs the revisions
#' @param sliding.window whether sliding window splitting is enabled or not
#'                       default: 'FALSE'
#'
#' @return the ranges as strings
construct.ranges = function(revs, sliding.window = FALSE) {
    ## setting offset to construct ranges, i.e.,
    ## combine each $offset revisions
    offset = 1

    ## with sliding window, we combine each second revision
    if (sliding.window)
        offset = 2

    ## extract sequences of revisions
    seq1 = revs[ 1:(length(revs) - offset) ]
    seq2 = revs[ (offset + 1):length(revs) ]

    ## construct ranges
    ranges = paste(seq1, seq2, sep = "-")

    return(ranges)
}

#' Constructs a string representing a configuration (i.e., a potentially nested list).
#'
#' @param conf the configuration list to represent as string
#' @param title the title line right before the constructed string
#'
#' @return a string representing the status of \code{conf}, including newline characters and
#'         pretty-printed list style
get.configuration.string = function(conf, title = deparse(substitute(conf))) {
    fill="↳ "
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

        if(is.atomic(struct) && len > 0) {
            if (len == 1) {
                field = paste0(" = ", paste(struct, collapse = ", "), "\n")
            } else if (len > 7) {
                # browser()
                entries = paste0(indentation, indentation.item, indentation.item, struct)
                field = paste0(
                    " = [\n",
                    paste(entries, collapse = ",\n"),
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
            if(is.null(structnames)) structnames = rep("", len)

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
