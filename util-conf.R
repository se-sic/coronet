## (c) Claus Hunsen, 2016, 2017
## hunsen@fim.uni-passau.de
## (c) Raphael Nömmer, 2017
## noemmer@fim.uni-passau.de
## (c) Christian Hechtl, 2017
## hechtl@fim.uni-passau.de


## libraries
requireNamespace("R6") # for R6 classes
requireNamespace("yaml") # for Codeface configuration files
requireNamespace("logging")


## / / / / / / / / / / / / / /
## Constants
##

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


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## NetworkConf ####
##
## Represents the network configurations

NetworkConf = R6::R6Class("NetworkConf",
    ## private members ####
    private = list(
        #Variables with default values
        #Values can be changed using update.values method
        author.relation = list(value = "mail",
                               type = "character"), # mail, cochange, issue
        author.directed = list(value = FALSE,
                               type = "logical"),
        author.all.authors = list(value = FALSE,
                                  type = "logical"),
        author.only.committers = list(value = FALSE,
                                      type = "logical"),
        artifact.relation = list(value = "cochange",
                                 type = "character"),
        artifact.directed = list(value = FALSE,
                                 type = "logical"),
        edge.attributes = list(value = c(
                                   "date", # general
                                   "message.id", "thread", # mail data
                                   "hash", "file", "artifact.type", "artifact" # commit data
                               ),
                               type = "character"),
        simplify = list(value = FALSE,
                        type = "logical"),
        skip.threshold = list(value = Inf,
                              type = "numeric"),

        #' Check whether the given 'value' is the correct datatype
        #' for the attribute 'name'.
        #'
        #' @param value the new value for the attribute
        #' @param name the name of the attribute
        #'
        #' @return 'TRUE' if the datatype is correct and
        #'         'FALSE' otherwise
        check.value = function(value, name) {
            return(exists(name, where = private) &&
                    (class(value) == (private[[name]][["type"]]))
            )
        }
    ),

    ## public members ####
    public = list(

        #' Print the private variables of the class.
        print = function() {
            logging::loginfo("Printing state of network configuration.")
            for (name in names(private)) {
                if ((class(private[[name]]) != "function")) {
                   logging::loginfo("%s: %s", name, private[[name]])
                }
            }
        },

        #' Update the attributes of the class with the new values given in the
        #' 'updated.values' list.
        #'
        #' @param updated.values the new values for the attributes to be updated
        update.values = function(updated.values = list()) {
            for (name in names(updated.values)) {
                if (private$check.value(value = updated.values[[name]], name = name)) {
                    if(name == "edge.attributes" && !("date" %in% updated.values[[name]])) {
                      private[[name]][["value"]] = c(updated.values[[name]], "date")
                    } else {
                      private[[name]][["value"]] = updated.values[[name]]
                    }
                } else {
                  logging::logwarn("Name or type of '%s' is incorrect. Type given is: '%s'.", name, class(updated.values[[name]]))
                    if(exists(name, where = private)) {
                        logging::logwarn("Expected type is: '%s'.", private[[name]][["type"]])
                    }
                }
            }
        },

        #' Get the variable whichs name is given by 'var.name'.
        #'
        #' @param var.name the name of the variable to be returned
        #'
        #' @return the variable
        get.variable = function(var.name) {
            return(private[[var.name]][["value"]])
        }
    )
)

## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## ProjectConf ####
##
## Represents the Project configuration

ProjectConf = R6::R6Class("ProjectConf",

    ## private members ####
    private = list(

        data = NULL, # character
        selection.process = NULL, # character
        casestudy = NULL, # character
        artifact = NULL, # character
        conf = NULL, # list

        synchronicity = FALSE,
        synchronicity.time.window = 5,
        artifact.filter.base = TRUE,
        pasta = FALSE,

        ## / / / / / / / / / / / / / /
        ## Revisions and ranges
        ##

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


        ## / / / / / / / / / / / / / /
        ## Path construction
        ##

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

        #' Construct and return the path to the results folder of Codeface.
        #'
        #' @param data the path to the codeface-data folder
        #' @param selection.process the selection process of the current study ('threemonth', 'releases')
        #' @param project the current project
        #' @param tagging the current tagging ('feature', 'proximity')
        #'
        #' @return the path to the results folder
        get.results.folder = function(data, selection.process, project, tagging) {
            return(file.path(data, private$subfolder.results, selection.process, project, tagging))
        },

        #' Construct and return the path to the callgraph folder of Codeface.
        #'
        #' @param data the path to the codeface-data folder
        #' @param selection.process the selection process of the current study ('threemonth', 'releases')
        #' @param casestudy the current casestudy
        #'
        #' @return the path to the callgraph folder
        get.callgraph.folder = function(data, selection.process, casestudy) {
            return(file.path(data, private$subfolder.results, selection.process, paste(casestudy, "callgraphs", sep = "_")))
        },

        #' Construct and return the path to the synchronicity folder of Codeface.
        #'
        #' @param data the path to the codeface-data folder
        #' @param selection.process the selection process of the current study ('threemonth', 'releases')
        #' @param casestudy the current casestudy
        #'
        #' @return the path to the synchronicity folder
        get.synchronicity.folder = function(data, selection.process, casestudy) {
            return(file.path(data, private$subfolder.results, selection.process, paste(casestudy, "synchronicity", sep = "_")))
        },

        #' Construct and return the path to the pasta folder of Codeface.
        #'
        #' @param data the path to the codeface-data folder
        #' @param selection.process the selection process of the current study ('threemonth', 'releases')
        #' @param casestudy the current casestudy
        #'
        #' @return the path to the pasta folder
        get.pasta.folder = function(data, selection.process, casestudy) {
            return(file.path(data, private$subfolder.results, selection.process, paste(casestudy, "pasta", sep = "_")))
        },

        #' Construct and return the path to the issues folder of Codeface.
        #'
        #' @param data the path to the codeface-data folder
        #' @param selection.process the selection process of the current study ('threemonth', 'releases')
        #' @param casestudy the current casestudy
        #'
        #' @return the path to the issues folder
        get.issues.folder = function(data, selection.process, casestudy) {
            return(file.path(data, private$subfolder.results, selection.process, paste(casestudy, "issues", sep = "_")))
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

        ## / / / / / / / / / / / / / /
        ## Configuration loading
        ##

        #' Load a Codeface configuration.
        #'
        #' @param data the path to the codeface-data folder
        #' @param selection.process the selection process of the current study ('threemonth', 'releases')
        #' @param casestudy the current casestudy
        #' @param artifact the artifact to study ('feature','function','file'), is mapped to the
        #'                 tagging parameter, used by codeface, automatically
        load.configuration = function(data, selection.process, casestudy, artifact = "feature") {

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

            ## store artifact in configuration
            conf$artifact = artifact
            conf$artifact.short = ARTIFACT.TO.ABBREVIATION[[ conf$artifact ]]
            conf$artifact.codeface = ARTIFACT.CODEFACE[[ conf$artifact ]]

            ## store path to actual Codeface data
            conf$datapath = private$get.results.folder(data, selection.process, conf[["project"]], tagging)
            ## store path to call graphs
            conf$datapath.callgraph = private$get.callgraph.folder(data, selection.process, casestudy)
            ## store path to synchronicity data
            conf$datapath.synchronicity = private$get.synchronicity.folder(data, selection.process, casestudy)
            ## store path to pasta data
            conf$datapath.pasta = private$get.pasta.folder(data, selection.process, casestudy)
            ## store path to issue data
            conf$datapath.issues = private$get.issues.folder(data, selection.process, conf[["project"]])

            ## READ REVISIONS META-DATA

            ## read revisions file
            revisions.file = file.path(conf$datapath, "revisions.list")
            revisions.df <- try(read.table(revisions.file, header = FALSE, sep = ";", strip.white = TRUE,
                                           fileEncoding = "latin1", encoding = "utf8"), silent = TRUE)
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

            ## SAVE FULL CONFIGURATION OBJECT
            private$conf = conf

            ## construct and save revisions and ranges
            ## (this has to be done after storing conf due to the needed access to the conf object)
            self$set.revisions(revisions, revisions.dates)

            ## logging
            logging::logdebug("Configuration:\n%s", self$get.conf.as.string())
        },

        #' Get an entry of the configuration list by its key
        #'
        #' @param key the key of the entry to be returned
        #'
        #' @return the specified entry of the configuration list
        get.conf.entry = function(key) {
            return(private$conf[[key]])
        }

    ),

    ## public members ####
    public = list(

        #' Constructor of the class.
        #'
        #' @param data the path to the codeface-data folder
        #' @param selection.process the selection process of the current study ('threemonth', 'releases')
        #' @param casestudy the current casestudy
        #' @param artifact the artifact to study ('feature','function','file')
        initialize = function(data, selection.process, casestudy, artifact = "feature") {
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
            private$load.configuration(data, selection.process, casestudy, artifact)
            logging::loginfo("Construct configuration: finished.")
        },


        #' Get configuration list.
        #'
        #' @return the configuration list
        get.conf = function() {
            return(private$conf)
        },

        #' Get configuration list as string.
        #'
        #' @return the configuration list as string
        get.conf.as.string = function() {
            return(yaml::as.yaml(private$conf))
        },

        ## CONFIGURATION ENTRIES

        #' Get an entry of the configuration list. Calls 'get.conf.entry'.
        #'
        #' @param entry.name name of the entry to be returned
        #'
        #' @return the entry specified
        get.entry = function(entry.name) {
            return(private$get.conf.entry(entry.name))
        },

        #' Get the corresponding callgraph revision for the given range.
        #'
        #' @param range the range for the callgraph revisions
        #'
        #' @return the callgraph revisions
        get.callgraph.revision.from.range = function(range) {
            idx = which(self$get.entry("ranges") == range)
            rev = self$get.entry("revisions.callgraph")[idx + 1]
            return(rev)
        },

        #' Get the 'synchronicity' variable.
        #'
        #' @return the 'synchronicity' variable
        get.synchronicity = function() {
            return(private$synchronicity)
        },

        #' Set the 'synchronicity' variable to the given new value.
        #'
        #' @param synchronicity the new value for the variable
        set.synchronicity = function(synchronicity) {
            if(class(synchronicity) == "logical")
                private$synchronicity = synchronicity
            else
                logging::logwarn("Wrong data type given in set.synchronicity.")
        },

        #' Get the 'synchronicity.time.window' variable.
        #'
        #' @return the 'synchronicity.time.window' variable
        get.synchronicity.time.window = function() {
          return(private$synchronicity.time.window)
        },

        #' Set the 'synchronicity.time.window' variable to the given new value.
        #'
        #' @param synchronicity.time.window the new value for the variable
        set.synchronicity.time.window = function(synchronicity.time.window) {
          if(class(synchronicity.time.window) == "numeric")
            private$synchronicity.time.window = synchronicity.time.window
          else
            logging::logwarn("Wrong data type given in set.synchronicity.time.window.")
        },

        #' Get the 'artifact.filter.base' variable.
        #'
        #' @return the 'artifact.filter.base' variable
        get.artifact.filter.base = function() {
          return(private$artifact.filter.base)
        },

        #' Set the 'artifact.filter.base' variable to the given new value.
        #'
        #' @param artifact.filter.base the new value for the variable
        set.artifact.filter.base = function(artifact.filter.base) {
          if(class(artifact.filter.base) == "logical")
            private$artifact.filter.base = artifact.filter.base
          else
            logging::logwarn("Wrong data type given in set.artifact.filter.base")
        },

        #' Get the 'pasta' variable.
        #'
        #' @return the 'pasta' variable
        get.pasta = function() {
          return(private$pasta)
        },

        #' Set the 'pasta' variable to the given new value.
        #'
        #' @param pasta the new value for the variable
        set.pasta = function(pasta) {
          if(class(pasta) == "logical")
            private$pasta = pasta
          else
            logging::logwarn("Wrong data type given in set.pasta.")
        },

        ## UPDATING CONFIGURATION ENTRIES

        #' Set the revisions and ranges for the study.
        #'
        #' @param revisions the revisions of the study
        #' @param revisions.dates the revision dates of the study
        #' @param sliding.window whether sliding window splitting is enabled or not
        #'                       default: 'FALSE'
        set.revisions = function(revisions, revisions.dates, sliding.window = FALSE) {
            ## store revision data
            private$conf$revisions = revisions
            private$conf$revisions.dates = revisions.dates

            ## call-graph revisions (do a postprocessing for list of revisions)
            private$conf$revisions.callgraph = private$postprocess.revision.list.for.callgraph.data(private$conf$revisions)

            ## compute revision ranges
            private$conf$ranges = construct.ranges(private$conf$revisions, sliding.window = sliding.window)
            private$conf$ranges.callgraph = construct.ranges(private$conf$revisions.callgraph, sliding.window = sliding.window)
        },

        ## set splitting information revisions and ranges
        set.splitting.info = function(type, length, basis, sliding.window, revisions, revisions.dates) {
            ## set basic slpitting information
            private$conf$split.type = type
            private$conf$split.length = length
            private$conf$split.basis = basis
            private$conf$split.sliding.window = sliding.window

            ## set splitting information on ranges
            private$conf$split.revisions = revisions
            private$conf$split.revisions.dates = revisions.dates
            private$conf$split.ranges = construct.ranges(revisions, sliding.window = sliding.window)
        }

    )
)


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Construction of range strings
##

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
