## (c) Claus Hunsen, 2016
## hunsen@fim.uni-passau.de


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
## CodefaceConf
##
## Represents the Codeface configuration

CodefaceConf = R6::R6Class("CodefaceConf",

    ## private members
    private = list(

        data = NULL, # character
        selection.process = NULL, # character
        casestudy = NULL, # character
        artifact = NULL, # character
        conf = NULL, # list

        ## / / / / / / / / / / / / / /
        ## Revisions and ranges
        ##

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

        postprocess.revision.list.for.callgraph.data = function(r) {
            r = gsub("version-", "", r) # remove version prefix (SQLite)
            r = gsub("OpenSSL_", "", r) # remove name prefix (OpenSSL)
            r = gsub("\\.", "_", r) # replace dots by underscores
            return(r)
        },

        construct.ranges = function(revs) {
            ranges = paste(revs[1:(length(revs) - 1)], revs[2:length(revs)], sep = "-")
            return(ranges)
        },


        ## / / / / / / / / / / / / / /
        ## Path construction
        ##

        subfolder.configurations = "configurations",
        subfolder.results = "results",

        ## Construct the path to the configuration folder of Codeface
        get.configurations.folder = function(data, selection.process) {
            return(file.path(data, private$subfolder.configurations, selection.process))
        },

        ## Construct the path to the results folder of Codeface
        get.results.folder = function(data, selection.process, project, tagging) {
            return(file.path(data, private$subfolder.results, selection.process, project, tagging))
        },

        ## Construct the path to the callgraph folder of Codeface
        get.callgraph.folder = function(data, selection.process, casestudy) {
            return(file.path(data, private$subfolder.results, selection.process, paste(casestudy, "callgraphs", sep = "_")))
        },

        ## Construct the path to the synchronicity folder of Codeface
        get.synchronicity.folder = function(data, selection.process, casestudy) {
            return(file.path(data, private$subfolder.results, selection.process, paste(casestudy, "synchronicity", sep = "_")))
        },



        ## construct path to a Codeface configuration
        ## - data: path to codeface-data folder
        ## - selection.process: one of: threemonth, releases
        ## - casestudy: the name of the casestudy (same name as repo folder, e.g., "busybox")
        ## - tagging: the tagging parameter used by Codeface, one of: feature, proximity
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

        ## function to load a Codeface configuration
        ## - data: path to codeface-data folder
        ## - selection.process: one of: threemonth, releases
        ## - casestudy: the name of the casestudy (same name as repo folder, e.g., "busybox")
        ## - artifact: the kind of artifact to study, one of: feature, function, file
        ##
        ## The artifact parameter is automatically mapped to the tagging parameter used by Codeface.
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

            ## store revision data
            conf$revisions = revisions
            conf$revisions.dates = revisions.dates

            ## call-graph revisions (do a postprocessing for list of revisions)
            conf$revisions.callgraph = private$postprocess.revision.list.for.callgraph.data(conf$revisions)

            ## compute revision ranges
            conf$ranges = private$construct.ranges(conf$revisions)
            conf$ranges.callgraph = private$construct.ranges(conf$revisions.callgraph)

            ## SAVE FULL CONFIGURATION OBJECT
            private$conf = conf

            ## logging
            logging::logdebug("Configuration:\n%s", self$get.conf.as.string())
        },

        ## access data in configuration list
        get.conf.entry = function(key) {
            return(private$conf[[key]])
        }

    ),

    ## public members
    public = list(

        ## constructor
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


        ## get configuration list
        get.conf = function() {
            return(private$conf)
        },

        get.conf.as.string = function() {
            return(yaml::as.yaml(private$conf))
        },


        ## CONFIGURATION ENTRIES

        ## project
        get.project = function() {
            return(private$get.conf.entry("project"))
        },

        ## repo
        get.repo = function() {
            return(private$get.conf.entry("repo"))
        },

        ## description
        get.description = function() {
            return(private$get.conf.entry("description"))
        },

        ## mailinglists
        get.mailinglists = function() {
            return(private$get.conf.entry("mailinglists"))
        },

        ## tagging
        get.tagging = function() {
            return(private$get.conf.entry("tagging"))
        },

        ## artifact
        get.artifact = function() {
            return(private$get.conf.entry("artifact"))
        },

        ## artifact.short
        get.artifact.short = function() {
            return(private$get.conf.entry("artifact.short"))
        },

        ## artifact
        get.artifact.codeface = function() {
            return(private$get.conf.entry("artifact.codeface"))
        },

        ## data.path
        get.datapath = function() {
            return(private$get.conf.entry("datapath"))
        },

        ## data.path.callgraph
        get.datapath.callgraph = function() {
            return(private$get.conf.entry("datapath.callgraph"))
        },

        ## data.path.synchronicity
        get.datapath.synchronicity = function() {
            return(private$get.conf.entry("datapath.synchronicity"))
        },

        ## revisions
        get.revisions = function() {
            return(private$get.conf.entry("revisions"))
        },

        ## revisions.dates
        get.revisions.dates = function() {
            return(private$get.conf.entry("revisions.dates"))
        },

        ## revisions.callgraph
        get.revisions.callgraph = function() {
            return(private$get.conf.entry("revisions.callgraph"))
        },

        ## ranges
        get.ranges = function() {
            return(private$get.conf.entry("ranges"))
        },

        ## ranges.callgraph
        get.ranges.callgraph = function() {
            return(private$get.conf.entry("ranges.callgraph"))
        },

        ## get a stripped-down version of the ranges
        ## (for use while plotting)
        get.ranges.simplified = function() {
            # remove names ,e.g. "version", from release cycle names
            return(self$get.ranges())
        },

        ## get the corresponding call-graph revision for the given range
        get.callgraph.revision.from.range = function(range) {
            idx = which(self$get.ranges() == range)
            rev = self$get.revisions.callgraph()[idx + 1]
            return(rev)
        }

    )
)
