## (c) Claus Hunsen, 2016
## hunsen@fim.uni-passau.de


## libraries
library(R6) # for R6 classes
library(yaml) # for Codeface configuration files
library(logging)


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

## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## NetworkConf
##
## Represents the network configurations

NetworkConf = R6Class("NetworkConf",
    private = list(
        #Variables with default values
        #has to be configured through update.values method
        author.relation = "mail", #character
        author.directed = FALSE, #bool
        artifact.relation = "cochange", #character
        artifact.directed = FALSE, #bool
        artifact.filter = TRUE, #bool
        artifact.filter.base = TRUE, #bool
        artifact.filter.empty = TRUE, #bool
        artifact.edge.attributes = c(), #list
        simplified = FALSE, #bool
        skip.threshold = Inf, #numeric
        synchronicity = FALSE, #bool
        synchronicity.time.window = 0, #numeric
        contract.edges = FALSE #bool




    ),
    public = list(
        #for testing reasons
        print.object = function() {
            print(private$author.relation)
            print(private$author.directed)
            print(private$artifact.relation)
            print(private$artifact.directed)
            print(private$artifact.filter)
            print(private$artifact.filter.base)
            print(private$artifact.filter.empty)
            print(private$artifact.edge.attributes)
            print(private$simplified)
            print(private$skip.threshold)
            print(private$synchronicity)
            print(private$synchronicity.time.window)
            print(private$contract.edges)

        },
        # initialize = function(author.relation, artifact.relation, author.directed, artifact.directed, simplified,
        #                       artifact.filter, artifact.filter.base, artifact.filter.empty, skip.threshold, synchronized,
        #                       synchronicity.time.window, contract.edges, artifact.extra.edge.attributes){
        #     if(!missing(author.relation) && is.character(author.relation)) {
        #         private$author.relation = author.relation
        #     }
        #     if(!missing(author.directed) && is.character(author.directed)) {
        #         private$author.directed = author.directed
        #     }
        #     if(!missing(artifact.relation) && is.character(artifact.relation)) {
        #         private$artifact.relation = artifact.relation
        #     }
        #     if(!missing(artifact.directed) && is.character(artifact.directed)) {
        #         private$artifact.directed = artifact.directed
        #     }
        #     if(!missing(artifact.filter) && is.logical(artifact.filter)) {
        #         private$artifact.filter = artifact.filter
        #     }
        #     if(!missing(artifact.filter.base) && is.logical(artifact.filter.base)) {
        #         private$artifact.filter.base = artifact.filter.base
        #     }
        #     if(!missing(artifact.filter.empty) && is.logical(artifact.filter.empty)) {
        #         private$artifact.filter.empty = artifact.filter.empty
        #     }
        #     if(!missing(artifact.edge.attributes) && is.list(artifact.edge.attributes)) {
        #         private$artifact.edge.attributes = artifact.edge.attributes
        #     }
        #     if(!missing(simplified) && is.logical(simplified)) {
        #         private$simplified = simplified
        #     }
        #     if(!missing(skip.theshold) && is.logical(skip.threshold)) {
        #       private$skip.threshold = skip.threshold
        #     }
        #     if(!missing(synchronicity) && is.logical(synchronicity)) {
        #       private$synchronicity = synchronicity
        #     }
        #     if(!missing(synchronicity.time.window) && is.numeric(synchronicity.time.window)) {
        #       private$synchronicity.time.window = synchronicity.time.window
        #     }
        #     if(!missing(contract.edges) && is.logical(contract.edges)) {
        #       private$contract.edges = contract.edges
        #     }
        # },

        update.values = function(attr.list = list()) {
            if(!is.null(attr.list$author.relation)) {
                private$author.relation = attr.list$author.relation
            }
            if(!is.null(attr.list$author.directed)) {
                private$author.directed = attr.list$author.directed
            }
            if(!is.null(attr.list$artifact.relation)) {
                private$artifact.relation = attr.list$artifact.relation
            }
            if(!is.null(attr.list$artifact.directed)) {
                private$artifact.directed = attr.list$artifact.directed
            }
            if(!is.null(attr.list$artifact.filter)) {
                private$artifact.filter = attr.list$artifact.filter
            }
            if(!is.null(attr.list$artifact.filter.base)) {
                private$artifact.filter.base = attr.list$artifact.filter.base
            }
            if(!is.null(attr.list$artifact.filter.empty)) {
                private$artifact.filter.empty = attr.list$artifact.filter.empty
            }
            if(!is.null(attr.list$artifact.edge.attributes)) {
                private$artifact.edge.attributes = attr.list$artifact.edge.attributes
            }
            if(!is.null(attr.list$simplified)) {
                private$simplified = attr.list$simplified
            }
            if(!is.null(attr.list$skip.threshold)) {
                private$skip.threshold = attr.list$skip.threshold
            }
            if(!is.null(attr.list$synchronicity)) {
                private$synchronicity = attr.list$synchronicity
            }
            if(!is.null(attr.list$synchronicity.time.window)) {
                private$synchronicity.time.window = attr.list$synchronciity.time.window
            }
            if(!is.null(attr.list$contract.edges)) {
                private$contract.edges = attr.list$contract.edges
            }
        },

        get.variable = function(var.name) {
           return(private[[var.name]])
        }

        # get.author.relation = function() {
        #   return(private$author.relation)
        # },
        #
        # get.artifact.relation = function() {
        #   return(private$artifact.relation)
        # },
        #
        # get.author.directed = function() {
        #   return(private$author.directed)
        # },
        #
        # get.artifact.directed = function() {
        #   return(private$artifact.directed)
        # },
        #
        # is.simplified = function() {
        #   return(private$simplified)
        # },
        #
        # get.artifact.filter = function() {
        #   return(private$artifact.filter)
        # },
        #
        # get.artifact.filter.base = function() {
        #   return(private$artiact.filter.base)
        # },
        #
        # get.artifact.filter.empty = function() {
        #   return(private$artifact.filter.empty)
        # },
        #
        # get.skip.threshold = function() {
        #   return(private$skip.threshold)
        # },
        #
        # get.synchronized = function() {
        #   return(private$synchronized)
        # },
        #
        # get.synchronicity.time.window = function() {
        #   return(private$synchronicity.time.window)
        # },
        #
        # get.contract.edges = function() {
        #   return(private$contract.edges)
        # },
        #
        # get.artifact.extra.edge.attributes = function() {
        #   return(private$artifact.extra.edge.attributes)
        # }


    )
)

## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## CodefaceConf
##
## Represents the Codeface configuration

CodefaceConf = R6Class("CodefaceConf",

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
            conf = yaml.load_file(conf.file)

            ## store artifact in configuration
            conf$artifact = artifact
            conf$artifact.short = ARTIFACT.TO.ABBREVIATION[[ conf$artifact ]]
            conf$artifact.codeface = paste0(
                toupper(substr(conf$artifact, 1, 1)),
                substr(conf$artifact, 2, nchar(conf$artifact))
                ) # first letter uppercase

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
            return(as.yaml(private$conf))
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
