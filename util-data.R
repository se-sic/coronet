## (c) Claus Hunsen, 2016, 2017
## hunsen@fim.uni-passau.de
## (c) Raphael Nömmer, 2017
## noemmer@fim.uni-passau.de
## (c) Christian Hechtl, 2017
## hechtl@fim.uni-passau.de


## libraries
requireNamespace("R6") # for R6 classes
requireNamespace("logging") # for logging
requireNamespace("parallel") # for parallel computation


options(stringsAsFactors = FALSE)


## / / / / / / / / / / / / / /
## NETWORk META-CONFIGURATION
##

## node types
TYPE.AUTHOR = 1
TYPE.ARTIFACT = 2

# edge types
TYPE.EDGES.INTRA = 3
TYPE.EDGES.INTER = 4


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## ProjectData
##
## Represents the data for one revision range on Codeface Data

#### ProjectData ####
ProjectData = R6::R6Class("ProjectData",

    ## private members ####
    private = list(
        ## configuration objects
        project.conf = NULL, # list
        network.conf = NULL,

        ## raw data
        ## commits and commit data
        commits.filtered = NULL, # data.frame
        commits.filtered.empty = NULL, #data.frame
        commits.raw = NULL, # data.frame
        artifacts = NULL, # list
        synchronicity = NULL, # data.frame
        ## pasta tool data
        pasta = NULL, # list
        ## mails
        mails = NULL, # data.frame
        ## authors
        authors = NULL, # list

        ## BASIC DATA ####

        #read the commits without empty artifacts
        filter.commits.empty = function() {

            logging::logdebug("filter.commits.empty: starting.")

            ## do not compute anything more than once
            if (!is.null(private$commits.filtered.empty)) {
                logging::logdebug("filter.commits.empty: finished. (already existing)")
                return(private$commits.filtered.empty)
            }

            ## get raw commit data
            commit.data = self$get.commits.filtered()

            ## break if the list of commits is empty
            if (nrow(commit.data) == 0) {
                logging::logwarn("There are no commits available for the current environment.")
                logging::logwarn("Class: %s", self$get.class.name())
                # logging::logwarn("Configuration: %s", private$project.conf$get.conf.as.string())
                private$commits.filtered.empty = data.frame()
                return(private$commits.filtered.empty)
            }

            ## only process commits with non-empty artifact
            commit.data = subset(commit.data, artifact != "")

            ## store the commit data
            private$commits.filtered.empty = commit.data
            logging::logdebug("filter.commits.empty: finished.")
        },

        ## read the base filtered commit data for the range
        filter.commits = function() {

            logging::logdebug("filter.commits: starting.")

            ## do not compute anything more than once
            if (!is.null(private$commits.filtered)) {
                logging::logdebug("filter.commits: finished. (already existing)")
                return(private$commits.filtered)
            }

            ## get raw commit data
            commit.data = self$get.commits.raw()

            ## break if the list of commits is empty
            if (nrow(commit.data) == 0) {
                logging::logwarn("There are no commits available for the current environment.")
                logging::logwarn("Class: %s", self$get.class.name())
                # logging::logwarn("Configuration: %s", private$project.conf$get.conf.as.string())
                private$commits.filtered = data.frame()
                return(private$commits.filtered)
            }

            ## only process commits with the artifact listed in the configuration or missing
            commit.data = subset(commit.data, artifact.type %in%
                                     c(private$project.conf$get.entry("artifact.codeface"), ""))

            ## filter out the base artifacts (i.e., Base_Feature, File_Level)
            if (private$network.conf$get.variable("artifact.filter.base")) {
                commit.data = subset(commit.data, !(artifact %in% c("Base_Feature", "File_Level")))
            }

            ## append synchronicity data if wanted
            if (private$network.conf$get.variable("synchronicity")) {
                synchronicity.data = self$get.synchronicity()
                commit.data = merge(commit.data, synchronicity.data, by = "hash", all.x = TRUE)
            }
            ## add synchronicity column anyway
            else {
                dummy.data = switch(
                    as.character(nrow(commit.data)),
                    ## if there are no data available, we need to add the synchronicity column in a special way
                    "0" = logical(0),
                    ## otherwise, add NAs to denote non-existing data
                    NA
                )
                commit.data = cbind(commit.data, synchronicity = dummy.data)
            }

            if (private$network.conf$get.variable("pasta")) {
                pasta = self$get.pasta()
            }

            ## store the commit data
            private$commits.filtered = commit.data
            logging::logdebug("filter.commits: finished.")

        }

    ),


    ## public members ####
    public = list(
        ## constructor
        initialize = function(project.conf, network.conf) {
            if (!missing(project.conf) && "ProjectConf" %in% class(project.conf)) {
                private$project.conf = project.conf
            }

            if(!missing(network.conf) && "NetworkConf" %in% class(network.conf)) {
                private$network.conf = network.conf
            }

            if (class(self)[1] == "ProjectData")
                logging::loginfo("Initialized data object %s", self$get.class.name())
        },


        ## TO STRING ;) ####

        get.class.name = function() {
            return(
                sprintf("ProjectData<%s>", private$project.conf$get.entry("repo"))
            )
        },


        ## RESET ENVIRONMENT ##

        # Reset cached data
        reset.environment = function() {
          private$commits.filtered = NULL
          private$commits.filtered.empty = NULL
          private$commits.raw = NULL
          private$artifacts = NULL
          private$synchronicity = NULL
          private$mails = NULL
          private$authors = NULL
        },


        ## CONFIGURATION ####

        # Get the current project configuration
        get.project.conf = function() {
            return(private$project.conf)
        },

        # Set the current project configuration to the given one.
        set.project.conf = function(project.conf, reset.environment = FALSE) {
            private$project.conf = project.conf

            if (reset.environment) {
                self$reset.environment()
            }
        },

        ## BACKUP ####

        save.to.disk = function(file) {
            save(self, file = file)
        },


        ## PATHS ####

        ## construct the absolute path to the project's result folder
        get.data.path = function() {
            data.path = private$project.conf$get.entry("datapath")
            return(data.path)
        },

        ## construct the absolute path to the range's result folder for synchronicity data
        get.data.path.synchronicity = function() {
            data.path = private$project.conf$get.entry("datapath.synchronicity")
            return(data.path)
        },

        get.data.path.pasta = function() {
            data.path = private$project.conf$get.entry("datapath.pasta")
            return(data.path)
        },


        ## RAW DATA ####

        #get the list of commits without empty artifacts
        get.commits.filtered.empty = function() {
            logging::loginfo("Getting commit data filtered by artifact.base and artifact.empty.")

            ## if commits are not read already, do this
            if (is.null(private$commits.filtered.empty)) {
                private$filter.commits.empty()
            }

            return(private$commits.filtered.empty)
        },

        ## get the complete filtered list of commits
        get.commits.filtered = function() {
            logging::loginfo("Getting commit data filtered by artifact.base.")

            ## if commits are not read already, do this
            if (is.null(private$commits.filtered)) {
                private$filter.commits()
            }

            return(private$commits.filtered)
        },

        ## get the complete raw list of commits
        get.commits.raw = function() {
            logging::loginfo("Getting raw commit data.")

            ## if commits are not read already, do this
            if (is.null(private$commits.raw)) {
                private$commits.raw = read.commits.raw(self$get.data.path(), private$project.conf$get.entry("artifact"))
            }

            return(private$commits.raw)
        },

        ## set the complete raw list of commits
        set.commits.raw = function(data) {
            logging::loginfo("Setting raw commit data.")
            if (is.null(data)) data = data.frame()
            private$commits.raw = data
        },

        ## get the complete synchronicity data
        get.synchronicity = function() {
            logging::loginfo("Getting synchronicity data.")

            ## if commits are not read already, do this
            if (is.null(private$synchronicity)) {
                private$synchronicity = read.synchronicity(self$get.data.path.synchronicity(),
                                                           private$project.conf$get.entry("artifact"),
                                                           private$network.conf$get.variable("synchronicity.time.window"))
            }

            return(private$synchronicity)
        },

        ## set the complete synchronicity data
        set.synchronicity = function(data) {
            logging::loginfo("Setting synchronicity data.")
            private$synchronicity = data
        },

        get.pasta = function() {
            logging::loginfo("Getting pasta data.")

            ## if commits are not read already, do this
            if (is.null(private$pasta)) {
                private$pasta = read.pasta(self$get.data.path.pasta())
            }

            return(private$pasta)
        },

        ## get the complete list of mails
        get.mails = function() {
            logging::loginfo("Getting e-mail data.")

            ## if mails are not read already, do this
            if (is.null(private$mails)) {
                private$mails = read.mails(self$get.data.path())
            }

            return(private$mails)
        },

        ## set the complete list of mails
        set.mails = function(data) {
            logging::loginfo("Setting e-mail data.")
            if (is.null(data)) data = data.frame()
            private$mails = data
        },

        ## get the ID--author mapping
        get.authors = function() {
            logging::loginfo("Getting author data.")

            ## if authors are not read already, do this
            if (is.null(private$authors)) {
                private$authors = read.authors(self$get.data.path())
            }

            return(private$authors)
        },

        ## set the ID--author mapping
        set.authors = function(data) {
            logging::loginfo("Setting author data.")
            private$authors = data
        },

        ## get the list of artifacts
        get.artifacts = function() {
            logging::loginfo("Getting artifact data.")

            ## if artifacts are not read already, do this
            if (is.null(private$artifacts)) {
                commits = self$get.commits.filtered.empty()

                ## get artifacts (empty list if no commits exist)
                artifacts = unique(commits[["artifact"]])
                if (is.null(artifacts)) artifacts = list()

                private$artifacts = artifacts
            }

            return(private$artifacts)
        },


        ## DATA ####

        ## get the authors for each artifact
        get.artifact2author = function() {
            logging::loginfo("Getting artifact--author data.")

            ## get commits sorted by date
            sorted.commits = self$get.commits.filtered.empty()

            ## break if list of commits is empty
            if (ncol(sorted.commits) == 0) {
                return(list())
            }

            ## sort commits by date
            sorted.commits = sorted.commits[order(sorted.commits[["date"]], decreasing = FALSE), ] # sort!

            ## store the authors per artifact
            mylist = get.thing2thing(sorted.commits, "artifact", "author.name", network.conf = private$network.conf)

            return(mylist)
        },

        ## get the artifacts for each author
        get.author2artifact = function() {
            logging::loginfo("Getting author--artifact data.")

            ## store the authors per artifact
            mylist = get.thing2thing(self$get.commits.filtered.empty(), "author.name", "artifact", network.conf = private$network.conf)

            return(mylist)
        },

        ## get the artifacts for each commits
        get.commit2artifact = function() {
          logging::loginfo("Getting commit--artifact data.")

          ## store the authors per artifact
          mylist = get.thing2thing(self$get.commits.filtered(), "hash", "artifact", network.conf = private$network.conf)

          return(mylist)
        },

        ## get the authors for each mail thread
        get.thread2author = function() {
          logging::loginfo("Getting thread--author data.")

          ## store the authors per thread
          mylist = get.thing2thing(self$get.mails(), "thread", "author.name", network.conf = private$network.conf)

          return(mylist)
        },




        ## NotUsed  ####

        ## get the commits for each author
        get.author2commit = function() {
          logging::loginfo("Getting author--commit data.")

          ## store the authors per artifact
          mylist = get.thing2thing(self$get.commits.raw(), "author.name", "hash", network.conf = private$network.conf)
          mylist = parallel::mclapply(mylist, unique)

          return(mylist)
        },

        ## get the files for each author
        get.author2file = function() {
            logging::loginfo("Getting author--file data.")

            ## store the authors per artifact
            mylist = get.thing2thing(self$get.commits.filtered.empty(), "author.name", "file", network.conf = private$network.conf)

            return(mylist)
        },

        ## get the files for each commits
        get.commit2file = function() {
            logging::loginfo("Getting commit--file data.")

            ## store the authors per artifact
            mylist = get.thing2thing(self$get.commits.filtered(), "hash", "file", network.conf = private$network.conf)

            return(mylist)
        }

        ## EntNotUsed ####

    )
)

## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## RangeData
##
## Represents the data for one revision range on Codeface Data

#### RangeData ####
RangeData = R6::R6Class("RangeData",

    inherit = ProjectData,

    ## private members ####
    private = list(
        range = NULL, # character
        revision.callgraph = NA # character
    ),

    ## public members ####
    public = list(

        ## constructor
        initialize = function(project.conf, network.conf, range, revision.callgraph = "") {
            ## call super constructor
            super$initialize(project.conf, network.conf)

            if (!missing(range) && is.character(range)) {
                private$range <- range
            }
            if (!missing(revision.callgraph) && is.character(revision.callgraph) && revision.callgraph != "") {
                    private$revision.callgraph <- revision.callgraph
            }

            logging::loginfo("Initialized data object %s", self$get.class.name())
        },

        ## TO STRING ;) ####

        get.class.name = function() {
            return(
                sprintf("RangeData<%s, %s, %s>",
                        private$project.conf$get.entry("repo"),
                        private$range,
                        private$revision.callgraph
                )
            )
        },


        ## PATHS ####

        ## construct the absolute path to the range's result folder
        get.data.path = function() {
            data.path = private$project.conf$get.entry("datapath")
            range = private$range
            return(file.path(data.path, range))
        },

        ## construct the absolute path to the range's result folder for callgraphs
        get.data.path.callgraph = function() {
            data.path = file.path(private$project.conf$get.entry("datapath.callgraph"), private$revision.callgraph)
            return(data.path)
        },


        ## DATA ####

        ## get range
        get.range = function() {
            return(private$range)
        },

        ## get call-graph revision
        get.revision.callgraph = function() {
            return(private$revision.callgraph)
        }

    )
)
