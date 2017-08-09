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


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## ProjectData
##
## Represents the data for building Networks

#### ProjectData ####
ProjectData = R6::R6Class("ProjectData",

    ## private members ####
    private = list(
        ## configuration objects
        project.conf = NULL, # list

        ## raw data
        ## commits and commit data
        commits.filtered = NULL, # data.frame
        commits.filtered.empty = NULL, #data.frame
        commits.raw = NULL, # data.frame
        artifacts = NULL, # list
        synchronicity = NULL, # data.frame
        pasta = NULL, # data.frame
        ## mails
        mails = NULL, # data.frame
        ## authors
        authors = NULL, # list
        ##issues
        issues = NULL, #data.frame

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
            if (private$project.conf$get.artifact.filter.base()) {
                commit.data = subset(commit.data, !(artifact %in% c("Base_Feature", "File_Level")))
            }

            ## append synchronicity data if wanted
            if (private$project.conf$get.synchronicity()) {
                synchronicity.data = self$get.synchronicity()
                commit.data = merge(commit.data, synchronicity.data,
                                    by = "hash", all.x = TRUE, sort = FALSE)
            }

            ## add PaStA data if wanted
            if (private$project.conf$get.pasta()) {
                self$get.pasta()
                commit.data = private$add.pasta.data(commit.data)
            }

            ## store the commit data
            private$commits.filtered = commit.data
            logging::logdebug("filter.commits: finished.")
        },

        ## add the pasta data to the given data.frame for further analysis
        add.pasta.data = function(data) {
            logging::loginfo("Adding pasta data.")
            data[, "pasta"] = NA
            if("message.id" %in% colnames(data)) {
                for(i in 1:nrow(data)) {
                    pasta.item = self$get.pasta.items(message.id = data[i, "message.id"])
                    if(!length(pasta.item) == 0) {
                        data$pasta[i] = I(list(pasta.item))
                    }
                }
            } else if("hash" %in% colnames(data)) {
                for(i in 1:nrow(data)) {
                    pasta.item = self$get.pasta.items(commit.hash = data[i, "hash"])
                    if(!length(pasta.item) == 0) {
                        data$pasta[i] = I(list(pasta.item))
                    }
                }
            }
            return(data)
        }
    ),


    ## public members ####
    public = list(
        ## constructor
        initialize = function(project.conf) {
            if (!missing(project.conf) && "ProjectConf" %in% class(project.conf)) {
                private$project.conf = project.conf
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

        ## reset cached data
        reset.environment = function() {
            private$commits.filtered = NULL
            private$commits.filtered.empty = NULL
            private$commits.raw = NULL
            private$artifacts = NULL
            private$synchronicity = NULL
            private$mails = NULL
            private$authors = NULL
            private$pasta = NULL
        },


        ## CONFIGURATION ####

        ## get the current project configuration
        get.project.conf = function() {
            return(private$project.conf)
        },

        ## set the current project configuration to the given one
        set.project.conf = function(project.conf, reset.environment = FALSE) {
            private$project.conf = project.conf

            if (reset.environment) {
                self$reset.environment()
            }
        },

        ## get value from project configuration
        get.project.conf.entry = function(entry) {
            if(entry == "synchronicity") {
                return(project.conf$get.synchronicity())
            } else if (entry == "synchronicity.time.window") {
                return(project.conf$get.synchronicity.time.window())
            } else if (entry == "artifact.filter.base") {
                return(project.conf$get.artifact.filter.base())
            } else if (entry == "pasta") {
                return(project.conf$get.pasta())
            } else {
                logging::logwarn(paste("The variable", entry, "doesn't exist in the project configuration."))
                return(NULL)
            }
        },

        ## set a value of the project configuration and reset the environment
        set.project.conf.entry = function(entry, value) {
            if(entry == "synchronicity" && class(value) == "logical") {
                reset.environment()
                project.conf$set.synchronicity(value)
            } else if (entry == "synchronicity.time.window" && class(value) == "numeric") {
                reset.environment()
                project.conf$set.synchronicity.time.window(value)
            } else if (entry == "artifact.filter.base" && class(value) == "logical") {
                reset.environment()
                project.conf$set.artifact.filter.base(value)
            } else if (entry == "pasta" && class(value) == "logical") {
                reset.environment()
                project.conf$set.pasta(value)
            } else {
                logging::logwarn(paste("The variable", entry, "doesn't exist in the project configuration."))
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

        get.data.path.issues = function() {
            data.path = private$project.conf$get.entry("datapath.issues")
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
                private$commits.raw = read.commits.raw(
                    self$get.data.path(),
                    private$project.conf$get.entry("artifact")
                )
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
                private$synchronicity = read.synchronicity(
                    self$get.data.path.synchronicity(),
                    private$project.conf$get.entry("artifact"),
                    private$project.conf$get.synchronicity.time.window()
                )
            }

            return(private$synchronicity)
        },

        ## set the complete synchronicity data
        set.synchronicity = function(data) {
            logging::loginfo("Setting synchronicity data.")
            private$synchronicity = data
        },

        ## get the complete PaStA data
        get.pasta = function() {
            logging::loginfo("Getting PaStA data.")

            ## if commits are not read already, do this
            if (is.null(private$pasta)) {
                private$pasta = read.pasta(self$get.data.path.pasta())
            }

            return(private$pasta)
        },

        ## set the complete PaStA data
        set.pasta = function(data) {
            logging::loginfo("Setting PaStA data.")
            private$pasta = data
        },

        ## get the complete list of mails
        get.mails = function() {
            logging::loginfo("Getting e-mail data.")

            ## if mails are not read already, do this
            if (is.null(private$mails)) {
                private$mails = read.mails(self$get.data.path())

                ## add PaStA data if wanted
                if(private$project.conf$get.pasta()) {
                    private$mails = private$add.pasta.data(private$mails)
                }
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

        get.issue.ids = function() {
            logging::loginfo("Getting event data.")
            ## if artifacts are not read already, do this
            if (is.null(private$issues)) {
                self$get.issues()

                ## get artifacts (empty list if no commits exist)
            }
            ids = unique(private$issues[["issue.id"]])

            return(ids)

        },

        ## get the list of issues
        get.issues = function() {
            logging::loginfo("Getting issue data")

            ## if issues have not been read yet do this
            if(is.null(private$issues)) {
                private$issues = read.issues(self$get.data.path.issues())
            }
            return(private$issues)
        },


        ## DATA ####

        ## get single pasta items
        get.pasta.items = function(message.id = NULL, commit.hash = NULL) {
            logging::loginfo("Getting pasta items started.")
            #if neither message.id nor commit.hash are specified break the code
            if(is.null(message.id) && is.null(commit.hash)) {
                logging::logwarn("Neither message.id nor commit.hash specified.")
                return()
            }

            ## get pasta data
            self$get.pasta()

            ## if a message.id is given just return the attached list of commit hashes
            ## else gather all message.ids which contain the given commit.hash and return them
            if(!is.null(message.id)) {
                result = private$pasta[private$pasta[["message.id"]] == message.id, "commit.hash"]
                return(result)
            } else {
                result = private$pasta[private$pasta[["commit.hash"]] == commit.hash, "message.id"]
                return(result)
            }
        },

        ## get the authors for each artifact
        get.artifact2author = function() {
            logging::loginfo("Getting artifact--author data.")

            ## store the authors per artifact
            mylist = get.key.to.value.from.df(self$get.commits.filtered.empty(), "artifact", "author.name")

            return(mylist)
        },

        ## get the artifacts for each author
        get.author2artifact = function() {
            logging::loginfo("Getting author--artifact data.")

            ## store the authors per artifact
            mylist = get.key.to.value.from.df(self$get.commits.filtered.empty(), "author.name", "artifact")

            return(mylist)
        },

        ## get the artifacts for each commits
        get.commit2artifact = function() {
          logging::loginfo("Getting commit--artifact data.")

          ## store the authors per artifact
          mylist = get.key.to.value.from.df(self$get.commits.filtered.empty(), "hash", "artifact")

          return(mylist)
        },

        ## get the authors for each mail thread
        get.thread2author = function() {
          logging::loginfo("Getting thread--author data.")

          ## store the authors per thread
          mylist = get.key.to.value.from.df(self$get.mails(), "thread", "author.name")

          return(mylist)
        },

        ## get the mails for each author
        get.author2mail = function() {
            logging::loginfo("Getting author--mail data.")

            ## store the mails per author
            mylist = get.key.to.value.from.df(self$get.mails(), "author.name", "message.id")

            return(mylist)
        },

        ## get the threads for each author
        get.author2thread = function() {
            logging::loginfo("Getting author--thread data.")

            ## store the threads per author
            mylist = get.key.to.value.from.df(self$get.mails(), "author.name", "thread")

            return(mylist)
        },


        get.issue2author = function() {
            logging::loginfo("Getting issue--author data")

            mylist = get.key.to.value.from.df(self$get.issues(), "issue.id", "author.name")

            return(mylist)
        },

        get.author2issue = function() {
            logging::loginfo("Getting author--issue data")

            mylist = get.key.to.value.from.df(self$get.issues(), "author.name", "issue.id")

            return(mylist)
        },



        ## NotUsed  ####

        ## get the commits for each author
        get.author2commit = function() {
          logging::loginfo("Getting author--commit data.")

          ## store the authors per artifact
          mylist = get.key.to.value.from.df(self$get.commits.raw(), "author.name", "hash")
          mylist = parallel::mclapply(mylist, unique)

          return(mylist)
        },

        ## get the files for each author
        get.author2file = function() {
            logging::loginfo("Getting author--file data.")

            ## store the authors per artifact
            mylist = get.key.to.value.from.df(self$get.commits.filtered.empty(), "author.name", "file")

            return(mylist)
        },

        ## get the files for each commits
        get.commit2file = function() {
            logging::loginfo("Getting commit--file data.")

            ## store the authors per artifact
            mylist = get.key.to.value.from.df(self$get.commits.filtered.empty(), "hash", "file")

            return(mylist)
        }



        ## EntNotUsed ####

    )
)

## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## RangeData
##
## Represents the data for one revision range for building Networks

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
        initialize = function(project.conf, range, revision.callgraph = "") {
            ## call super constructor
            super$initialize(project.conf)

            if (!missing(range) && is.character(range)) {
                private$range = range
            }
            if (!missing(revision.callgraph) && is.character(revision.callgraph) && revision.callgraph != "") {
                private$revision.callgraph = revision.callgraph
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


## Transform data.frame 'base.data' to a list
## - split by column given by thing1 (key)
## - use thing2 as first column in sublist items (value)
## - append all other existing columns (including thing1 and thing2)
## - each item in the results list gets attributes
##   - group.type (=thing1) and
##   - group.name (=unique(item[[thing1]]))
## TODO rename arguments 'thing1' and 'thing2' to 'key' and 'value', resp.
get.key.to.value.from.df = function(base.data, thing1, thing2, ...) {
    logging::logdebug("get.key.to.value.from.df: starting.")

    ## define names for key (thing1) and value (thing2) columns
    column.key = "data.coupling"
    column.value = "data.vertices"

    ## if there is not data to subset, return am enpty list directly
    if (nrow(base.data) == 0) {
        logging::logwarn("Trying to get subset of non-existent data.")
        logging::logwarn(sprintf("Stacktrace:  %s", get.stacktrace(sys.calls())))
        logging::logdebug("get.key.to.value.from.df: finished.")
        return(list())
    }

    ## re-arrange columns and use things as first columns
    cols.old = colnames(base.data)
    base.data = base.data[c(thing1, thing2, cols.old)]
    colnames(base.data) = c(column.key, column.value, cols.old)

    ## group list by thing1 and construct a list: thing1 -> data.frame(thing2, other columns)
    transform.df.per.item = function(df) {
        group = unique(df[[thing1]])
        ## remove key column from list of columns and keep data.frame
        df = df[, -match(c(column.key), names(df)), drop = FALSE]
        ## add group information as attributes
        attr(df, "group.type") = thing1
        attr(df, "group.name") = group
        return(df)
    }
    mylist = plyr::dlply(base.data, thing1, transform.df.per.item)

    ## remove object attributes introduced by dlply
    attr(mylist, "split_labels") = NULL
    attr(mylist, "split_type") = NULL

    logging::logdebug("get.key.to.value.from.df: finished.")

    return(mylist)
}
