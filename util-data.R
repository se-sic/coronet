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

        #' Filter commits with empty artifacts from the commit list and save the new list
        #' to 'commits.filtered.empty'.
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

        #' Filter the commits from the commit list that touch the base artifact and save the new list
        #' to 'commits.filtered'.
        #' Add synchronicity and pasta data if configured in 'project.conf'.
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

            ## add pasta data if wanted
            if (private$project.conf$get.pasta()) {
                self$get.pasta()
                commit.data = private$add.pasta.data(commit.data)
            }

            ## store the commit data
            private$commits.filtered = commit.data
            logging::logdebug("filter.commits: finished.")
        },

        #' Add the pasta data to the given data.frame for further analysis.
        #'
        #' @param data the base data as data.frame to append the PaStA data to.
        #'
        #' @return the augmented data.frame
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
        #' The constructor of the class.
        #'
        #' @param project.conf the given 'project.conf' for this instance of the class
        initialize = function(project.conf) {
            if (!missing(project.conf) && "ProjectConf" %in% class(project.conf)) {
                private$project.conf = project.conf
            }

            if (class(self)[1] == "ProjectData")
                logging::loginfo("Initialized data object %s", self$get.class.name())
        },


        ## TO STRING ;) ####
        #' The to String method of the class.
        get.class.name = function() {
            return(
                sprintf("ProjectData<%s>", private$project.conf$get.entry("repo"))
            )
        },


        ## RESET ENVIRONMENT ##

        #' Reset the current environment in order to rebuild it.
        #' Has to be called whenever the project configuration or data gets
        #' changed.
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

        #' Get the current project configuration.
        #'
        #' @return the 'project.conf' of the current instance of the class
        get.project.conf = function() {
            return(private$project.conf)
        },

        #' Set the current project configuration to the given one.
        #'
        #' @param project.conf the new project configuration.
        #' @param reset.environment parameter to determine whether the environment
        #'                          has to be reset or not
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

        #' Backup the current environment to a file on the disk.
        #'
        #' @param file the path to the backup file
        save.to.disk = function(file) {
            save(self, file = file)
        },


        ## PATHS ####

        #' Get the absolute path to the project's result folder.
        #'
        #' @return the path to the result folder
        get.data.path = function() {
            data.path = private$project.conf$get.entry("datapath")
            return(data.path)
        },

        #' Get the absolute path to the range's result folder for synchronicity data.
        #'
        #' @return the path to the synchronicity files
        get.data.path.synchronicity = function() {
            data.path = private$project.conf$get.entry("datapath.synchronicity")
            return(data.path)
        },

        #' Get the absolute path to the result folder for pasta data.
        #'
        #' @return the path to the pasta data
        get.data.path.pasta = function() {
            data.path = private$project.conf$get.entry("datapath.pasta")
            return(data.path)
        },

        get.data.path.issues = function() {
            data.path = private$project.conf$get.entry("datapath.issues")
            return(data.path)
        },


        ## RAW DATA ####

        #' Get the list of commits without empty artifacts.
        #' If it doesn´t already exist call the filter method.
        #'
        #' @return the commit list without empty artifacts
        get.commits.filtered.empty = function() {
            logging::loginfo("Getting commit data filtered by artifact.base and artifact.empty.")

            ## if commits are not read already, do this
            if (is.null(private$commits.filtered.empty)) {
                private$filter.commits.empty()
            }

            return(private$commits.filtered.empty)
        },

        #' Get the list of commits without the base artifact.
        #' If it doesn´t already exist call the filter method.
        #'
        #' @return the commit list without the base artifact
        get.commits.filtered = function() {
            logging::loginfo("Getting commit data filtered by artifact.base.")

            ## if commits are not read already, do this
            if (is.null(private$commits.filtered)) {
                private$filter.commits()
            }

            return(private$commits.filtered)
        },

        #' Get the complete list of commits.
        #' If it doesn´t already exist call the read method first.
        #'
        #' @return the list of commits
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

        #' Set the commit list of the project to a new one.
        #'
        #' @param data the new list of commits
        set.commits.raw = function(data) {
            logging::loginfo("Setting raw commit data.")
            if (is.null(data)) data = data.frame()
            private$commits.raw = data
        },

        #' Get the synchronicity data.
        #' If it doesn´t already exist call the read method.
        #'
        #' @return the synchronicity data
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

        #' Set the synchronicity data to the given data.
        #'
        #' @param data the new synchronicity data
        set.synchronicity = function(data) {
            logging::loginfo("Setting synchronicity data.")
            private$synchronicity = data
        },

        #' Get the pasta data.
        #' If it doesn´t already exist call the read method.
        #'
        #' @return the pasta data
        get.pasta = function() {
            logging::loginfo("Getting PaStA data.")

            ## if commits are not read already, do this
            if (is.null(private$pasta)) {
                private$pasta = read.pasta(self$get.data.path.pasta())
            }

            return(private$pasta)
        },

        #' Set the pasta data to the given new data.
        #'
        #' @param data the new pasta data
        set.pasta = function(data) {
            logging::loginfo("Setting PaStA data.")
            private$pasta = data
        },

        #' Get the mail data.
        #' If it doesn´t already exist call the read method.
        #' Add pasta data if it is configured.
        #'
        #' @return the mail data
        get.mails = function() {
            logging::loginfo("Getting e-mail data.")

            ## if mails are not read already, do this
            if (is.null(private$mails)) {
                private$mails = read.mails(self$get.data.path())

                ## add pasta data if wanted
                if(private$project.conf$get.pasta()) {
                    private$mails = private$add.pasta.data(private$mails)
                }
            }

            return(private$mails)
        },

        #' Set the mail data to the given new data.
        #'
        #' @param data the new mail data
        set.mails = function(data) {
            logging::loginfo("Setting e-mail data.")
            if (is.null(data)) data = data.frame()
            private$mails = data
        },

        #' Get the author data.
        #' If it doesn´t already exist call the read method.
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

        #' Get the list of artifacts of the project.
        #'
        #' @return the list of artifacts
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

        #' Get single pasta items.
        #' For a given 'message.id', the associated 'commit.hash' is returned.
        #' For a given 'commit.hash', the associated 'message.id' or IDs are returned.
        #'
        #' @param message.id the message ID to get the corresponding commit hash
        #' @param commit.hash the commit hash to get the corresponding message ID
        #'
        #' @return the selected pasta data
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

        #' Map the corresponding authors to each artifact and return the list.
        #'
        #' @return the list of authors for each artifact
        get.artifact2author = function() {
            logging::loginfo("Getting artifact--author data.")

            ## store the authors per artifact
            mylist = get.key.to.value.from.df(self$get.commits.filtered.empty(), "artifact", "author.name")

            return(mylist)
        },

        #' Map the corresponding artifacts to each author and return the list.
        #'
        #' @return the list of artifacts for every author
        get.author2artifact = function() {
            logging::loginfo("Getting author--artifact data.")

            ## store the authors per artifact
            mylist = get.key.to.value.from.df(self$get.commits.filtered.empty(), "author.name", "artifact")

            return(mylist)
        },

        #' Map the corresponding artifacts to each commit and return the list.
        #'
        #' @return the list of artifacts for each commit
        get.commit2artifact = function() {
          logging::loginfo("Getting commit--artifact data.")

          ## store the authors per artifact
          mylist = get.key.to.value.from.df(self$get.commits.filtered.empty(), "hash", "artifact")

          return(mylist)
        },

        #' Map the corresponding authors to each mail thread and return the list.
        #'
        #' @return the list of authors for each mail thread
        get.thread2author = function() {
          logging::loginfo("Getting thread--author data.")

          ## store the authors per thread
          mylist = get.key.to.value.from.df(self$get.mails(), "thread", "author.name")

          return(mylist)
        },

        #' Map the corresponding mails to each author and return the list.
        #'
        #' @return the list of mails for each author
        get.author2mail = function() {
            logging::loginfo("Getting author--mail data.")

            ## store the mails per author
            mylist = get.key.to.value.from.df(self$get.mails(), "author.name", "message.id")

            return(mylist)
        },

        #' Map the corresponding threads to each author and return the list.
        #'
        #' @return the list of threads for each author
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

        #' Map the corresponding commits to each author and return the list.
        #'
        #' @return the list of commits for each author
        get.author2commit = function() {
          logging::loginfo("Getting author--commit data.")

          ## store the authors per artifact
          mylist = get.key.to.value.from.df(self$get.commits.raw(), "author.name", "hash")
          mylist = parallel::mclapply(mylist, unique)

          return(mylist)
        },

        #' Map the corresponding files to each author and return the list.
        #'
        #' @return the list of files for each author
        get.author2file = function() {
            logging::loginfo("Getting author--file data.")

            ## store the authors per artifact
            mylist = get.key.to.value.from.df(self$get.commits.filtered.empty(), "author.name", "file")

            return(mylist)
        },

        #' Map the corresponding files to each commit and return the list.
        #'
        #' @return the list of files for each commit
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

        #' Constructor of the class. Constructs a new instance by calling the
        #' constructor of 'ProjectData' with the given 'project.conf' and then
        #' setting the 'range' and the 'revision.callgraph' to the given ones
        #' if they exist.
        #'
        #' @param project.conf the project configuration for the new instance
                  #' @param range the range for the new instance
                  #' @param revision.callgraph the revision callgraph for the new instance
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

        #' The to string method of the class.
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

        #' Construct and return the absolute path to the range's result folder.
        #'
        #' @return the path to the range's result folder
        get.data.path = function() {
            data.path = private$project.conf$get.entry("datapath")
            range = private$range
            return(file.path(data.path, range))
        },

        #' Construct and return the absolute path to the range's result folder for callgraphs
        #'
        #' @return the path to the range's result folder for callgraphs
        get.data.path.callgraph = function() {
            data.path = file.path(private$project.conf$get.entry("datapath.callgraph"), private$revision.callgraph)
            return(data.path)
        },


        ## DATA ####

        #' Get the 'range' of the current instance.
        #'
        #' @return the range
        get.range = function() {
            return(private$range)
        },

        #' Get the 'revision.callgraph' of the current instance
        #'
        #' @return the revision callgraph
        get.revision.callgraph = function() {
            return(private$revision.callgraph)
        }

    )
)

## TODO rename arguments 'thing1' and 'thing2' to 'key' and 'value', resp.
#' Transform the 'base data' data.frame to a list in order to execute
#' the following tasks:
#'  - split by column given by key
#'  - use value as first column in sublist items
#'  - append all other existing columns (including key and value)
#'  - each item in the results list gets attributes:
#'   - group.type (=value) and
#'   - group.name (=unique(item[[key]]))
#'
#' @param base.data the base data for the method
#' @param thing1 the key for the result
#' @param thing2 the value for the result
#' @param ... a possibility for further attributes to be passed
#'
#' @return the resulting list
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
