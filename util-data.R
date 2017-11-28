## (c) Claus Hunsen, 2016, 2017
## hunsen@fim.uni-passau.de
## (c) Raphael Nömmer, 2017
## noemmer@fim.uni-passau.de
## (c) Christian Hechtl, 2017
## hechtl@fim.uni-passau.de


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Libraries ---------------------------------------------------------------

requireNamespace("R6") # for R6 classes
requireNamespace("logging") # for logging
requireNamespace("parallel") # for parallel computation


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

        ## * * filtering commits -------------------------------------------

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
                                     c(private$project.conf$get.value("artifact.codeface"), ""))

            ## filter out the base artifacts (i.e., Base_Feature, File_Level)
            if (private$project.conf$get.value("artifact.filter.base")) {
                commit.data = subset(commit.data, !(artifact %in% c("Base_Feature", "File_Level")))
            }

            ## append synchronicity data if wanted
            if (private$project.conf$get.value("synchronicity")) {
                synchronicity.data = self$get.synchronicity()
                commit.data = merge(commit.data, synchronicity.data,
                                    by = "hash", all.x = TRUE, sort = FALSE)
            }

            ## add pasta data if wanted
            if (private$project.conf$get.value("pasta")) {
                self$get.pasta()
                commit.data = private$add.pasta.data(commit.data)
            }

            ## store the commit data
            private$commits.filtered = commit.data
            logging::logdebug("filter.commits: finished.")
        },

        ## * * pasta data -------------------------------------------

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

    ## * public ------------------------------------------------------------

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
            private$commits.filtered.empty = NULL
            private$commits.raw = NULL
            private$artifacts = NULL
            private$synchronicity = NULL
            private$mails = NULL
            private$authors = NULL
            private$pasta = NULL
        },

        ## * * configuration -----------------------------------------------

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

        #' Get the absolute path to the result folder for pasta data.
        #'
        #' @return the path to the pasta data
        get.data.path.pasta = function() {
            data.path = private$project.conf$get.value("datapath.pasta")
            return(data.path)
        },

        get.data.path.issues = function() {
            data.path = private$project.conf$get.value("datapath.issues")
            return(data.path)
        },

        ## * * raw data ----------------------------------------------------

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
                    private$project.conf$get.value("artifact")
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
                if(private$project.conf$get.value("pasta")) {
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

        #' Get the issue data.
        #' If it doesn´t already exist call the read method.
        #'
        #' @return the issue data
        get.issues = function() {
            logging::loginfo("Getting issue data")

            ## if issues have not been read yet do this
            if(is.null(private$issues)) {
                private$issues = read.issues(self$get.data.path.issues())
            }
            return(private$issues)
        },

        #' Set the issue data to the given new data.
        #'
        #' @param data the new issue data
        set.issues = function(data) {
            logging::loginfo("Setting issue data.")
            if (is.null(data)) data = data.frame()
            private$issues = data
        },

        #' Get the list of artifacts of the project.
        #'
        #' @return the list of artifacts
        get.artifacts = function() {
            ## FIXME the artifacts determination should be dependent on the artifact.relation
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

        ## * * processed data ----------------------------------------------

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

        #' Get the 'revision.callgraph' of the current instance
        #'
        #' @return the revision callgraph
        get.revision.callgraph = function() {
            return(private$revision.callgraph)
        },

        #' Get the bounds of the current instance
        #'
        #' @return Returns a vector with two entries (start, end) of type POSIXct if input was a date;
        #'         or of type character if input was a commit hash or version;
        #'         or NULL if the string could not be parsed
        get.bounds = function() {
            return (get.range.bounds(private$range))
        }

    )
)


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Helper functions --------------------------------------------------------

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
