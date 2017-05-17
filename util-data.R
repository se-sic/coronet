## (c) Claus Hunsen, 2016
## hunsen@fim.uni-passau.de


## libraries
library(R6) # for R6 classes
library(igraph) # networks
library(plyr) # for dlply function
library(sqldf) # for sqldf
library(logging) # for logging
library(parallel) # for parallel computation


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
## CodefaceProjectData
##
## Represents the data for one revision range on Codeface Data

#### CodefaceProjectData ####
CodefaceProjectData = R6Class("CodefaceProjectData",

    ## private members ####
    private = list(
        ## configuration
        conf = NULL, # list
        network.conf = NULL, #object

        ## raw data
        ## commits and commit data
        commits = NULL, # data.frame
        commits.raw = NULL, # data.frame
        synchronicity = NULL, # data.frame
        ## mails
        mails = NULL, # data.frame
        ## authors
        authors = NULL, # list

        ## networks
        authors.network.mail = NULL, # igraph
        authors.network.cochange = NULL, # igraph
        artifacts.network.cochange = NULL, # igraph
        artifacts.network.callgraph = NULL, # igraph


        ## BASIC DATA ####

        ## read the commit data for the range
        read.commits = function() {
            data.path = self$get.data.path()

            logging::logdebug("read.commits: starting.")

            ## do not compute anything more than once
            if (!is.null(private$commits)) {
                logging::logdebug("read.commits: finished. (already existing)")
                return(private$commits)
            }

            ## get raw commit data
            commit.data = self$get.commits.raw()

            ## only process commits with non-empty artifact
            if (network.conf$get.variable("filter.empty.artifacts")) {
                commit.data = subset(commit.data, artifact != "")
            }

            ## only process commits with the artifact listed in the configuration
            if (network.conf$get.variable("filter.artifact")) {
                commit.data = subset(commit.data, artifact.type == private$conf$get.artifact.codeface())
            }

            ## filter out the base artifacts (i.e., Base_Feature, File_Level)
            if (network.conf$get.variable("filter.base.artifact")) {
                commit.data = subset(commit.data, !(artifact %in% c("Base_Feature", "File_Level")))
            }

            ## store the commit data
            private$commits = commit.data
            logging::logdebug("read.commits: finished.")
        },

        read.commits.raw = function() {

            logging::logdebug("read.commits.raw: starting.")

            ## do not compute anything more than once
            if (!is.null(private$commits.raw)) {
                logging::logdebug("read.commits.raw: finished. (already existing)")
                return(private$commits.raw)
            }

            ## get file name of commit data
            data.path = self$get.data.path()
            file = file.path(data.path, "commits.list")

            ## read data.frame from disk (as expected from save.list.to.file) [can be empty]
            commit.data <- try(read.table(file, header = FALSE, sep = ";", strip.white = TRUE,
                                          fileEncoding = "latin1", encoding = "utf8"), silent = TRUE)

            ## break if the list of commits is empty
            if (inherits(commit.data, 'try-error')) {
                logging::logerror("There are no commits available for the current environment.")
                logging::logerror("Class: %s", self$get.class.name())
                # logging::logerror("Configuration: %s", private$conf$get.conf.as.string())
                stop("Stopped due to missing commits.")
            }

            ## set proper column names based on Codeface extraction:
            ##
            ## SELECT c.id, c.authorDate, a.name, a.email1, c.commitHash,
            ## c.ChangedFiles, c.AddedLines, c.DeletedLines, c.DiffSize,
            ## cd.file, cd.entityId, cd.entityType, cd.size
            colnames(commit.data) = c(
                "id", # id
                "date", "author.name", "author.email", # author information
                "hash", "changed.files", "added.lines", "deleted.lines", "diff.size", # commit information
                "file", "artifact", "artifact.type", "artifact.diff.size" ## commit-dependency information
            )

            ## rewrite data.frame when we want file-based data
            ## (we have proximity-based data as foundation)
            if (private$conf$get.artifact() == "file") {
                ## aggregate diff size by hash and file
                commit.data = sqldf("select *, sum(`artifact.diff.size`) as diffsum from `commit.data` group by hash, file
                                    order by `date`, `author.name`, `id`, `file`, `artifact`")

                ## fix column class for diffsum
                commit.data["diffsum"] = as.numeric(commit.data[["diffsum"]])

                ## copy columns to match proper layout for further analyses
                commit.data["artifact"] = commit.data[["file"]]
                commit.data["artifact.type"] = "File"
                commit.data["artifact.diff.size"] = commit.data[["diffsum"]]
                commit.data["diffsum"] = NULL # remove
            }

            ## rewrite data.frame when we want function-based data
            ## (we have proximity-based data as foundation)
            if (private$conf$get.artifact() == "function") {
                ## artifact = file name + "::" . function name
                artifacts.new = paste(commit.data[["file"]], commit.data[["artifact"]], sep = "::")

                ## clean up empty artifacts and File_Level artifact
                artifacts.new = gsub("^::$", "", artifacts.new)
                artifacts.new = gsub("^(.*)::File_Level$", "File_Level", artifacts.new)

                ## insert new artifact names into commit table
                commit.data["artifact"] = artifacts.new
            }

            ## append synchronicity data if wanted
            if (network.conf$get.variable("synchronicity")) {
                synchronicity.data = self$get.synchronicity()
                commit.data = merge(commit.data, synchronicity.data, by = "hash", all.x = TRUE)
            } else {
                ## fill with NAs for safety reasons
                commit.data[["synchronicity"]] = NA
            }

            ## store the commit data
            private$commits.raw = commit.data
            logging::logdebug("read.commits.raw: finished.")
        },

        ## read the synchronicity data of commits
        read.synchronicity = function(time.window = 5){
            logging::logdebug("read.synchronicity: starting.")

            ## do not compute anything more than once
            if (!is.null(private$synchronicity)) {
                logging::logdebug("read.synchronicity: finished. (already existing)")
                return(private$sychronicity)
            }

            ## check time.window
            allowed.time.windows = c(1, 5, 10)
            stopifnot(time.window %in% allowed.time.windows)

            ## construct path and file
            data.path = self$get.data.path.synchronicity()
            file.name = paste0("commit_sync_analysis_", private$conf$get.artifact(), "s_", time.window, ".dat")
            file = file.path(data.path, file.name)

            ## break if file does not exist
            stopifnot(file.exists(file))

            ## load commit.ids object
            load(file = file)
            synchronous.commits = data.frame(hash = commit.hashes[["synchronous"]], synchronous = TRUE)
            nonsynchronous.commits = data.frame(hash = commit.hashes[["non.synchronous"]], synchronous = FALSE)

            ## construct data.frame
            synchronicity = rbind.fill(synchronous.commits, nonsynchronous.commits)

            ## store the synchronicity data
            private$synchronicity = synchronicity
            logging::logdebug("read.synchronicity: finished.")
        },

        ## read the mail data for the range
        read.mails = function() {

            logging::logdebug("read.mails: starting.")

            ## do not compute anything more than once
            if (!is.null(private$mails)) {
                logging::logdebug("read.mails: finished. (already existing)")
                return(private$mails)
            }

            ## get file name of commit data
            data.path = self$get.data.path()
            file = file.path(data.path, "emails.list")

            ## read data.frame from disk (as expected from save.list.to.file) [can be empty]
            mail.data <- try(read.table(file, header = FALSE, sep = ";", strip.white = TRUE,
                                        fileEncoding = "latin1", encoding = "utf8"), silent = TRUE)

            ## break if the list of mails is empty
            if (inherits(mail.data, 'try-error')) {
                logging::logwarn("There are no mails available for the current environment.")
                logging::logwarn("Class: %s", self$get.class.name())
                # logging::logwarn("Configuration: %s", private$conf$get.conf.as.string())
                private$mails = data.frame()
                return()
            }

            ## set proper column names based on Codeface extraction:
            ##
            ## SELECT a.name AS authorName, a.messageId, a.email1, m.creationDate, m.subject, m.threadId
            colnames(mail.data) = c(
                "author.name", "author.email", # author information
                "message.id", "date", "date.offset", "subject", # meta information
                "thread" # thread ID
            )

            ## remove mails without a proper date as they mess up directed mail-based networks
            ## this basically only applies for project-level analysis
            empty.dates = which(mail.data[["date"]] == "" | is.na(mail.data[["date"]]))
            if (length(empty.dates) > 0)
                mail.data = mail.data[-empty.dates, ]

            ## store the mail data
            private$mails = mail.data
            logging::logdebug("read.mails: finished. (already existing)")
        },

        ## read the author data for the range
        read.authors = function() {

            logging::logdebug("read.authors: starting.")

            ## do not compute anything more than once
            if (!is.null(private$authors)) {
                logging::logdebug("read.authors: finished. (already existing)")
                return(private$authors)
            }

            ## get file name of commit data
            data.path = self$get.data.path()
            file = file.path(data.path, "authors.list")

            ## read data.frame from disk (as expected from save.list.to.file) [can be empty]
            authors.df <- try(read.table(file, header = FALSE, sep = ";", strip.white = TRUE,
                                         fileEncoding = "latin1", encoding = "utf8"), silent = TRUE)

            ## break if the list of authors is empty
            if (inherits(authors.df, 'try-error')) {
                logging::logerror("There are no authors available for the current environment.")
                logging::logerror("Class: %s", self$get.class.name())
                logging::logerror("Configuration: %s", private$conf$get.conf.as.string())
                stop("Stopped due to missing authors.")
            }

            ## set proper column names based on Codeface extraction:
            ##
            ## SELECT a.name AS authorName, a.email1, m.creationDate, m.subject, m.threadId
            colnames(authors.df) = c(
                "ID", "author.name" # author information
            )

            ## store the ID--author mapping
            private$authors = authors.df
            logging::logdebug("read.authors: finished. (already existing)")
        },


        ## AUTHOR NETWORKS ####

        ## get the co-change-based developer relation as network
        get.author.network.cochange = function() {

            logging::logdebug("get.author.network.cochange: starting.")

            ## do not compute anything more than once
            if (!is.null(private$authors.network.cochange)) {
                logging::logdebug("get.author.network.cochange: finished. (already existing)")
                return(private$authors.network.cochange)
            }

            ## read authors if not done yet
            if (is.null(private$authors)) {
                private$read.authors()
            }

            ## construct network based on artifact2author data
            artifact2author = self$get.artifact2author(network.conf = private$network.conf)
            author.net = construct.dependency.network.from.list(artifact2author, network.conf = private$network.conf)

            ## store network
            private$authors.network.cochange = author.net
            logging::logdebug("get.author.network.cochange: finished.")

            return(author.net)
        },

        ## get the thread-based developer relation as network
        get.author.network.mail = function() {

            logging::logdebug("get.author.network.mail: starting.")

            ## do not compute anything more than once
            if (!is.null(private$authors.network.mail)) {
                logging::logdebug("get.author.network.mail: finished. (already existing)")
                return(private$authors.network.mail)
            }

            edge.attributes = c("date", "message.id", "thread")
            thread2author = self$get.thread2author(extra.data = edge.attributes)

            if (length(thread2author) != 0) {
                dev.relation =
                    construct.dependency.network.from.list(thread2author, network.conf = private$network.conf)
            } else {
                dev.relation = create.empty.network(directed = directed)
            }

            ## store network
            private$authors.network.mail = dev.relation
            logging::logdebug("get.author.network.mail: finished.")

            return(dev.relation)
        },


        ## ARTIFACT NETWORKS ####

        ## co-change-based artifact network
        get.artifact.network.cochange = function() {

            logging::logdebug("get.artifact.network.cochange: starting.")

            ## do not compute anything more than once
            if (!is.null(private$artifacts.network.cochange)) {
                logging::logdebug("get.artifact.network.cochange: finished. (already existing)")
                return(private$artifacts.network.cochange)
            }

            commit2artifact = self$get.commit2artifact(network.conf = private$network.conf)
            artifacts.net = construct.dependency.network.from.list(commit2artifact, network.conf = private$network.conf)

            ## store network
            private$artifacts.network.cochange = artifacts.net
            logging::logdebug("get.artifact.network.cochange: finished.")

            return(artifacts.net)
        },

        ## call-graph-based artifact network
        ## IMPORTANT: This only works for range-level analyses! (errors otherwise)
        get.artifact.network.callgraph = function() {

            logging::logdebug("get.artifact.network.callgraph: starting.")

            ## do not compute anything more than once
            if (!is.null(private$artifacts.network.callgraph)) {
                logging::logdebug("get.artifact.network.callgraph: finished. (already existing)")
                return(private$artifacts.network.callgraph)
            }

            ## check if revision for call-graphs is set
            if (is.na(private$revision.callgraph)) {
                logging::logerror("The call-graph revision is not set. Aborting...")
                logging::logerror("This may be due to project-level analysis.
                                   The call-graph data is only available in range-level analysis.")
                stop("Trying to get call-graph data before setting a revision.")
            }

            ## construct path and file
            file.dir = self$get.data.path.callgraph()
            file.name = paste0("cg_nw_", private$conf$get.artifact.short(), "_", private$revision.callgraph, ".net")
            file = file.path(file.dir, file.name)

            ## read network from disk
            artifacts.net = read.network.from.file(file)
            ## post-process network
            artifacts.net = postprocess.artifact.names.callgraph(artifacts.net, private$conf$get.artifact())

            ## store network
            private$artifacts.network.callgraph = artifacts.net
            logging::logdebug("get.artifact.network.callgraph: finished.")

            return(artifacts.net)
        }

    ),


    ## public members ####
    public = list(

        ## constructor
        initialize = function(conf) {
            if (!missing(conf) && "CodefaceConf" %in% class(conf)) {
                private$conf = conf
            }

            if (class(self)[1] == "CodefaceProjectData")
                logging::loginfo("Initialized data object %s", self$get.class.name())
        },


        ## TO STRING ;) ####

        get.class.name = function() {
            return(
                sprintf("CodefaceProjectData<%s>", private$conf$get.repo())
            )
        },


        ## CONFIGURATION ####

        get.conf = function() {
            return(private$conf)
        },


        ## BACKUP ####

        save.to.disk = function(file) {
            save(self, file = file)
        },


        ## PATHS ####

        ## construct the absolute path to the project's result folder
        get.data.path = function() {
            data.path = private$conf$get.datapath()
            return(data.path)
        },

        ## construct the absolute path to the range's result folder for synchronicity data
        get.data.path.synchronicity = function() {
            data.path = private$conf$get.datapath.synchronicity()
            return(data.path)
        },


        ## RAW DATA ####

        ## get the complete filtered list of commits
        get.commits = function() {
            logging::loginfo("Getting commit data.")

            ## if commits are not read already, do this
            if (is.null(private$commits)) {
                private$read.commits(network.conf = private$network.conf)
            }

            return(private$commits)
        },

        ## get the complete raw list of commits
        get.commits.raw = function() {
            logging::loginfo("Getting raw commit data.")

            ## if commits are not read already, do this
            if (is.null(private$commits.raw)) {
                private$read.commits.raw(network.conf = private$network.conf)
            }

            return(private$commits.raw)
        },

        ## get the complete synchronicity data
        get.synchronicity = function(time.window = c(5)) {
            logging::loginfo("Getting synchronicity data.")

            ## if commits are not read already, do this
            if (is.null(private$synchronicity)) {
                private$read.synchronicity()
            }

            return(private$synchronicity)
        },

        ## get the complete list of mails
        get.mails = function() {
            logging::loginfo("Getting e-mail data.")

            ## if mails are not read already, do this
            if (is.null(private$mails)) {
                private$read.mails()
            }

            return(private$mails)
        },

        ## get the ID--author mapping
        get.authors = function() {
            logging::loginfo("Getting author data.")

            ## if authors are not read already, do this
            if (is.null(private$authors)) {
                private$read.authors()
            }

            return(private$authors)
        },


        ## DATA ####

        ## get the authors for each artifact
        get.artifact2author = function() {
            logging::loginfo("Getting artifact--author data.")

            ## get commits sorted by date
            sorted.commits = self$get.commits(network.conf = private$network.conf)
            sorted.commits = sorted.commits[order(sorted.commits[["date"]], decreasing = FALSE), ] # sort!

            ## store the authors per artifact
            mylist = get.thing2thing(sorted.commits, "artifact", "author.name", network.conf = private$network.conf
                                     # extra.data = c(extra.data)
                                     )

            return(mylist)
        },

        ## get the commits for each author
        get.author2commit = function() {
            logging::loginfo("Getting author--commit data.")

            ## if commits are not read already, do this
            if (is.null(private$commits.raw)) {
                private$read.commits.raw(network.conf = private$network.conf)
            }

            ## store the authors per artifact
            mylist = get.thing2thing(private$commits.raw, "author.name", "hash", network.conf = private$network.conf)
            mylist = mclapply(mylist, unique)

            return(mylist)
        },

        ## get the artifacts for each author
        ## (formerly Author2ArtifactExtraction, authors2{artifact}.list)
        get.author2artifact = function() {
            logging::loginfo("Getting author--artifact data.")

            ## if commits are not read already, do this
            if (is.null(private$commits)) {
                private$read.commits(network.conf = private$network.conf)
            }

            ## store the authors per artifact
            mylist = get.thing2thing(private$commits, "author.name", "artifact", network.conf = private$network.conf)

            return(mylist)
        },

        ## get the files for each author
        ## (formerly Author2FileExtraction, authors2file.list)
        get.author2file = function() {
            logging::loginfo("Getting author--file data.")

            ## if commits are not read already, do this
            if (is.null(private$commits)) {
                private$read.commits(network.conf = private$network.conf)
            }

            ## store the authors per artifact
            mylist = get.thing2thing(private$commits, "author.name", "file")

            return(mylist)
        },

        ## get the artifacts for each commits
        ## (formerly Commit2ArtifactExtraction, commit2artifact.list)
        get.commit2artifact = function() {
            logging::loginfo("Getting commit--artifact data.")

            ## if commits are not read already, do this
            if (is.null(private$commits)) {
                private$read.commits(network.conf = private$network.conf)
            }

            ## store the authors per artifact
            mylist = get.thing2thing(private$commits, "hash", "artifact", network.conf = private$network.conf)

            return(mylist)
        },

        ## get the files for each commits
        ## (formerly Commit2FileExtraction, commit2file.list)
        get.commit2file = function() {
            logging::loginfo("Getting commit--file data.")

            ## if commits are not read already, do this
            if (is.null(private$commits)) {
                private$read.commits(network.conf = private$network.conf)
            }

            ## store the authors per artifact
            mylist = get.thing2thing(private$commits, "hash", "file")

            return(mylist)
        },

        ## get the authors for each mail thread
        ## (formerly Thread2AuthorExtraction, thread2author.list)
        get.thread2author = function(# extra.data = c()
          ) {
            logging::loginfo("Getting thread--author data.")

            ## if mails are not read already, do this
            if (is.null(private$mails)) {
                private$read.mails()
            }

            ## store the authors per thread
            mylist = get.thing2thing(private$mails, "thread", "author.name",
                                     network.conf = private$network.conf)

            return(mylist)
        },

        ## get the developer relation as network (generic)
        get.author.network = function() {
            logging::loginfo("Constructing author network.")
            relation = network.conf$get.variable("author.relation")
            if (relation == "cochange")
                return(private$get.author.network.cochange(network.conf = private$network.conf))
            else if (relation == "mail")
                return(private$get.author.network.mail(network.conf = private$network.conf))
            else
                stop(sprintf("The author relation '%s' does not exist.", relation))
        },

        ## get artifact relation as network (generic)
        get.artifact.network = function() {
            logging::loginfo("Constructing artifact network.")
            relation = network.conf$get.variablo("artifact.relation")
            if (relation == "cochange")
                return(private$get.artifact.network.cochange(network.conf = private$network.conf))
            else if (relation == "callgraph")
                return(private$get.artifact.network.callgraph())
            else
                stop(sprintf("The artifact relation '%s' does not exist.", relation))
        },

        ## get all networks (build unification to avoid null-pointers)
        get.networks = function() {
            logging::loginfo("Constructing all networks.")

            ## get method arguments
            author.relation = network.conf$get.variable("author.relation")
            artifact.relation = network.conf$get.variable("artifact.relation")

            ## authors-artifact relation
            authors.to.artifacts = self$get.author2artifact(network.conf = private$network.conf)

            ## authors relation
            authors.net = self$get.author.network(network.conf = private$network.conf)

            ## unify vertices with developer-artifact relation
            authors.from.net = get.vertex.attribute(authors.net, "name")
            authors.from.artifacts = names(authors.to.artifacts)
            authors.net = authors.net + vertices(setdiff(authors.from.artifacts, authors.from.net))

            ## remove all authors from the corresponding network who do not have touched any artifact
            if (author.only.committers & !is.null(authors.from.artifacts))
                authors.net = delete.vertices(authors.net, setdiff(authors.from.net, authors.from.artifacts))

            ## artifact relation
            artifacts.net = self$get.artifact.network(network.conf = private$network.conf)
            # merge vertices on artifact network to avoid NULL references
            artifacts.net = unify.artifact.vertices(artifacts.net, authors.to.artifacts)
            # ## compute communities # TODO in the end, this needs to read Thomas' files!
            # artifacts.comm = get.communities(artifact.net)

            return(list(
                "authors.to.artifacts" = authors.to.artifacts,
                "authors.net" = authors.net,
                "artifacts.net" = artifacts.net
            ))
        },

        ## get the bipartite networks (get.networks combined in one network)
        get.bipartite.network = function(
          # simple.network = TRUE, contract.edges = simple.network,
          #                                artifact.extra.edge.attr = c("date", "hash"), ...
          ) {
            logging::loginfo("Constructing bipartite network.")

            networks = self$get.networks(network.conf = private$network.conf)

            authors.to.artifacts = networks[["authors.to.artifacts"]]
            authors.net = networks[["authors.net"]]
            artifacts.net = networks[["artifacts.net"]]

            if (is.directed(authors.net) && !is.directed(artifacts.net)) {
                logging::logwarn("Author network is directed, but artifact network is not. Converting artifact network...")
                artifacts.net = as.directed(artifacts.net, mode = "mutual")
            } else if (!is.directed(authors.net) && is.directed(artifacts.net)) {
                logging::logwarn("Author network is undirected, but artifact network is not. Converting artifact network...")
                contraction.mode = "collapse"
                if (!contract.edges) contraction.mode = "each"
                artifacts.net = as.undirected(artifacts.net, mode = contraction.mode, edge.attr.comb = EDGE.ATTR.HANDLING)
            }

            u = combine.networks(authors.net, artifacts.net, authors.to.artifacts,
                                 network.conf = private$network.conf)

            return(u)

        }

        ## FIXME split data by three-month ranges (e.g., given by argument (four months, ten days, ...))

    )
)

## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## CodefaceRangeData
##
## Represents the data for one revision range on Codeface Data

#### CodefaceRangeData ####
CodefaceRangeData = R6Class("CodefaceRangeData",

    inherit = CodefaceProjectData,

    ## private members ####
    private = list(
        range = NULL, # character
        revision.callgraph = NA # character
    ),

    ## public members ####
    public = list(

        ## constructor
        initialize = function(conf, range, revision.callgraph = "") {
            ## call super constructor
            super$initialize(conf)

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
                sprintf("CodefaceRangeData<%s, %s, %s>", private$conf$get.repo(), private$range, private$revision.callgraph)
            )
        },


        ## PATHS ####

        ## construct the absolute path to the range's result folder
        get.data.path = function() {
            data.path = private$conf$get.datapath()
            range = private$range
            return(file.path(data.path, range))
        },

        ## construct the absolute path to the range's result folder for callgraphs
        get.data.path.callgraph = function() {
            data.path = file.path(private$conf$get.datapath.callgraph(), private$revision.callgraph)
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


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Union of networks
##

## combine networks to a bipartite network
combine.networks = function(authors.net, artifacts.net, authors.to.artifacts, simple.network = TRUE,
                            extra.data = c()) {

    authors = vertex_attr(authors.net, "name")
    artifacts = vertex_attr(artifacts.net, "name")

    ## check emptiness of networks
    if (length(authors) == 0) {
        logging::logwarn("Author network is empty.")
    }
    if (length(artifacts) == 0) {
        logging::logwarn("Artifact network is empty.")
    }

    ## combine networks
    u = igraph::disjoint_union(authors.net, artifacts.net)

    ## set vertex and edge attributes for identifaction
    V(u)[ name %in% authors ]$type = TYPE.AUTHOR
    V(u)[ name %in% artifacts ]$type = TYPE.ARTIFACT
    E(u)$type = TYPE.EDGES.INTRA

    ## add edges for devs.to.arts relation
    u = add.edges.for.devart.relation(u, authors.to.artifacts, extra.data = extra.data)

    ## FIXME simplify + as.undirected yield list of lists for date attributes (probably also others)

    ## simplify network
    if (simple.network)
        u = simplify.network(u)

    return(u)
}


## helper function to add dependencies from dev--art mapping to the bipartite network
add.edges.for.devart.relation = function(net, auth.to.arts, edge.type = TYPE.EDGES.INTER, extra.data = c()) {

    # construct edges (i.e., a vertex sequence with c(source, target, source, target, ...))
    vertex.sequence.for.edges = mcmapply(function(d, a.df) {
        a = a.df[["artifact"]]
        new.edges = lapply(a, function(art) {
            V(net)[d, art] # get two vertices from source network:  c(developer, artifact)
        })
        return(new.edges)
    }, names(auth.to.arts), auth.to.arts)

    ## get extra edge attributes
    extra.edge.attributes.df = mclapply(auth.to.arts, function(a.df) {
        return(a.df[, extra.data, drop = FALSE])
    })
    extra.edge.attributes.df = rbind.fill(extra.edge.attributes.df)
    extra.edge.attributes.df["weight"] = 1 # add weight

    extra.edge.attributes = as.list(extra.edge.attributes.df)

    ## set edge type
    extra.edge.attributes = c(extra.edge.attributes, list(type = edge.type))

    ## add the vertex sequences as edges to the network
    new.net = add_edges(net, unlist(vertex.sequence.for.edges), attr = extra.edge.attributes)

    return(new.net)

}
