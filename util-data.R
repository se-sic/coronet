## (c) Claus Hunsen, 2016, 2017
## hunsen@fim.uni-passau.de
## (c) Raphael Nömmer, 2017
## noemmer@fim.uni-passau.de
## (c) Christian Hechtl, 2017
## hechtl@fim.uni-passau.de


## libraries
requireNamespace("R6") # for R6 classes
requireNamespace("igraph") # networks
requireNamespace("plyr") # for dlply function
requireNamespace("sqldf") # for sqldf
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
## CodefaceProjectData
##
## Represents the data for one revision range on Codeface Data

#### CodefaceProjectData ####
CodefaceProjectData = R6::R6Class("CodefaceProjectData",

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
        pasta = NULL, # list
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

        #read the commits without empty artifacts
        read.commits.filtered.empty = function() {

            logging::logdebug("read.commits.filtered.empty: starting.")

            ## do not compute anything more than once
            if (!is.null(private$commits.filtered.empty)) {
                logging::logdebug("read.commits.filtered.empty: finished. (already existing)")
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
            logging::logdebug("read.commits.filtered.empty: finished.")
        },

        ## read the base filtered commit data for the range
        read.commits.filtered = function() {

            logging::logdebug("read.commits.filtered: starting.")

            ## do not compute anything more than once
            if (!is.null(private$commits.filtered)) {
                logging::logdebug("read.commits.filtered: finished. (already existing)")
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
            logging::logdebug("read.commits.filtered: finished.")

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

            artifact2author = self$get.artifact2author()
            author.net = construct.dependency.network.from.list(artifact2author, network.conf = private$network.conf,
                                                                directed = private$network.conf$get.variable("author.directed"))


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
            thread2author = self$get.thread2author()

            if (length(thread2author) != 0) {
                dev.relation =

                    construct.dependency.network.from.list(thread2author, network.conf = private$network.conf,
                                                           directed = private$network.conf$get.variable("author.directed"))

            } else {
                dev.relation = create.empty.network(private$network.conf$get.variable("author.directed"))
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

            commit2artifact = self$get.commit2artifact()
            artifacts.net = construct.dependency.network.from.list(commit2artifact, network.conf = private$network.conf,
                                                                   directed = FALSE)

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
            file.name = paste0("cg_nw_", private$project.conf$get.entry("artifact.short"), "_", private$revision.callgraph, ".net")
            file = file.path(file.dir, file.name)

            ## read network from disk
            artifacts.net = read.network.from.file(file)
            ## post-process network
            artifacts.net = postprocess.artifact.names.callgraph(artifacts.net, private$project.conf$get.entry("artifact"))

            ## store network
            private$artifacts.network.callgraph = artifacts.net
            logging::logdebug("get.artifact.network.callgraph: finished.")

            return(artifacts.net)
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

            if (class(self)[1] == "CodefaceProjectData")
                logging::loginfo("Initialized data object %s", self$get.class.name())
        },


        ## TO STRING ;) ####

        get.class.name = function() {
            return(
                sprintf("CodefaceProjectData<%s>", private$project.conf$get.entry("repo"))
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
          private$authors.network.mail = NULL
          private$authors.network.cochange = NULL
          private$artifacts.network.cochange = NULL
          private$artifacts.network.callgraph = NULL
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

        # Get the current network configuration
        get.network.conf = function() {
            return(private$network.conf)
        },

        # Set the current network configuration to the given one.
        set.network.conf = function(network.conf) {
          private$network.conf = network.conf
          self$reset.environment()
        },

        ## UPDATE CONFIGURATION ####
        update.network.conf = function(updated.values = list()) {
          private$network.conf$update.values(updated.values = updated.values)
          self$reset.environment()
        },

        # for testing reasons
        # might be used for other purposes
        get.network.conf.variable = function(var.name) {
            return(private$network.conf$get.variable(var.name))
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
                private$read.commits.filtered.empty()
            }

            return(private$commits.filtered.empty)
        },

        ## get the complete filtered list of commits
        get.commits.filtered = function() {
            logging::loginfo("Getting commit data filtered by artifact.base.")

            ## if commits are not read already, do this
            if (is.null(private$commits.filtered)) {
                private$read.commits.filtered()
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

        ## get the commits for each author
        get.author2commit = function() {
            logging::loginfo("Getting author--commit data.")

            ## if commits are not read already, do this
            if (is.null(private$commits.raw)) {
                private$read.commits.raw()
            }

            ## store the authors per artifact
            mylist = get.thing2thing(private$commits.raw, "author.name", "hash", network.conf = private$network.conf)
            mylist = parallel::mclapply(mylist, unique)

            return(mylist)
        },

        ## get the artifacts for each author
        ## (formerly Author2ArtifactExtraction, authors2{artifact}.list)
        get.author2artifact = function() {
            logging::loginfo("Getting author--artifact data.")

            #get commits
            commits = self$get.commits.filtered.empty()

            ## store the authors per artifact
            mylist = get.thing2thing(commits, "author.name", "artifact", network.conf = private$network.conf)

            return(mylist)
        },

        ## get the files for each author
        ## (formerly Author2FileExtraction, authors2file.list)
        get.author2file = function() {
            logging::loginfo("Getting author--file data.")

            #get commits
            commits = self$get.commits.filtered.empty()

            ## store the authors per artifact
            mylist = get.thing2thing(commits, "author.name", "file", network.conf = private$network.conf)

            return(mylist)
        },

        ## get the artifacts for each commits
        ## (formerly Commit2ArtifactExtraction, commit2artifact.list)
        get.commit2artifact = function() {
            logging::loginfo("Getting commit--artifact data.")

            #get commits
            commits = self$get.commits.filtered()

            ## store the authors per artifact
            mylist = get.thing2thing(commits, "hash", "artifact", network.conf = private$network.conf)

            return(mylist)
        },

        ## get the files for each commits
        ## (formerly Commit2FileExtraction, commit2file.list)
        get.commit2file = function() {
            logging::loginfo("Getting commit--file data.")

            #get commits
            commits = self$get.commits.filtered()

            ## store the authors per artifact
            mylist = get.thing2thing(commits, "hash", "file", network.conf = private$network.conf)

            return(mylist)
        },

        ## get the authors for each mail thread
        ## (formerly Thread2AuthorExtraction, thread2author.list)
        get.thread2author = function() {
            logging::loginfo("Getting thread--author data.")

            ## if mails are not read already, do this
            if (is.null(private$mails)) {
                self$get.mails()
            }

            ## store the authors per thread
            mylist = get.thing2thing(private$mails, "thread", "author.name",
                                     network.conf = private$network.conf)

            return(mylist)
        },

        ## get the developer relation as network (generic)
        get.author.network = function() {
            logging::loginfo("Constructing author network.")

            ## construct network
            relation = private$network.conf$get.variable("author.relation")
            net = switch(
                relation,
                cochange =
                    private$get.author.network.cochange(),
                mail =
                    private$get.author.network.mail(),
                stop(sprintf("The author relation '%s' does not exist.", relation))
            )

            ## set vertex and edge attributes for identifaction
            igraph::V(net)$type = TYPE.AUTHOR
            igraph::E(net)$type = TYPE.EDGES.INTRA

            return(net)
        },

        ## get artifact relation as network (generic)
        get.artifact.network = function() {
            logging::loginfo("Constructing artifact network.")

            ## construct network
            relation = private$network.conf$get.variable("artifact.relation")

            net = switch(
                relation,
                cochange =
                    private$get.artifact.network.cochange(),
                callgraph =
                    private$get.artifact.network.callgraph(),
                stop(sprintf("The artifact relation '%s' does not exist.", relation))
            )

            ## set vertex and edge attributes for identifaction
            igraph::V(net)$type = TYPE.ARTIFACT
            igraph::E(net)$type = TYPE.EDGES.INTRA

            return(net)
        },

        ## get the (real) bipartite network
        get.bipartite.network = function() {
            ## authors-artifact relation
            authors.to.artifacts = self$get.author2artifact()

            ## extract vertices
            authors = names(authors.to.artifacts)
            artifacts = self$get.artifacts()

            ## construct networks from vertices
            authors.net = create.empty.network(directed = FALSE) +
                igraph::vertices(authors, name = authors, type = TYPE.AUTHOR)
            artifacts.net = create.empty.network(directed = FALSE) +
                igraph::vertices(artifacts, name = artifacts, type = TYPE.ARTIFACT)

            ## combine the networks
            u = combine.networks(authors.net, artifacts.net, authors.to.artifacts,
                                 network.conf = private$network.conf)
            return(u)
        },

        ## get all networks (build unification to avoid null-pointers)
        get.networks = function() {
            logging::loginfo("Constructing all networks.")

            ## authors-artifact relation
            authors.to.artifacts = self$get.author2artifact()

            ## bipartite network
            bipartite.net = self$get.bipartite.network()

            ## authors relation
            authors.net = self$get.author.network()

            ## unify vertices with developer-artifact relation
            authors.from.net = igraph::get.vertex.attribute(authors.net, "name")
            authors.from.artifacts = names(authors.to.artifacts)
            authors.net = authors.net + igraph::vertices(setdiff(authors.from.artifacts, authors.from.net), type = TYPE.AUTHOR)

            ## remove all authors from the corresponding network who do not have touched any artifact
            if (private$network.conf$get.variable("author.only.committers") & !is.null(authors.from.artifacts)) {
                authors.net = igraph::delete.vertices(authors.net, setdiff(authors.from.net, authors.from.artifacts))
            }

            ## artifact relation
            artifacts.net = self$get.artifact.network()
            # merge vertices on artifact network to avoid NULL references
            artifacts.net = unify.artifact.vertices(artifacts.net, authors.to.artifacts)
            # ## compute communities # TODO in the end, this needs to read Thomas' files!
            # artifacts.comm = get.communities(artifact.net)

            return(list(
                "authors.to.artifacts" = authors.to.artifacts,
                "bipartite.net" = bipartite.net,
                "authors.net" = authors.net,
                "artifacts.net" = artifacts.net
            ))
        },

        ## get the multi networks (get.networks combined in one network)
        get.multi.network = function() {
            logging::loginfo("Constructing multi network.")

            ## construct the network parts we need for the multi network
            networks = self$get.networks()
            authors.to.artifacts = networks[["authors.to.artifacts"]]
            authors.net = networks[["authors.net"]]
            artifacts.net = networks[["artifacts.net"]]

            ## check directedness and adapt artifact network if needed
            if (igraph::is.directed(authors.net) && !igraph::is.directed(artifacts.net)) {
                logging::logwarn("Author network is directed, but artifact network is not. Converting artifact network...")
                artifacts.net = igraph::as.directed(artifacts.net, mode = "mutual")
            } else if (!igraph::is.directed(authors.net) && igraph::is.directed(artifacts.net)) {
                logging::logwarn("Author network is undirected, but artifact network is not. Converting artifact network...")
                contraction.mode = ifelse(
                    private$network.conf$get.variable("contract.edges"),
                    "collapse",
                    "each"
                )
                artifacts.net = igraph::as.undirected(artifacts.net, mode = contraction.mode, edge.attr.comb = EDGE.ATTR.HANDLING)
            }

            ## reduce memory consumption by removing temporary data
            rm(networks)
            gc()

            ## combine the networks
            u = combine.networks(authors.net, artifacts.net, authors.to.artifacts,
                                 network.conf = private$network.conf)

            return(u)
        }

    )
)

## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## CodefaceRangeData
##
## Represents the data for one revision range on Codeface Data

#### CodefaceRangeData ####
CodefaceRangeData = R6::R6Class("CodefaceRangeData",

    inherit = CodefaceProjectData,

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
                sprintf("CodefaceRangeData<%s, %s, %s>",
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


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Union of networks
##

## combine networks to a bipartite network
combine.networks = function(authors.net, artifacts.net, authors.to.artifacts, network.conf) {

    authors = igraph::get.vertex.attribute(authors.net, "name")
    artifacts = igraph::get.vertex.attribute(artifacts.net, "name")

    ## check emptiness of networks
    if (length(authors) == 0) {
        logging::logwarn("Author network is empty.")
    }
    if (length(artifacts) == 0) {
        logging::logwarn("Artifact network is empty.")
    }

    ## combine networks
    u = igraph::disjoint_union(authors.net, artifacts.net)

    ## add edges for devs.to.arts relation
    u = add.edges.for.devart.relation(u, authors.to.artifacts, network.conf = network.conf)

    ## simplify network
    if (network.conf$get.variable("simplify"))
        u = simplify.network(u)

    return(u)
}


## helper function to add dependencies from dev--art mapping to the bipartite network
add.edges.for.devart.relation = function(net, auth.to.arts, network.conf) {

    # construct edges (i.e., a vertex sequence with c(source, target, source, target, ...))
    vertex.sequence.for.edges = parallel::mcmapply(function(d, a.df) {
        a = a.df[["artifact"]]
        new.edges = lapply(a, function(art) {
            igraph::V(net)[d, art] # get two vertices from source network:  c(developer, artifact)
        })
        return(new.edges)
    }, names(auth.to.arts), auth.to.arts)

    ## get extra edge attributes
    extra.edge.attributes.df = parallel::mclapply(auth.to.arts, function(a.df) {
        cols.which = network.conf$get.variable("edge.attributes") %in% colnames(a.df)
        return(a.df[, network.conf$get.variable("edge.attributes")[cols.which], drop = FALSE])
    })
    extra.edge.attributes.df = plyr::rbind.fill(extra.edge.attributes.df)
    extra.edge.attributes.df["weight"] = 1 # add weight

    extra.edge.attributes = as.list(extra.edge.attributes.df)

    ## set edge type
    extra.edge.attributes = c(extra.edge.attributes, list(type = TYPE.EDGES.INTER))

    ## add the vertex sequences as edges to the network
    new.net = igraph::add_edges(net, unlist(vertex.sequence.for.edges), attr = extra.edge.attributes)

    return(new.net)

}
