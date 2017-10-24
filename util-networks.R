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
requireNamespace("plyr") # for dlply function
requireNamespace("igraph") # networks


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Vertex and edge types ---------------------------------------------------

## node types
TYPE.AUTHOR = 1
TYPE.ARTIFACT = 2

# edge types
TYPE.EDGES.INTRA = 3
TYPE.EDGES.INTER = 4


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Edge-attribute handling during simplification ---------------------------

## Edge-attribute contraction: configure handling of attributes by name
EDGE.ATTR.HANDLING = list(
    ## network-analytic data
    weight = "sum",
    type = "first",

    ## commit data
    changed.files = "sum",
    added.lines = "sum",
    deleted.lines = "sum",
    diff.size = "sum",
    artifact.diff.size = "sum",

    ## everything else
    "concat"
)


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## NetworkBuilder ----------------------------------------------------------

NetworkBuilder = R6::R6Class("NetworkBuilder",

    ## * private -----------------------------------------------------------

    private = list(
        ## * * data and configuration --------------------------------------

        proj.data = NULL,
        proj.data.original = NULL,
        network.conf = NULL,

        ## * * network caching ---------------------------------------------

        authors.network.mail = NULL, # igraph
        authors.network.cochange = NULL, # igraph
        authors.network.issue = NULL, #igraph
        artifacts.network.cochange = NULL, # igraph
        artifacts.network.callgraph = NULL, # igraph
        artifacts.network.mail = NULL, # igraph
        artifacts.network.issue = NULL, # igraph

        ## * * data cutting ---------------------------------------------


        #' Cut the data sources of the data object to the same date ranges.
        cut.data.to.same.timestamps = function() {
            cut.data = private$proj.data$get.data.cut.to.same.date(data.sources = private$get.data.sources())
            private$proj.data = cut.data
        },

        #' Determine which data sources should be cut depending on the artifact and author relation.
        #'
        #' @return the data sources to be cut
        get.data.sources = function() {
            author.relation = private$network.conf$get.value("author.relation")
            artifact.relation = private$network.conf$get.value("artifact.relation")
            data.sources = unique(c(RELATION.TO.DATASOURCE[[author.relation]],
                                    RELATION.TO.DATASOURCE[[artifact.relation]]))
            return(data.sources)
        },

        ## * * author networks ---------------------------------------------

        #' Get the co-change-based author relation as network.
        #' If it doesn´t already exist build it first.
        #'
        #' @return the author network with cochange relation
        get.author.network.cochange = function() {
            logging::logdebug("get.author.network.cochange: starting.")

            ## do not compute anything more than once
            if (!is.null(private$authors.network.cochange)) {
                logging::logdebug("get.author.network.cochange: finished. (already existing)")
                return(private$authors.network.cochange)
            }

            ## construct network based on artifact2author data
            author.net = construct.network.from.list(
                private$proj.data$get.artifact2author(),
                network.conf = private$network.conf,
                directed = private$network.conf$get.value("author.directed")
            )

            ## store network
            private$authors.network.cochange = author.net
            logging::logdebug("get.author.network.cochange: finished.")

            return(author.net)
        },

        #' Get the thread-based author relation as network.
        #' If it doesn´t already exist build it first.
        #'
        #' @return the author network with mail relation
        get.author.network.mail = function() {

            logging::logdebug("get.author.network.mail: starting.")

            ## do not compute anything more than once
            if (!is.null(private$authors.network.mail)) {
                logging::logdebug("get.author.network.mail: finished. (already existing)")
                return(private$authors.network.mail)
            }

            author.relation = construct.network.from.list(
                private$proj.data$get.thread2author(),
                network.conf = private$network.conf,
                directed = private$network.conf$get.value("author.directed")
            )

            ## store network
            private$authors.network.mail = author.relation
            logging::logdebug("get.author.network.mail: finished.")

            return(author.relation)
        },

        ##get the issue based author relation as network
        get.author.network.issue = function() {
            logging::logdebug("get.author.network.issue: starting.")

            if(!is.null(private$authors.network.issue)) {
                logging::logdebug("get.author.network.issue: finished. (already existing)")
                return(private$authors.network.issue)
            }

            author.relation = construct.network.from.list(
                private$proj.data$get.issue2author(),
                network.conf = private$network.conf,
                directed = private$network.conf$get.value("author.directed")
            )

            private$authors.network.issue = author.relation
            logging::logdebug("get.author.network.issue: finished.")

            return(author.relation)
        },

        ## * * artifact networks -------------------------------------------

        #' Get the co-change-based artifact network,
        #' If it doesn´t already exist build it first.
        #'
        #' @return the artifact network with cochange realtion
        get.artifact.network.cochange = function() {

            logging::logdebug("get.artifact.network.cochange: starting.")

            ## do not compute anything more than once
            if (!is.null(private$artifacts.network.cochange)) {
                logging::logdebug("get.artifact.network.cochange: finished. (already existing)")
                return(private$artifacts.network.cochange)
            }

            artifacts.net = construct.network.from.list(
                private$proj.data$get.commit2artifact(),
                network.conf = private$network.conf,
                directed = FALSE
            )

            ## store network
            private$artifacts.network.cochange = artifacts.net
            logging::logdebug("get.artifact.network.cochange: finished.")

            return(artifacts.net)
        },

        #' Get the call-graph-based artifact network.
        #' If it doesn´t already exist build it first.
        #' IMPORTANT: This only works for range-level analyses!
        #'
        #' @return the artifact network with callgraph relation
        get.artifact.network.callgraph = function() {

            logging::logdebug("get.artifact.network.callgraph: starting.")

            ## do not compute anything more than once
            if (!is.null(private$artifacts.network.callgraph)) {
                logging::logdebug("get.artifact.network.callgraph: finished. (already existing)")
                return(private$artifacts.network.callgraph)
            }

            ## check if revision for call-graphs is set
            if (is.na(private$proj.data$get.revision.callgraph())) {
                logging::logerror("The call-graph revision is not set. Aborting...")
                logging::logerror("This may be due to project-level analysis.
                                  The call-graph data is only available in range-level analysis.")
                stop("Trying to get call-graph data before setting a revision.")
            }

            ## construct path and file
            file.dir = private$proj.data$get.data.path.callgraph()
            file.name = sprintf("cg_nw_%s_%s.net",
                                private$proj.data$get.project.conf.entry("artifact.short"),
                                private$proj.data$get.revision.callgraph())
            file = file.path(file.dir, file.name)

            ## read network from disk
            artifacts.net = igraph::read.graph(file, format = "pajek")
            # set vertex labels properly (copy "id" attribute to "name" attribute)
            artifacts.net = igraph::set.vertex.attribute(
                artifacts.net,
                "name",
                igraph::V(artifacts.net),
                igraph::get.vertex.attribute(artifacts.net, "id")
            )

            ## process vertex names in artifact networks for consistent names:
            ## Since feature and file networks may have unique naming structures, the names
            ## need to be processed in order to match the ones coming from other analyses
            ## (e.g. Codeface):
            ## (1) retrieve parameters for processing
            names = igraph::get.vertex.attribute(artifacts.net, "name")
            artifact = private$proj.data$get.project.conf.entry("artifact")
            ## (2) different replacings for different artifacts
            ## feature
            if (artifact == "feature") {
                names = gsub("^CONFIG_", "ENABLE_", names) # BusyBox
                names = gsub("^1$", "Base_Feature", names) # Base feature
            }
            ## file
            else if (artifact == "file" || artifact == "function") {
                ## transform to relative paths
                names = gsub("^/local/bockthom/TypeChef-BusyboxAnalysis/gitbusybox/", "", names) # BusyBox
                names = gsub("^/local/bockthom/openssl/", "", names) # OpenSSL
                names = gsub("^/local/bockthom/sqlite/\\./", "", names) # SQLite
                names = gsub("^/local/bockthom/sqlite/", "", names) # SQLite

                ## remove call-graph extension
                names = gsub(".cg", "", names, fixed = TRUE)
            }
            ## (3) set processed names inside graph object
            artifacts.net = igraph::set.vertex.attribute(artifacts.net, "name", value = names)
            artifacts.net = igraph::set.vertex.attribute(artifacts.net, "id", value = names)

            ## store network
            private$artifacts.network.callgraph = artifacts.net
            logging::logdebug("get.artifact.network.callgraph: finished.")

            return(artifacts.net)
        },

        #' Get the mail-based artifact network.
        #' If it doesn´t already exist build it first.
        #'
        #' @return the artifact network with mail relation
        get.artifact.network.mail = function() {

            logging::logdebug("get.artifact.network.mail: starting.")

            ## do not compute anything more than once
            if (!is.null(private$artifacts.network.mail)) {
                logging::logdebug("get.artifact.network.mail: finished. (already existing)")
                return(private$artifacts.network.mail)
            }

            ## log warning as we do not have relations among threads right now
            logging::logwarn(paste(
                "There exist no actual artifact network with the relation 'mail'.",
                "Return an edge-less network now."
            ))

            ## construct empty network
            directed = private$network.conf$get.value("artifact.directed")
            artifacts = names(private$proj.data$get.thread2author()) # thread IDs
            artifacts.net = create.empty.network(directed = directed) +
                igraph::vertices(artifacts, type = TYPE.ARTIFACT)

            ## store network
            private$artifacts.network.mail = artifacts.net
            logging::logdebug("get.artifact.network.mail: finished.")

            return(artifacts.net)
        },

        #' Get the issue-based artifact network.
        #' If it doesn´t already exist build it first.
        #'
        #' @return the artifact network with issue relation
        get.artifact.network.issue = function() {

            logging::logdebug("get.artifact.network.issue: starting.")

            ## do not compute anything more than once
            if (!is.null(private$artifacts.network.issue)) {
                logging::logdebug("get.artifact.network.issue: finished. (already existing)")
                return(private$artifacts.network.issue)
            }

            ## log warning as we do not have relations among issues right now
            logging::logwarn(paste(
                "There exist no actual artifact network with the relation 'issue'.",
                "Return an edge-less network now."
            ))

            ## construct empty network
            directed = private$network.conf$get.value("artifact.directed")
            artifacts = names(private$proj.data$get.issue2author()) # thread IDs
            artifacts.net = create.empty.network(directed = directed) +
                igraph::vertices(artifacts, type = TYPE.ARTIFACT)

            ## store network
            private$artifacts.network.issue = artifacts.net
            logging::logdebug("get.artifact.network.issue: finished.")

            return(artifacts.net)
        },

        ## * * bipartite relation ------------------------------------------

        #' Get the key-value data for the bipartite relation,
        #' which is implied by the "artifact.relation" from the network configuration.
        #'
        #' @return the data for the bipartite relation, with the attribute
        #'         'artifact.type' denoting the artifact type for the relation
        get.bipartite.relation = function() {
            logging::logdebug("get.bipartite.relation: starting.")

            relation = private$network.conf$get.variable("artifact.relation")
            logging::logdebug("Using bipartite relation '%s'.", relation)

            switch(
                relation,
                cochange = {
                    bip.relation = private$proj.data$get.author2artifact()
                    artifact.type =private$proj.data$get.project.conf.entry("artifact")
                },
                callgraph = {
                    bip.relation = private$proj.data$get.author2artifact()
                    artifact.type =private$proj.data$get.project.conf.entry("artifact")
                },
                mail = {
                    bip.relation = private$proj.data$get.author2thread()
                    artifact.type = "thread"
                },
                issue = {
                    bip.relation = private$proj.data$get.author2issue()
                    artifact.type = "issue.id"
                }
            )

            ## set artifact.type attribute
            attr(bip.relation, "artifact.type") = artifact.type

            logging::logdebug("get.bipartite.relation: finished.")
            return(bip.relation)
        }

    ),

    ## * * public ----------------------------------------------------------

    public = list(

        #' Constructor of the class. Constructs a new instance based on the
        #' given data object and the network configuration
        #'
        #' @param project.data the given data object
                  #' @param network.conf the network configuration
        initialize = function(project.data, network.conf) {
            private$proj.data = project.data
            private$proj.data.original = project.data

            if(!missing(network.conf) && "NetworkConf" %in% class(network.conf)) {
                private$network.conf = network.conf
            }

            if (class(self)[1] == "ProjectData")
                logging::loginfo("Initialized data object %s", self$get.class.name())

            if(private$network.conf$get.value("unify.date.ranges")) {
                private$cut.data.to.same.timestamps()
            }
        },

        ## * * resetting environment ---------------------------------------

        #' Reset the current environment in order to rebuild it.
        #' Has to be called whenever the data or configuration get changed.
        reset.environment = function() {
            private$authors.network.mail = NULL
            private$authors.network.cochange = NULL
            private$authors.network.issue = NULL
            private$artifacts.network.cochange = NULL
            private$artifacts.network.callgraph = NULL
            private$proj.data = private$proj.data.original
            if(private$network.conf$get.value("unify.date.ranges")) {
                private$cut.data.to.same.timestamps()
            }
        },

        ## * * configuration -----------------------------------------------

        #' Get the current network configuration.
        #'
        #' @return the 'network.conf' of the current instance of the class
        get.network.conf = function() {
            return(private$network.conf)
        },

        #' Set the current network configuration to the given one.
        #'
        #' @param network.conf the new network configuration.
        #' @param reset.environment parameter to determine whether the environment
        #'                          has to be reset or not
        set.network.conf = function(network.conf, reset.environment = FALSE) {
            private$network.conf = network.conf

            if (reset.environment) {
                self$reset.environment()
            }
        },

        #' Get  a value of the network configuration
        #'
        #' @return the value of the given entry name
        get.network.conf.entry = function(entry) {
            return(private$network.conf$get.value(entry))
        },

        #' Set  a value of the network configuration and reset the environment
        set.network.conf.entry = function(entry, value) {
            private$network.conf$update.value(entry, value)
        },

        #' Get the project data Object of the NetworkBuilder.
        #' This Method is mainly used for testing purposes at the moment.
        #'
        #' @return the project data object of the NetworkBuilder
        get.project.data = function() {
            return(private$proj.data)
        },

        #' Update the network configuration based on the given list
        #' of values and reset the environment afterwards
        #'
        #' @param updated.values the new values for the network configuration
        update.network.conf = function(updated.values = list()) {
            private$network.conf$update.values(updated.values = updated.values)
            self$reset.environment()
        },

        ## * * networks ----------------------------------------------------

        #' Get the generic author network.
        #'
        #' @return the generic author network
        get.author.network = function() {
            logging::loginfo("Constructing author network.")

            ## construct network
            relation = private$network.conf$get.value("author.relation")
            net = switch(
                relation,
                cochange = private$get.author.network.cochange(),
                mail = private$get.author.network.mail(),
                issue = private$get.author.network.issue(),
                stop(sprintf("The author relation '%s' does not exist.", relation))
            )

            ## add all missing authors to the network if wanted
            if (private$network.conf$get.value("author.all.authors")) {
                authors.all = private$proj.data$get.authors()[[ "author.name" ]]
                authors.net = igraph::get.vertex.attribute(net, "name")
                net = net + igraph::vertices(setdiff(authors.all, authors.net))
            }

            ## remove all authors from the corresponding network who do not have touched any artifact
            if (private$network.conf$get.value("author.only.committers")) {
                ## authors-artifact relation
                authors.from.net = igraph::get.vertex.attribute(net, "name")
                authors.from.artifacts = names(private$get.bipartite.relation())
                if (!is.null(authors.from.artifacts)) {
                    net = igraph::delete.vertices(net, setdiff(authors.from.net, authors.from.artifacts))
                }
            }

            ## set vertex and edge attributes for identifaction
            igraph::V(net)$type = TYPE.AUTHOR
            igraph::E(net)$type = TYPE.EDGES.INTRA

            return(net)
        },

        #' Get the generic artifact network.
        #'
        #' @return the generic artifact network
        get.artifact.network = function() {
            logging::loginfo("Constructing artifact network.")

            ## construct network
            relation = private$network.conf$get.value("artifact.relation")

            net = switch(
                relation,
                cochange = private$get.artifact.network.cochange(),
                callgraph = private$get.artifact.network.callgraph(),
                mail = private$get.artifact.network.mail(),
                issue = private$get.artifact.network.issue(),
                stop(sprintf("The artifact relation '%s' does not exist.", relation))
            )

            ## set vertex and edge attributes for identifaction
            igraph::V(net)$type = TYPE.ARTIFACT
            igraph::E(net)$type = TYPE.EDGES.INTRA

            return(net)
        },

        #' Get the (real) bipartite network.
        #'
        #' @return the bipartite network
        get.bipartite.network = function() {
            ## get data by the chosen relation
            net.to.net = private$get.bipartite.relation()
            artifact.type = attr(net.to.net, "artifact.type")

            ## extract vertices for author network
            if (private$network.conf$get.value("author.all.authors")) {
                authors = private$proj.data$get.authors()[[ "author.name" ]]
            } else {
                authors = names(net.to.net)
            }

            ## construct networks from vertices:
            directed = private$network.conf$get.value("author.directed")
            ## 1) author network
            authors.net = create.empty.network(directed = directed) +
                igraph::vertices(authors, name = authors, type = TYPE.AUTHOR)
            ## 2) artifact network
            artifact.vertices = unique(unlist(lapply(net.to.net, function(df) {
                return(df$data.vertices)
            })))
            artifact.net = create.empty.network(directed = directed) +
                igraph::vertices(artifact.vertices, name = artifact.vertices, type = TYPE.ARTIFACT, artifact.type = artifact.type)
            ## 3) combine both networks and bipartite relation
            u = combine.networks(authors.net, artifact.net, net.to.net, network.conf = private$network.conf)

            ## remove vertices that are not committers if wanted
            if (private$network.conf$get.value("author.only.committers")) {
                committers = unique(private$proj.data$get.commits.raw()[["author.name"]])
                authors = igraph::get.vertex.attribute(u, "name", igraph::V(u)[ type == TYPE.AUTHOR ])
                authors.to.remove = setdiff(authors, committers)
                u = igraph::delete.vertices(u, authors.to.remove)
            }

            return(u)
        },

        #' Get all networks as list.
        #' Build unification to avoid null-pointers.
        #'
        #' @return all networks in a list
        get.networks = function() {
            logging::loginfo("Constructing all networks.")

            ## author-artifact relation
            authors.to.artifacts = private$get.bipartite.relation()
            ## bipartite network
            bipartite.net = self$get.bipartite.network()
            ## author relation
            authors.net = self$get.author.network()
            ## artifact relation
            artifacts.net = self$get.artifact.network()

            return(list(
                "authors.to.artifacts" = authors.to.artifacts,
                "bipartite.net" = bipartite.net,
                "authors.net" = authors.net,
                "artifacts.net" = artifacts.net
            ))
        },

        #' Get the multi network.
        #' The multi network is basically the result of 'get.networks' combined
        #' in one network.
        #'
        #' @return the multi network
        get.multi.network = function() {
            logging::loginfo("Constructing multi network.")

            ## construct the network parts we need for the multi network
            networks = self$get.networks()
            authors.to.artifacts = networks[["authors.to.artifacts"]]
            authors.net = networks[["authors.net"]]
            artifacts.net = networks[["artifacts.net"]]

            ## remove authors from author-artifact relation, if needed:
            ## we only add bipartite edges for authors already present in the author network (this can be
            ## configured by 'author.only.committers', for example), thus, we need to remove any authors
            ## from the author--artifact relation that are superfluous
            authors.from.net = igraph::get.vertex.attribute(authors.net, "name")
            authors.from.artifacts = names(authors.to.artifacts)
            authors.to.artifacts = authors.to.artifacts[ intersect(authors.from.net, authors.from.artifacts) ]
            ## unify vertices with artifacts from bipartite and artifact relation:
            ## when the source for artifacts is different for the bipartite relation and the artifact relation (e.g.,
            ## "cochange" and "callgraph"), then the vertex names need to be unified so that all vertices are
            ## represented properly and, more important here, all bipartite relations can be added
            artifacts.all = unique(plyr::rbind.fill(authors.to.artifacts)[[ "data.vertices" ]])
            artifacts.from.net = igraph::get.vertex.attribute(artifacts.net, "name")
            browser(expr = length(setdiff(artifacts.all, artifacts.from.net)) > 0)
            artifacts.net = artifacts.net + igraph::vertices(setdiff(artifacts.all, artifacts.from.net), type = TYPE.ARTIFACT)

            ## check directedness and adapt artifact network if needed
            if (igraph::is.directed(authors.net) && !igraph::is.directed(artifacts.net)) {
                logging::logwarn("Author network is directed, but artifact network is not. Converting artifact network...")
                artifacts.net = igraph::as.directed(artifacts.net, mode = "mutual")
            } else if (!igraph::is.directed(authors.net) && igraph::is.directed(artifacts.net)) {
                logging::logwarn("Author network is undirected, but artifact network is not. Converting artifact network...")
                artifacts.net = igraph::as.undirected(artifacts.net, mode = "each", edge.attr.comb = EDGE.ATTR.HANDLING)
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


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Network and edge construction -------------------------------------------

#' Construct a dependency network from the given list of lists.
#' For example for a list of authors per thread, where all authors are connected if they are
#' in the same thread (sublist).
#'
#' If directed the order of things in the sublist is respected and the 'edge.attr's hold the
#' vector of possible edge attributes in the given list.
#'
#' @param list the list of lists with data
#' @param network.conf the network configuration
#' @param directed whether or not the network should be directed
#'
#' @return the built network
construct.network.from.list = function(list, network.conf, directed = FALSE) {
    logging::loginfo("Create edges.")
    logging::logdebug("construct.network.from.list: starting.")

    # initialize an edge list to fill and the set of nodes
    nodes.processed = c()
    edge.list = data.frame()

    keys = names(list)
    keys.number = length(list)

    if (directed) {

        ## for all subsets (sets), connect all items in there with the previous ones
        edge.list.data = parallel::mclapply(list, function(set) {
            number.edges = sum(0:(nrow(set) - 1))
            logging::logdebug("[%s/%s] Constructing edges for %s '%s': starting (%s edges to construct).",
                              match(attr(set, "group.name"), keys), keys.number,
                              attr(set, "group.type"), attr(set, "group.name"), number.edges)

            ## Skip artifacts with many, many edges
            if (number.edges > network.conf$get.value("skip.threshold")) {
                logging::logwarn("Skipping edges for %s '%s' due to amount (> %s).",
                                 attr(set, "group.type"), attr(set, "group.name"), network.conf$get.value("skip.threshold"))
                return(NULL)
            }

            # queue of already processed artifacts
            edge.list.set = data.frame()
            nodes.processed.set = c()

            # connect the current item to all previous ones
            for (item.no in 1:nrow(set)) {
                item = set[item.no, ]

                ## get vertex data
                item.node = item[, 1]

                ## get edge attributes
                cols.which = network.conf$get.value("edge.attributes") %in% colnames(item)
                item.edge.attrs = item[, network.conf$get.value("edge.attributes")[cols.which], drop = FALSE]

                ## construct edges
                combinations = expand.grid(item.node, nodes.processed.set, stringsAsFactors = default.stringsAsFactors())
                if (nrow(combinations) > 0 & nrow(item.edge.attrs) == 1)
                    combinations = cbind(combinations, item.edge.attrs, row.names = NULL) # add edge attributes
                edge.list.set = rbind(edge.list.set, combinations) # add to edge list

                # mark current item as processed
                nodes.processed.set = c(nodes.processed.set, item.node)
            }

            ## store set of processed nodes
            attr(edge.list.set, "nodes.processed") = nodes.processed.set

            logging::logdebug("Constructing edges for %s '%s': finished.", attr(set, "group.type"), attr(set, "group.name"))

            return(edge.list.set)
        })

        edge.list = plyr::rbind.fill(edge.list.data)
        nodes.processed = unlist( parallel::mclapply(edge.list.data, function(data) attr(data, "nodes.processed")) )

    } else {

        ## for all items in the sublists, construct the cartesian product
        edge.list.data = parallel::mclapply(list, function(set) {
            number.edges = sum(table(set[,1]) * (dim(table(set[,1])) - 1))
            logging::logdebug("[%s/%s] Constructing edges for %s '%s': starting (%s edges to construct).",
                              match(attr(set, "group.name"), keys), keys.number,
                              attr(set, "group.type"), attr(set, "group.name"), number.edges)

            ## Skip artifacts with many, many edges
            if (number.edges > network.conf$get.value("skip.threshold")) {
                logging::logwarn("Skipping edges for %s '%s' due to amount (> %s).",
                                 attr(set, "group.type"), attr(set, "group.name"), network.conf$get.value("skip.threshold"))
                return(NULL)
            }

            ## get vertex data
            nodes = unique(set[, 1])

            ## break if there is no developer
            if (length(nodes) < 1) {
                return(NULL)
            }

            ## if there is only one developer, just create the node, but no edges
            if (length(nodes) == 1) {
                edges = data.frame()
                attr(edges, "nodes.processed") = nodes # store set of processed nodes
                return(edges)
            }

            ## get combinations
            combinations = combn(nodes, 2) # all unique pairs of developers

            ## construct edge list
            edges = apply(combinations, 2, function(comb) {
                ## basic edge data
                edge = data.frame(comb[1], comb[2])

                ## get edge attibutes
                edge.attrs = set[ set[,1] %in% comb, ] # get data for current combination
                cols.which = network.conf$get.value("edge.attributes") %in% colnames(edge.attrs)
                edge.attrs = edge.attrs[, network.conf$get.value("edge.attributes")[cols.which], drop = FALSE]

                # add edge attributes to edge list
                edgelist = cbind(edge, edge.attrs)

                return(edgelist)
            })
            edges = plyr::rbind.fill(edges)

            ## store set of processed nodes
            attr(edges, "nodes.processed") = nodes

            return(edges)
        })

        edge.list = plyr::rbind.fill(edge.list.data)
        nodes.processed = unlist( parallel::mclapply(edge.list.data, function(data) attr(data, "nodes.processed")) )

    }

    logging::loginfo("Construct network from edges.")

    ## get unique list of vertices to produce
    nodes.processed = unique(nodes.processed)

    # if we do not have nodes AND the edge.list is empty, return rightaway
    if (length(nodes.processed) == 0) {
        return(create.empty.network(directed = directed))
    }

    ## if we have nodes to create, but no edges
    if (is.null(edge.list) || nrow(edge.list) == 0) {
        ## create network with only the vertices
        net = igraph::graph.empty(n = 0, directed = directed) + igraph::vertices(nodes.processed)
    }
    ## if we have nodes and edges
    else {
        ## construct network from edge list
        net = igraph::graph.data.frame(edge.list, directed = directed, vertices = nodes.processed)
    }

    net = igraph::set.vertex.attribute(net, "id", value = igraph::get.vertex.attribute(net, "name"))
    net = igraph::set.edge.attribute(net, "weight", value = 1)

    # transform multiple edges to edge weights
    if (network.conf$get.value("simplify"))
        net = simplify.network(net)

    logging::logdebug("construct.network.from.list: finished.")

    return(net)
}

#' Combine networks to a bipartite network.
#'
#' @param net1 the first network to merge
#' @param net2 the second network to merge
#' @param net1.to.net2 the relation between both given networks
#' @param network.conf the network.conf
#'
#' @return the combined bipartite network
combine.networks = function(net1, net2, net1.to.net2, network.conf) {
    vertices.net1 = igraph::get.vertex.attribute(net1, "name")
    vertices.net2 = igraph::get.vertex.attribute(net2, "name")

    ## check emptiness of networks
    if (length(vertices.net1) == 0) {
        logging::logwarn("net1 is empty.")
    }
    if (length(vertices.net2) == 0) {
        logging::logwarn("net2 is empty.")
    }

    ## combine networks
    u = igraph::disjoint_union(net1, net2)

    u = add.edges.for.bipartite.relation(u, net1.to.net2, network.conf = network.conf)

    ## simplify network
    if (network.conf$get.value("simplify"))
        u = simplify.network(u)

    return(u)
}

#' Add vertex relations to the given network.
#'
#' @param net the network
#' @param net1.to.net2 the vertex relations to add to the given network
#' @param network.conf the network configuration
#'
#' @return the adjusted network
add.edges.for.bipartite.relation = function(net, net1.to.net2, network.conf) {

    ## construct edges (i.e., a vertex sequence with c(source, target, source, target, ...))
    vertex.sequence.for.edges = parallel::mcmapply(function(d, a.df) {
        a = a.df[["data.vertices"]]
        new.edges = lapply(a, function(vert) {
            igraph::V(net)[d, vert] # get two vertices from source network:  c(developer, artifact)
        })
        return(new.edges)
    }, names(net1.to.net2), net1.to.net2)

    ## get extra edge attributes
    extra.edge.attributes.df = parallel::mclapply(net1.to.net2, function(a.df) {
        cols.which = network.conf$get.value("edge.attributes") %in% colnames(a.df)
        return(a.df[, network.conf$get.value("edge.attributes")[cols.which], drop = FALSE])
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

#' Create an empty network that doesn´t break the algorithms.
#'
#' @param directed whether or not the network should be directed
#'
#' @return the new empty network
create.empty.network = function(directed = TRUE) {
    ## create empty network
    net = igraph::graph.empty(0, directed = directed)

    # set proper attributes
    net = igraph::set.vertex.attribute(net, "name", value = "")
    net = igraph::set.vertex.attribute(net, "type", value = 3)
    net = igraph::set.edge.attribute(net, "type", value = 6)

    return(net)
}


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Network simplification --------------------------------------------------

#' Simplify a given network.
#'
#' @param network the given network
#'
#' @return the simplified network
simplify.network = function(network) {
    logging::logdebug("simplify.network: starting.")
    logging::loginfo("Simplifying network.")

    ## simplify networks (contract edges and remove loops)
    network = igraph::simplify(network, edge.attr.comb = EDGE.ATTR.HANDLING, remove.loops = TRUE)

    logging::logdebug("simplify.network: finished.")
    return(network)
}

#' Simplify a list of networks.
#'
#' @param networks the list of networks
#'
#' @return the simplified networks
simplify.networks = function(networks){
    logging::logdebug("simplify.networks: starting.")
    logging::loginfo(
        "Simplifying networks (names = [%s]).",
        paste(names(networks), collapse = ", ")
    )

    nets = parallel::mclapply(networks, simplify.network)

    logging::logdebug("simplify.networks: finished.")
    return(nets)
}


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Sample network ----------------------------------------------------------

SAMPLE.DATA = normalizePath("./sample")

#' Get a example network for illustration purposes.
#'
#' @param testing whether the function gets called from the test cases; if yes,
#'                the path to the sample data is adapted [default: false]
#'
#' @return the sample network
get.sample.network = function(testing = FALSE) {

    ## project configuration
    proj.conf = ProjectConf$new(SAMPLE.DATA, "testing", "sample", "feature")
    proj.conf$update.values(list(artifact.filter.base = FALSE))

    ## RangeData object
    range = proj.conf$get.value("ranges")[1]
    range.callgraph = proj.conf$get.callgraph.revision.from.range(range)
    proj.data = RangeData$new(proj.conf, range, range.callgraph)

    ## network configuration
    net.conf = NetworkConf$new()
    net.conf$update.values(list(
        author.relation = "mail", author.directed = FALSE,
        artifact.relation = "callgraph", artifact.directed = FALSE,
        author.all.authors = TRUE, author.only.committers = FALSE,
        simplify = TRUE
    ))

    ## network builder
    net.builder = NetworkBuilder$new(proj.data, net.conf)

    ## construct multi network
    network = net.builder$get.multi.network()
    network = igraph::set.graph.attribute(network, "sample.network", TRUE)

    ## set layout for plotting
    lay = matrix(c(  20, 179, 693, 552, 956, 1091, 124, 317, 516, 615, 803, 1038,
                    245, 175, 255, 185, 253, 225,   73,   8,  75,   0,  96,   86),
                 nrow = 12, byrow = FALSE) # for sample graph
    network = igraph::set.graph.attribute(network, "layout", lay)

    return(network)
}
