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
## NetworkBuilder ----------------------------------------------------------

NetworkBuilder = R6::R6Class("NetworkBuilder",

    ## * private -----------------------------------------------------------

    private = list(
        ## * * data and configuration --------------------------------------

        proj.data = NULL,
        network.conf = NULL,

        ## * * network caching ---------------------------------------------

        authors.network.mail = NULL, # igraph
        authors.network.cochange = NULL, # igraph
        authors.network.issue = NULL, #igraph
        artifacts.network.cochange = NULL, # igraph
        artifacts.network.callgraph = NULL, # igraph
        artifacts.network.mail = NULL, # igraph
        artifacts.network.issue = NULL, # igraph

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
            author.net = construct.dependency.network.from.list(
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

            author.relation = construct.dependency.network.from.list(
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

            author.relation = construct.dependency.network.from.list(
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

            artifacts.net = construct.dependency.network.from.list(
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
            if (is.na(private$revision.callgraph)) {
                logging::logerror("The call-graph revision is not set. Aborting...")
                logging::logerror("This may be due to project-level analysis.
                                  The call-graph data is only available in range-level analysis.")
                stop("Trying to get call-graph data before setting a revision.")
            }

            ## construct path and file
            file.dir = private$proj.data$get.data.path.callgraph()
            file.name = paste0("cg_nw_", private$proj.data$get.project.conf()$get.value("artifact.short"), "_", private$proj.data$get.revision.callgraph(), ".net")
            file = file.path(file.dir, file.name)

            ## read network from disk
            artifacts.net = read.network.from.file(file)
            ## post-process network
            artifacts.net = postprocess.artifact.names.callgraph(artifacts.net, private$proj.data$get.project.conf()$get.value("artifact"))

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

            if(!missing(network.conf) && "NetworkConf" %in% class(network.conf)) {
                private$network.conf = network.conf
            }

            if (class(self)[1] == "ProjectData")
                logging::loginfo("Initialized data object %s", self$get.class.name())
        },

        ## * * resetting environment ---------------------------------------

        #' Reset the current environment in order to rebuild it.
        #' Has to be called whenever the data or configuration get changed.
        reset.environment = function() {
            private$authors.network.mail = NULL
            private$authors.network.cochange = NULL
            private$artifacts.network.cochange = NULL
            private$artifacts.network.callgraph = NULL
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
## Helper functions --------------------------------------------------------

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

    u = add.edges.for.bip.relation(u, net1.to.net2, network.conf = network.conf)

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
add.edges.for.bip.relation = function(net, net1.to.net2, network.conf) {

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


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Sample network ----------------------------------------------------------

#' Get a example network for illustration purposes.
#'
#' @param network.conf the network configuration
#'
#' @return the sample network
get.sample.network = function(network.conf = NetworkConf$new()) {
    ## INDEPENDENT NETWORKS
    authors = igraph::graph.empty(directed = FALSE) +
        igraph::vertices("D1", "D2", "D3", "D4", "D5", "D6", type = TYPE.AUTHOR) +
        igraph::edges("D1", "D2", "D1", "D4", "D3", "D4", "D4", "D5", type = TYPE.EDGES.INTRA, weight = 1)

    artifacts = igraph::graph.empty(directed = FALSE) +
        igraph::vertices("A1", "A2", "A3", "A4", "A5", "A6", type = TYPE.ARTIFACT) +
        igraph::edges("A1", "A2", "A1", "A3", "A2", "A3", "A2", "A4", "A5", "A6", type = TYPE.EDGES.INTRA, weight = 1)
    # artifacts = igraph::as.directed(artifacts, mode = "mutual")

    authors.to.artifacts.df = data.frame(
        author.name = c("D1", "D2", "D3", "D4", "D4", "D5", "D6"),
        artifact    = c("A1", "A1", "A3", "A4", "A5", "A6", "A6")
    )
    authors.to.artifacts = get.key.to.value.from.df(authors.to.artifacts.df, "author.name", "artifact")

    ## combine networks
    network = combine.networks(authors, artifacts, authors.to.artifacts, network.conf)
    network = igraph::set.graph.attribute(network, "sample.network", TRUE)

    return(network)
}
