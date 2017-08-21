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
requireNamespace("plyr") # for dlply function
requireNamespace("igraph") # networks

## / / / / / / / / / / / / / /
## NETWORk META-CONFIGURATION
##

## node types
TYPE.AUTHOR = 1
TYPE.ARTIFACT = 2

# edge types
TYPE.EDGES.INTRA = 3
TYPE.EDGES.INTER = 4


## NetworkBuilder ####
NetworkBuilder = R6::R6Class("NetworkBuilder",

    ## private members ####
    private = list(
        proj.data = NULL,
        network.conf = NULL,

        ## networks
        authors.network.mail = NULL, # igraph
        authors.network.cochange = NULL, # igraph
        artifacts.network.cochange = NULL, # igraph
        artifacts.network.callgraph = NULL, # igraph

        ## AUTHOR NETWORKS ####

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
                directed = private$network.conf$get.variable("author.directed")
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

            ## TODO do we need the if-else statement here? (is this captured by the called function?)
            if (length(private$proj.data$get.thread2author()) != 0) {
                author.relation = construct.dependency.network.from.list(
                    private$proj.data$get.thread2author(),
                    network.conf = private$network.conf,
                    directed = private$network.conf$get.variable("author.directed")
                )
            } else {
                author.relation = create.empty.network(private$network.conf$get.variable("author.directed"))
            }

            ## store network
            private$authors.network.mail = author.relation
            logging::logdebug("get.author.network.mail: finished.")

            return(author.relation)
        },


        ## ARTIFACT NETWORKS ####

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
            file.name = paste0("cg_nw_", private$proj.data$get.project.conf()$get.entry("artifact.short"), "_", private$proj.data$get.revision.callgraph(), ".net")
            file = file.path(file.dir, file.name)

            ## read network from disk
            artifacts.net = read.network.from.file(file)
            ## post-process network
            artifacts.net = postprocess.artifact.names.callgraph(artifacts.net, private$proj.data$get.project.conf()$get.entry("artifact"))

            ## store network
            private$artifacts.network.callgraph = artifacts.net
            logging::logdebug("get.artifact.network.callgraph: finished.")

            return(artifacts.net)
        }

    ),


    ## public members ####
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
        ## RESET ENVIRONMENT ##

        #' Reset the current environment in order to rebuild it.
        #' Has to be called whenever the data or configuration get changed.
        reset.environment = function() {
            private$authors.network.mail = NULL
            private$authors.network.cochange = NULL
            private$artifacts.network.cochange = NULL
            private$artifacts.network.callgraph = NULL
        },

        ## CONFIGURATION ####

        #' Get the current network configuration.
        #'
        #' @return the current network configuration
        get.network.conf = function() {
            return(private$network.conf)
        },

        #' Set the network configuration to the given new one.
        #'
        #' @param network.conf the new network configuration
        set.network.conf = function(network.conf) {
            private$network.conf = network.conf
            self$reset.environment()
        },


        ## UPDATE CONFIGURATION ####

        #' Update the network configuration based on the given list
        #' of values and reset the environment afterwards
        #'
        #' @param updated.values the new values for the network configuration
        update.network.conf = function(updated.values = list()) {
            private$network.conf$update.values(updated.values = updated.values)
            self$reset.environment()
        },


        #' Get the generic author network.
        #'
        #' @return the generic author network
        get.author.network = function() {
            logging::loginfo("Constructing author network.")

            ## construct network
            relation = private$network.conf$get.variable("author.relation")
            net = switch(
                relation,
                cochange = private$get.author.network.cochange(),
                mail = private$get.author.network.mail(),
                stop(sprintf("The author relation '%s' does not exist.", relation))
            )

            ## add all missing authors to the network if wanted
            if (private$network.conf$get.variable("author.all.authors")) {
                authors.all = private$proj.data$get.authors()[[ "author.name" ]]
                authors.net = igraph::get.vertex.attribute(net, "name")
                net = net + igraph::vertices(setdiff(authors.all, authors.net))
            }

            ## remove all authors from the corresponding network who do not have touched any artifact
            if (private$network.conf$get.variable("author.only.committers")) {
                ## authors-artifact relation
                authors.from.net = igraph::get.vertex.attribute(net, "name")
                authors.from.artifacts = names(private$proj.data$get.author2artifact())
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
            relation = private$network.conf$get.variable("artifact.relation")

            net = switch(
                relation,
                cochange = private$get.artifact.network.cochange(),
                callgraph = private$get.artifact.network.callgraph(),
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
            ## authors-artifact relation
            authors.to.artifacts = private$proj.data$get.author2artifact()

            ## extract vertices for author network
            if (private$network.conf$get.variable("author.all.authors") &&
                !private$network.conf$get.variable("author.only.committers")) {
                authors = private$proj.data$get.authors()[[ "author.name" ]]
            } else {
                authors = names(authors.to.artifacts)
            }
            ## extract vertices for author network
            artifacts = private$proj.data$get.artifacts()

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

        #' Get all networks as list.
        #' Build unification to avoid null-pointers.
        #'
        #' @return all networks in a list
        get.networks = function() {
            logging::loginfo("Constructing all networks.")

            ## author-artifact relation
            authors.to.artifacts = private$proj.data$get.author2artifact()
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

            ## unify vertices with author-artifact relation
            authors.from.net = igraph::get.vertex.attribute(authors.net, "name")
            authors.from.artifacts = names(authors.to.artifacts)
            authors.net = authors.net + igraph::vertices(setdiff(authors.from.artifacts, authors.from.net), type = TYPE.AUTHOR)
            ## unify vertices with artifacts from bipartite and artifact relation
            artifacts.all = private$proj.data$get.artifacts()
            artifacts.from.net = igraph::get.vertex.attribute(artifacts.net, "name")
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


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Union of networks ####
##

#' Combine networks to a bipartite network.
#'
#' @param authors.net the given author network
#' @param artifacts.net the given artifact network
#' @param authors.to.artifacts the raltion between both
#' @param network.conf the network.conf
#'
#' @return the combined bipartite network
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


#' Add dependencies from dev--art mapping to the bipartite network.
#'
#' @param net the bipartite network
#' @param auth.to.arts the dev--art mapping
#' @param network.conf the network configuration
#'
#' @return the adjusted network
add.edges.for.devart.relation = function(net, auth.to.arts, network.conf) {

    ## construct edges (i.e., a vertex sequence with c(source, target, source, target, ...))
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

## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Exemplary network for illustration purposes
##

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
