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
TYPE.ISSUE = 3

# edge types
TYPE.EDGES.INTRA = 4
TYPE.EDGES.INTER = 5


## NetworkBuilder ####
NetworkBuilder = R6::R6Class("NetworkBuilder",

    ## private members ####
    private = list(
        proj.data = NULL,
        network.conf = NULL,

        ## networks
        authors.network.mail = NULL, # igraph
        authors.network.cochange = NULL, # igraph
        authors.network.issue = NULL, #igraph
        artifacts.network.cochange = NULL, # igraph
        artifacts.network.callgraph = NULL, # igraph

        ## AUTHOR NETWORKS ####

        ## get the co-change-based author relation as network
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

        ## get the thread-based author relation as network
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
                directed = private$network.conf$get.variable("author.directed")
            )

            private$authors.network.issue = author.relation
            logging::logdebug("get.author.network.issue: finished.")

            return(author.relation)
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
        ## constructor
        initialize = function(project.data, network.conf) {
            private$proj.data = project.data

            if(!missing(network.conf) && "NetworkConf" %in% class(network.conf)) {
                private$network.conf = network.conf
            }

            if (class(self)[1] == "ProjectData")
                logging::loginfo("Initialized data object %s", self$get.class.name())
        },
        ## RESET ENVIRONMENT ##

        ## reset cached data
        reset.environment = function() {
            private$authors.network.mail = NULL
            private$authors.network.cochange = NULL
            private$artifacts.network.cochange = NULL
            private$artifacts.network.callgraph = NULL
        },

        ## CONFIGURATION ####

        ## get the current network configuration
        get.network.conf = function() {
            return(private$network.conf)
        },

        ## set the current network configuration to the given one
        set.network.conf = function(network.conf) {
            private$network.conf = network.conf
            self$reset.environment()
        },


        ## UPDATE CONFIGURATION ####

        ## update network-configuration parameters
        update.network.conf = function(updated.values = list()) {
            private$network.conf$update.values(updated.values = updated.values)
            self$reset.environment()
        },


        ## get the author relation as network (generic)
        get.author.network = function() {
            logging::loginfo("Constructing author network.")

            ## construct network
            relation = private$network.conf$get.variable("author.relation")
            net = switch(
                relation,
                cochange = private$get.author.network.cochange(),
                mail = private$get.author.network.mail(),
                issue = private$get.author.network.issue(),
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

        ## get artifact relation as network (generic)
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

        ## get the (real) bipartite network
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
                                 network.conf = private$network.conf, "artifact")
            return(u)
        },

        get.bipartite.issue.network = function() {
            authors.to.issues = private$proj.data$get.author2issue()

            ## extract vertices for author network
            if (private$network.conf$get.variable("author.all.authors") &&
                !private$network.conf$get.variable("author.only.committers")) {
                authors = private$proj.data$get.authors()[[ "author.name" ]]
            } else {
                authors = names(authors.to.issues)
            }

            issues = private$proj.data$get.issue.ids()

            ##construct networks from vertices
            authors.net = create.empty.network(directed = FALSE) +
                igraph::vertices(authors, name = authors, type = TYPE.AUTHOR)

            issues.net = create.empty.network(directed = FALSE) +
                igraph::vertices(issues, name = issues, type = TYPE.ISSUE)

            u = combine.networks(authors.net, issues.net, authors.to.issues,
                                         network.conf = private$network.conf, "issue.id")

            return(u)
        },

        ## get all networks (build unification to avoid null-pointers)
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

        ## get the multi networks (get.networks combined in one network)
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
                                 network.conf = private$network.conf, "artifact")

            return(u)
        }

    )
)


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Union of networks ####
##

combine.networks = function(net1, net2, net1.to.net2, network.conf, temp) {
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

    u = add.edges.for.bip.relation(u, net1.to.net2, network.conf = network.conf, temp.vertex = temp)

    ## simplify network
    if (network.conf$get.variable("simplify"))
        u = simplify.network(u)

    return(u)
}

## helper function to add dependencies from dev--art mapping to the bipartite network
## TODO change name of temp.vertex
add.edges.for.bip.relation = function(net, net1.to.net2, network.conf, temp.vertex) {

    ## construct edges (i.e., a vertex sequence with c(source, target, source, target, ...))
    vertex.sequence.for.edges = mapply(function(d, a.df) {
        a = a.df[[temp.vertex]]
        new.edges = lapply(a, function(vert) {
            igraph::V(net)[d, vert] # get two vertices from source network:  c(developer, artifact)
        })
        return(new.edges)
    }, names(net1.to.net2), net1.to.net2)

    ## get extra edge attributes
    extra.edge.attributes.df = parallel::mclapply(net1.to.net2, function(a.df) {
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

get.sample.network = function(network.conf = NetworkConf$new()) {
    ## INDEPENDENT NETWORKS
    authors = igraph::graph.empty(directed = FALSE) +
        igraph::vertices("D1", "D2", "D3", "D4", "D5", "D6", type = TYPE.AUTHOR) +
        igraph::edges("D1", "D2", "D1", "D4", "D3", "D4", "D4", "D5", type = TYPE.EDGES.INTRA)

    artifacts = igraph::graph.empty(directed = FALSE) +
        igraph::vertices("A1", "A2", "A3", "A4", "A5", "A6", type = TYPE.ARTIFACT) +
        igraph::edges("A1", "A2", "A1", "A3", "A2", "A3", "A2", "A4", "A5", "A6", type = TYPE.EDGES.INTRA)
    # artifacts = igraph::as.directed(artifacts, mode = "mutual")

    authors.to.artifacts.df = data.frame(
        author.name = c("D1", "D2", "D3", "D4", "D4", "D5", "D6"),
        artifact    = c("A1", "A1", "A3", "A4", "A5", "A6", "A6")
    )
    authors.to.artifacts = get.key.to.value.from.df(authors.to.artifacts.df, "author.name", "artifact")

    ## combine networks
    network = combine.networks(authors, artifacts, authors.to.artifacts, network.conf, "artifact")
    network = igraph::set.graph.attribute(network, "sample.network", TRUE)

    return(network)
}
