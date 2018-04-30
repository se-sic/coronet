## This file is part of codeface-extraction-r, which is free software: you
## can redistribute it and/or modify it under the terms of the GNU General
## Public License as published by  the Free Software Foundation, version 2.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License along
## with this program; if not, write to the Free Software Foundation, Inc.,
## 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
##
## Copyright 2016-2018 by Claus Hunsen <hunsen@fim.uni-passau.de>
## Copyright 2017 by Raphael Nömmer <noemmer@fim.uni-passau.de>
## Copyright 2017 by Christian Hechtl <hechtl@fim.uni-passau.de>
## Copyright 2017-2018 by Thomas Bock <bockthom@fim.uni-passau.de>
## Copyright 2018 by Barbara Eckl <ecklbarb@fim.uni-passau.de>
## All Rights Reserved.


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Libraries ---------------------------------------------------------------

requireNamespace("R6") # for R6 classes
requireNamespace("logging") # for logging
requireNamespace("parallel") # for parallel computation
requireNamespace("plyr") # for dlply function
requireNamespace("igraph") # networks


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Vertex and edge types ---------------------------------------------------

## vertex types
TYPE.AUTHOR = "Author"
TYPE.ARTIFACT = "Artifact"

## edge types
TYPE.EDGES.INTRA = "Unipartite"
TYPE.EDGES.INTER = "Bipartite"


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Edge-attribute handling during simplification ---------------------------

## Edge-attribute contraction: configure handling of attributes by name
EDGE.ATTR.HANDLING = list(
    ## network-analytic data
    weight = "sum",
    type = "first",
    relation = "first",

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

        ## * * relation-to-vertex-kind mapping -----------------------------

        #' Determine which vertex kind should be chose for the vertex depending on the relation
        #' between the vertices.
        #'
        #' @return the vertex kind to be used
        get.vertex.kind.for.relation = function(relation) {

            vertex.kind = switch(relation,
                cochange  = private$proj.data$get.project.conf.entry("artifact.codeface"),
                callgraph = private$proj.data$get.project.conf.entry("artifact.codeface"),
                mail      = "MailThread",
                issue     = "Issue"
            )

            return(vertex.kind)
        },

        ## * * data cutting ------------------------------------------------

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
        #' If it does not already exist build it first.
        #'
        #' @return the author network with cochange relation
        get.author.network.cochange = function() {
            logging::logdebug("get.author.network.cochange: starting.")

            ## do not compute anything more than once
            if (!is.null(private$authors.network.cochange)) {
                logging::logdebug("get.author.network.cochange: finished. (already existing)")
                return(private$authors.network.cochange)
            }

            ## construct edge list based on artifact2author data
            author.net.data = construct.edge.list.from.key.value.list(
                private$proj.data$get.artifact2author(),
                network.conf = private$network.conf,
                directed = private$network.conf$get.value("author.directed")
            )

            ## construct network from obtained data
            author.net = construct.network.from.edge.list(
                author.net.data[["vertices"]],
                author.net.data[["edges"]],
                network.conf = private$network.conf,
                directed = private$network.conf$get.value("author.directed")
            )

            ## store network
            private$authors.network.cochange = author.net
            logging::logdebug("get.author.network.cochange: finished.")

            return(author.net)
        },

        #' Get the thread-based author relation as network.
        #' If it does not already exist build it first.
        #'
        #' @return the author network with mail relation
        get.author.network.mail = function() {

            logging::logdebug("get.author.network.mail: starting.")

            ## do not compute anything more than once
            if (!is.null(private$authors.network.mail)) {
                logging::logdebug("get.author.network.mail: finished. (already existing)")
                return(private$authors.network.mail)
            }


            ## construct edge list based on thread2author data
            author.net.data = construct.edge.list.from.key.value.list(
                private$proj.data$get.thread2author(),
                network.conf = private$network.conf,
                directed = private$network.conf$get.value("author.directed")
            )

            ## construct network from obtained data
            author.net = construct.network.from.edge.list(
                author.net.data[["vertices"]],
                author.net.data[["edges"]],
                network.conf = private$network.conf,
                directed = private$network.conf$get.value("author.directed")
            )

            ## store network
            private$authors.network.mail = author.net
            logging::logdebug("get.author.network.mail: finished.")

            return(author.net)
        },

        ##get the issue based author relation as network
        get.author.network.issue = function() {
            logging::logdebug("get.author.network.issue: starting.")

            if (!is.null(private$authors.network.issue)) {
                logging::logdebug("get.author.network.issue: finished. (already existing)")
                return(private$authors.network.issue)
            }

            ## construct edge list based on issue2author data
            author.net.data = construct.edge.list.from.key.value.list(
                private$proj.data$get.issue2author(),
                network.conf = private$network.conf,
                directed = private$network.conf$get.value("author.directed")
            )

            ## construct network from obtained data
            author.net = construct.network.from.edge.list(
                author.net.data[["vertices"]],
                author.net.data[["edges"]],
                network.conf = private$network.conf,
                directed = private$network.conf$get.value("author.directed")
            )

            private$authors.network.issue = author.net
            logging::logdebug("get.author.network.issue: finished.")

            return(author.net)
        },

        ## * * artifact networks -------------------------------------------

        #' Get the co-change-based artifact network,
        #' If it does not already exist build it first.
        #'
        #' @return the artifact network with cochange realtion
        get.artifact.network.cochange = function() {

            logging::logdebug("get.artifact.network.cochange: starting.")

            ## do not compute anything more than once
            if (!is.null(private$artifacts.network.cochange)) {
                logging::logdebug("get.artifact.network.cochange: finished. (already existing)")
                return(private$artifacts.network.cochange)
            }

            ## construct edge list based on commit2artifact data
            artifacts.net.data.raw = private$proj.data$get.commit2artifact()
            artifacts.net.data = construct.edge.list.from.key.value.list(
                artifacts.net.data.raw,
                network.conf = private$network.conf,
                directed = FALSE
            )

            ## construct network from obtained data
            artifacts.net = construct.network.from.edge.list(
                artifacts.net.data[["vertices"]],
                artifacts.net.data[["edges"]],
                network.conf = private$network.conf,
                directed = FALSE
            )

            ## set network attribute 'vertex.kind' to ensure the correct setting of the
            ## vertex attribute 'kind' later
            artifacts.net = igraph::set.graph.attribute(
                artifacts.net, "vertex.kind",
                value = private$get.vertex.kind.for.relation("cochange")
            )

            ## store network
            private$artifacts.network.cochange = artifacts.net
            logging::logdebug("get.artifact.network.cochange: finished.")

            return(artifacts.net)
        },

        #' Get the call-graph-based artifact network.
        #' If it does not already exist build it first.
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

            ## set edge attribute 'artifact.type' as the raw data do not contain this!
            artifacts.net = igraph::set.edge.attribute(
                artifacts.net, "artifact.type",
                value = private$proj.data$get.project.conf.entry("artifact.codeface")
            )

            ## set network attribute 'vertex.kind' to ensure the correct setting of the
            ## vertex attribute 'kind' later
            artifacts.net = igraph::set.graph.attribute(
                artifacts.net, "vertex.kind",
                value = private$get.vertex.kind.for.relation("callgraph")
            )

            ## store network
            private$artifacts.network.callgraph = artifacts.net
            logging::logdebug("get.artifact.network.callgraph: finished.")

            return(artifacts.net)
        },

        #' Get the mail-based artifact network.
        #' If it does not already exist build it first.
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
            artifacts.net.data.raw = private$proj.data$get.thread2author()
            artifacts = names(artifacts.net.data.raw) # thread IDs
            artifacts.net = create.empty.network(directed = directed) + igraph::vertices(artifacts)

            ## set network attribute 'vertex.kind' to ensure the correct setting of the
            ## vertex attribute 'kind' later
            artifacts.net = igraph::set.graph.attribute(
                artifacts.net, "vertex.kind",
                value = private$get.vertex.kind.for.relation("mail")
            )

            ## store network
            private$artifacts.network.mail = artifacts.net
            logging::logdebug("get.artifact.network.mail: finished.")

            return(artifacts.net)
        },

        #' Get the issue-based artifact network.
        #' If it does not already exist build it first.
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
            artifacts.net.data.raw = private$proj.data$get.issue2author()
            artifacts = names(artifacts.net.data.raw) # thread IDs
            artifacts.net = create.empty.network(directed = directed) + igraph::vertices(artifacts)

            ## set network attribute 'vertex.kind' to ensure the correct setting of the
            ## vertex attribute 'kind' later
            artifacts.net = igraph::set.graph.attribute(
                artifacts.net, "vertex.kind",
                value = private$get.vertex.kind.for.relation("issue")
            )

            ## store network
            private$artifacts.network.issue = artifacts.net
            logging::logdebug("get.artifact.network.issue: finished.")

            return(artifacts.net)
        },

        ## * * bipartite relations ------------------------------------------

        #' Get the key-value data for the bipartite relations,
        #' which are implied by the "artifact.relation" from the network configuration.
        #'
        #' @return the list of data for the bipartite relations, each with the attributes
        #'         'vertex.kind' denoting the artifact type for the relations
        #'         and 'relation' denoting the respective network-configuration entry
        get.bipartite.relations = function() {
            logging::logdebug("get.bipartite.relations: starting.")

            relations = private$network.conf$get.variable("artifact.relation")
            logging::logdebug("Using bipartite relations '%s'.", relations)

            bip.relations = lapply(relations, function(relation) {
                ## get the actual relation and the artifact type
                switch(
                    relation,
                    cochange = {
                        bip.relation = private$proj.data$get.author2artifact()
                    },
                    callgraph = {
                        bip.relation = private$proj.data$get.author2artifact()
                    },
                    mail = {
                        bip.relation = private$proj.data$get.author2thread()
                    },
                    issue = {
                        bip.relation = private$proj.data$get.author2issue()
                    }
                )

                ## set vertex.kind and relation attributes
                attr(bip.relation, "vertex.kind") = private$get.vertex.kind.for.relation(relation)
                attr(bip.relation, "relation") = relation

                return (bip.relation)
            })
            names(bip.relations) = relations

            logging::logdebug("get.bipartite.relations: finished.")
            return(bip.relations)
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

            ## check arguments
            private$proj.data.original = verify.argument.for.parameter(project.data, "ProjectData", class(self)[1])
            private$proj.data = project.data$clone()
            private$network.conf = verify.argument.for.parameter(network.conf, "NetworkConf", class(self)[1])

            ## cut data if needed
            if (private$network.conf$get.value("unify.date.ranges")) {
                private$cut.data.to.same.timestamps()
            }

            if (class(self)[1] == "NetworkBuilder") {
                logging::loginfo("Initialized network builder for data %s.",
                                 private$proj.data$get.class.name())
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
            if (private$network.conf$get.value("unify.date.ranges")) {
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
        #'                          has to be reset or not [default: FALSE]
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
            relations = private$network.conf$get.value("author.relation")
            networks = lapply(relations, function(relation) {
                network = switch(
                    relation,
                    cochange = private$get.author.network.cochange(),
                    mail = private$get.author.network.mail(),
                    issue = private$get.author.network.issue(),
                    stop(sprintf("The author relation '%s' does not exist.", rel))
                    ## TODO construct edge lists here and merge those (inline the private methods)
                )

                ## set edge attributes on all edges
                igraph::E(network)$type = TYPE.EDGES.INTRA
                igraph::E(network)$relation = relation

                return(network)
            })
            net = merge.networks(networks)

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
                authors.from.artifacts = lapply(private$get.bipartite.relations(), function(bipartite.relation) {
                    return(names(bipartite.relation))
                })
                authors.from.artifacts = unlist(authors.from.artifacts)
                if (!is.null(authors.from.artifacts)) {
                    net = igraph::delete.vertices(net, setdiff(authors.from.net, authors.from.artifacts))
                }
            }

            ## set vertex attributes for identifaction
            igraph::V(net)$kind = TYPE.AUTHOR
            igraph::V(net)$type = TYPE.AUTHOR

            ## add range attribute for later analysis (if available)
            if ("RangeData" %in% class(private$proj.data)) {
                attr(net, "range") = private$proj.data$get.range()
            }

            return(net)
        },

        #' Get the generic artifact network.
        #'
        #' @return the generic artifact network
        get.artifact.network = function() {
            logging::loginfo("Constructing artifact network.")

            ## construct network
            relations = private$network.conf$get.value("artifact.relation")
            networks = lapply(relations, function(relation) {
                network = switch(
                    relation,
                    cochange = private$get.artifact.network.cochange(),
                    callgraph = private$get.artifact.network.callgraph(),
                    mail = private$get.artifact.network.mail(),
                    issue = private$get.artifact.network.issue(),
                    stop(sprintf("The artifact relation '%s' does not exist.", relation))
                )

                ## set edge attributes on all edges
                igraph::E(network)$type = TYPE.EDGES.INTRA
                igraph::E(network)$relation = relation

                ## set vertex attribute 'kind' on all edges, corresponding to relation
                kind = unique(igraph::get.graph.attribute(network, "vertex.kind"))
                network = igraph::set.vertex.attribute(network, "kind", value = kind)

                return(network)
            })
            net = merge.networks(networks)

            ## set vertex and edge attributes for identifaction
            igraph::V(net)$type = TYPE.ARTIFACT

            ## add range attribute for later analysis (if available)
            if ("RangeData" %in% class(private$proj.data)) {
                attr(net, "range") = private$proj.data$get.range()
            }

            return(net)
        },

        #' Get the (real) bipartite network.
        #'
        #' @return the bipartite network
        get.bipartite.network = function() {
            ## get data by the chosen relation
            bipartite.relation.data = private$get.bipartite.relations()
            directed = private$network.conf$get.value("author.directed")

            vertex.data = lapply(bipartite.relation.data, function(net.to.net) {

                ## extract vertices for author network
                if (private$network.conf$get.value("author.all.authors")) {
                    author.vertices = private$proj.data$get.authors()[[ "author.name" ]]
                } else {
                    author.vertices = names(net.to.net)
                }

                ## select all artifact vertices
                artifact.vertices = unique(unlist(lapply(net.to.net, function(df) {
                    return(df[["data.vertices"]])
                })))

                ## get vertex.kind from bipartite relation
                vertex.kind = attr(net.to.net, "vertex.kind")

                ## join author and artifact vertices to the list of all vertices in the network
                vertices = data.frame(
                    name = c(author.vertices, artifact.vertices),
                    type = c(
                        rep(TYPE.AUTHOR, length(author.vertices)),
                        rep(TYPE.ARTIFACT, length(artifact.vertices))
                    ),
                    kind = c(
                        rep(TYPE.AUTHOR, length(author.vertices)),
                        rep(vertex.kind , length(artifact.vertices))
                    )
                )
                return(vertices)
            })

            ## Merge network data and construct a vertex-only network for now
            vertex.data = merge.network.data(vertex.data = vertex.data, edge.data = NULL)[["vertices"]]
            vertex.network = igraph::graph.data.frame(
                d = create.empty.edge.list(),
                vertices = vertex.data,
                directed = directed
            )

            ## Add edges to the vertex network
            network = add.edges.for.bipartite.relation(
                vertex.network,
                bipartite.relations = bipartite.relation.data,
                private$network.conf
            )

            ## remove vertices that are not committers if wanted
            if (private$network.conf$get.value("author.only.committers")) {
                committers = unique(private$proj.data$get.commits()[["author.name"]])
                authors = igraph::get.vertex.attribute(network, "name", igraph::V(network)[ type == TYPE.AUTHOR ])
                authors.to.remove = setdiff(authors, committers)
                network = igraph::delete.vertices(network, authors.to.remove)
            }

            ## add range attribute for later analysis (if available)
            if ("RangeData" %in% class(private$proj.data)) {
                attr(network, "range") = private$proj.data$get.range()
            }

            return(network)
        },

        #' Get all networks as list.
        #' Build unification to avoid null-pointers.
        #'
        #' @return all networks in a list
        get.networks = function() {
            logging::loginfo("Constructing all networks.")

            ## author-artifact relation
            authors.to.artifacts = private$get.bipartite.relations()
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
            igraph::V(authors.net)$kind = TYPE.AUTHOR
            artifacts.net = networks[["artifacts.net"]]

            ## remove authors from author-artifact relation, if needed:
            ## we only add bipartite edges for authors already present in the author network (this can be
            ## configured by 'author.only.committers', for example), thus, we need to remove any authors
            ## from the author--artifact relation that are superfluous
            authors.from.net = igraph::get.vertex.attribute(authors.net, "name")
            ## save relation and intersect the author vertices from the author network and the
            ## bipartite networks
            authors.to.artifacts = mapply(function(a2a.rel, relation.type) {
                authors.from.artifacts = names(a2a.rel)
                a2a = a2a.rel[ intersect(authors.from.net, authors.from.artifacts) ]
                attr(a2a, "relation") = relation.type
                return(a2a)
            }, authors.to.artifacts, names(authors.to.artifacts), SIMPLIFY = FALSE)

            ## unify vertices with artifacts from bipartite and artifact relation:
            ## when the source for artifacts is different for the bipartite relation and the artifact relation (e.g.,
            ## "cochange" and "callgraph"), then the vertex names need to be unified so that all vertices are
            ## represented properly and, more important here, all bipartite relations can be added
            artifacts = lapply(authors.to.artifacts, function(a2a.rel) {
                artifact.list = plyr::rbind.fill(a2a.rel)[ c("data.vertices", "artifact.type") ]
                artifact.list = unique(artifact.list)
                return(artifact.list)
            })
            artifacts.all = plyr::rbind.fill(artifacts)

            artifacts.from.net = igraph::get.vertex.attribute(artifacts.net, "name")
            artifacts.to.add = setdiff(artifacts.all[["data.vertices"]], artifacts.from.net)
            artifacts.to.add.kind = artifacts.all[
                artifacts.all[["data.vertices"]] %in% artifacts.to.add, "artifact.type"
            ]
            artifacts.net = artifacts.net + igraph::vertices(artifacts.to.add, type = TYPE.ARTIFACT,
                                                             kind = artifacts.to.add.kind)

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
            ## TODO use merge.networks and add.edges.for.bipartite.relations here (remove combine.networks then!)
            u = combine.networks(authors.net, artifacts.net, authors.to.artifacts,
                                 network.conf = private$network.conf)

            ## add range attribute for later analysis (if available)
            if ("RangeData" %in% class(private$proj.data)) {
                attr(u, "range") = private$proj.data$get.range()
            }

            return(u)
        }

    )
)


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Network and edge construction -------------------------------------------

#' Construct a edge list from the given list of lists.
#' For example for a list of authors per thread, where all authors are connected if they are
#' in the same thread (sublist).
#'
#' Important: The input needs to be compatible with the function \code{get.key.to.value.from.df}.
#'
#' If directed the order of things in the sublist is respected and the 'edge.attr's hold the
#' vector of possible edge attributes in the given list.
#'
#' @param list the list of lists with data
#' @param network.conf the network configuration
#' @param directed whether or not the network should be directed [default: FALSE]
#'
#' @return the built edge list
construct.edge.list.from.key.value.list = function(list, network.conf, directed = FALSE) {
    logging::loginfo("Create edges.")
    logging::logdebug("construct.edge.list.from.key.value.list: starting.")

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
            number.edges = sum(table(set[, 1]) * (dim(table(set[, 1])) - 1))
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

            ## break if there is no author
            if (length(nodes) < 1) {
                return(NULL)
            }

            ## if there is only one author, just create the node, but no edges
            if (length(nodes) == 1) {
                edges = data.frame()
                attr(edges, "nodes.processed") = nodes # store set of processed nodes
                return(edges)
            }

            ## get combinations
            combinations = combn(nodes, 2) # all unique pairs of authors

            ## construct edge list
            edges = apply(combinations, 2, function(comb) {
                ## basic edge data
                edge = data.frame(comb[1], comb[2])

                ## get edge attibutes
                edge.attrs = set[ set[, 1] %in% comb, ] # get data for current combination
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

    logging::logdebug("construct.edge.list.from.key.value.list: finished.")

    return(list(
        vertices = nodes.processed,
        edges = edge.list
    ))
}

#' Construct a network from the given lists of vertices and edges.
#' For example for a list of authors per thread, where all authors are connected if they are
#' in the same thread (sublist).
#'
#' If directed the order of things in the sublist is respected and the 'edge.attr's hold the
#' vector of possible edge attributes in the given list.
#'
#' @param vertices data frame with vertex data
#' @param edge.list list of edges
#' @param network.conf the network configuration
#' @param directed whether or not the network should be directed [default: FALSE]
#'
#' @return the built network
construct.network.from.edge.list = function(vertices, edge.list, network.conf, directed = FALSE) {
    logging::logdebug("construct.network.from.edge.list: starting.")
    logging::loginfo("Construct network from edges.")

    ## get unique list of vertices to produce
    nodes.processed = unique(vertices)

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

    net = igraph::set.edge.attribute(net, "weight", value = 1)

    # transform multiple edges to edge weights
    if (network.conf$get.value("simplify")) {
        net = simplify.network(net)
    }

    logging::logdebug("construct.network.from.edge.list: finished.")

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

#' Merges a list vertex data frame and merges a list of edge
#' data frames
#'
#' @param vertex.data the list of vertex data frames
#' @param edge.data the list of edge data frames
#'
#' @return list containing one edge data frame and one vertex data frame
merge.network.data = function(vertex.data, edge.data) {
    logging::logdebug("merge.network.data: starting.")

    vertices = plyr::rbind.fill(vertex.data)

    ## select unique vertices
    vertices = unique.data.frame(vertices)
    edges = plyr::rbind.fill(edge.data)

    logging::logdebug("merge.network.data: finished.")
    return(list(
        vertices = vertices,
        edges = edges
    ))
}

#' Merges a list of networks to one big network
#'
#' @param networks the list of networks
#'
#' @return the built one network
merge.networks = function(networks) {
    logging::logdebug("merge.networks: starting.")

    ## list with all vertex data frames
    vertex.data = lapply(networks, function(network) {
        return(igraph::as_data_frame(network, what = "vertices"))
    })

    ## list of all edge data frames
    edge.data = lapply(networks, function(network) {
        return(igraph::as_data_frame(network, what = "edges"))
    })

    ## merge all edge and vertex data frames
    new.network.data = merge.network.data(vertex.data, edge.data)

    ## build whole network form edge and vertex data frame
    whole.network = igraph::graph.data.frame(
        new.network.data[["edges"]],
        vertices = new.network.data[["vertices"]],
        directed = igraph::is.directed(networks[[1]])
    )

    logging::logdebug("merge.networks: finished.")

    return(whole.network)
}

#' Add vertex relations to the given network.
#'
#' @param net the network
#' @param bipartite.relations the list of the vertex relations to add to the given network for
#'                            all configured relations
#' @param network.conf the network configuration
#'
#' @return the adjusted network
add.edges.for.bipartite.relation = function(net, bipartite.relations, network.conf) {

    ## iterate about all bipartite.relations depending on the relation type
    for (net1.to.net2 in bipartite.relations) {

        ## construct edges (i.e., a vertex sequence with c(source, target, source, target, ...))
        vertex.sequence.for.edges = parallel::mcmapply(function(d, a.df) {
            a = a.df[["data.vertices"]]
            new.edges = lapply(a, function(vert) {
                igraph::V(net)[d, vert] # get two vertices from source network:  c(author, artifact)
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
        extra.edge.attributes.df["type"] = TYPE.EDGES.INTER # add egde type
        extra.edge.attributes.df["relation"] = attr(net1.to.net2, "relation") # add relation type

        extra.edge.attributes = as.list(extra.edge.attributes.df)

        ## add the vertex sequences as edges to the network
        net = igraph::add_edges(net, unlist(vertex.sequence.for.edges), attr = extra.edge.attributes)
    }

    return(net)
}

#' Create an empty network that does not break the algorithms.
#'
#' @param directed whether or not the network should be directed [default: TRUE]
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

#' Create an empty data frame that does not break the algorithms and
#' represents an edge list.
#'
#' @return the new empty data frame as edge list
create.empty.edge.list = function() {
    return(data.frame(from = character(0), to = character(0)))
}


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Network simplification --------------------------------------------------

#' Simplify a given network.
#'
#' @param network the given network
#' @param remove.multiple whether to contract multiple edges between the same pair of nodes [default: TRUE]
#' @param remove.loops whether to remove loops [default: TRUE]
#'
#' @return the simplified network
simplify.network = function(network, remove.multiple = TRUE, remove.loops = TRUE) {
    logging::logdebug("simplify.network: starting.")
    logging::loginfo("Simplifying network.")

    if (length(unique(igraph::get.edge.attribute(network, "relation"))) > 1) {
        ## data frame of the network
        edge.data = igraph::as_data_frame(network, what = "edges")
        vertex.data = igraph::as_data_frame(network, what = "vertices")

        ## select edges of one relation, build the network and simplify this network
        networks = lapply(unique(edge.data[["relation"]]),
                             function(relation) {
                                network.data = edge.data[edge.data[["relation"]] == relation, ]
                                net = igraph::graph_from_data_frame(d = network.data,
                                                                    vertices = vertex.data,
                                                                    directed = igraph::is.directed(network))

                                ## simplify networks (contract edges and remove loops)
                                net = igraph::simplify(net, edge.attr.comb = EDGE.ATTR.HANDLING,
                                                       remove.multiple = remove.multiple,
                                                       remove.loops = remove.loops)
                                ## TODO perform simplification on edge list?
                                return(net)
        })

        ## merge the simplified networks
        network = merge.networks(networks)
    } else {
        network = igraph::simplify(network, edge.attr.comb = EDGE.ATTR.HANDLING,
                                   remove.multiple = remove.multiple, remove.loops = remove.loops)
    }

    logging::logdebug("simplify.network: finished.")
    return(network)
}

#' Simplify a list of networks.
#'
#' @param networks the list of networks
#' @param remove.multiple whether to contract multiple edges between the same pair of nodes [default: TRUE]
#' @param remove.loops whether to remove loops [default: TRUE]
#'
#' @return the simplified networks
simplify.networks = function(networks, remove.multiple = TRUE, remove.loops = TRUE) {
    logging::logdebug("simplify.networks: starting.")
    logging::loginfo(
        "Simplifying networks (names = [%s]).",
        paste(names(networks), collapse = ", ")
    )

    nets = parallel::mclapply(networks, simplify.network, remove.multiple = remove.multiple,
                              remove.loops = remove.loops)

    logging::logdebug("simplify.networks: finished.")
    return(nets)
}

#' Delete isolate vertices from the given network, i.e., any vertices without any in-going
#' or out-going edges.
#'
#' @param network the network from which isolates are to be removed
#'
#' @return the network without isolates
delete.isolates = function(network) {
    network.no.isolates = igraph::delete.vertices(
        network,
        igraph::degree(network, mode = "all") == 0
    )
    return(network.no.isolates)
}


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Multi-network views -----------------------------------------------------

#' Extract the author-network part from the given (multi) network, i.e.,
#' return the subgraph induced by the vertices of type TYPE.AUTHOR.
#'
#' @param network the (multi) network to reduce
#' @param remove.isolates whether to remove isolates after extraction [default: FALSE]
#'
#' @return the author-vertex-induced subgraph of \code{network}
extract.author.network.from.network = function(network, remove.isolates = FALSE) {
    ## only retain all author vertices
    author.network = igraph::induced.subgraph(network, igraph::V(network)[type == TYPE.AUTHOR])
    ## remove isolates if wanted
    if (remove.isolates) {
        author.network = delete.isolates(author.network)
    }
    return(author.network)
}

#' Extract the artifact-network part from the given (multi) network, i.e.,
#' return the subgraph induced by the vertices of type TYPE.ARTIFACT.
#'
#' @param network the (multi) network to reduce
#' @param remove.isolates whether to remove isolates after extraction [default: FALSE]
#'
#' @return the artifact-vertex-induced subgraph of \code{network}
extract.artifact.network.from.network = function(network, remove.isolates = FALSE) {
    ## only retain all artifact vertices
    artifact.network = igraph::induced.subgraph(network, igraph::V(network)[type == TYPE.ARTIFACT])
    ## remove isolates if wanted
    if (remove.isolates) {
        artifact.network = delete.isolates(artifact.network)
    }
    return(artifact.network)
}

#' Extract the bipartite-network part from the given (multi) network, i.e.,
#' return the subgraph induced by the edges of type TYPE.EDGES.INTER.
#'
#' @param network the (multi) network to reduce
#'
#' @return the bipartite-edge-induced subgraph of \code{network}
extract.bipartite.network.from.network = function(network) {
    ## only retain all bipartite edges and induced vertices
    bip.network = igraph::subgraph.edges(network, igraph::E(network)[type == TYPE.EDGES.INTER])
    return(bip.network)
}


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Sample network ----------------------------------------------------------

SAMPLE.DATA = normalizePath("./sample")

#' Get a example network for illustration purposes.
#'
#' @return the sample network
get.sample.network = function() {

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
