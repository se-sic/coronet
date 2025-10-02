## This file is part of coronet, which is free software: you
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
## Copyright 2016-2019 by Claus Hunsen <hunsen@fim.uni-passau.de>
## Copyright 2017 by Raphael Nömmer <noemmer@fim.uni-passau.de>
## Copyright 2017-2018 by Christian Hechtl <hechtl@fim.uni-passau.de>
## Copyright 2024 by Christian Hechtl <hechtl@cs.uni-saarland.de>
## Copyright 2017-2019 by Thomas Bock <bockthom@fim.uni-passau.de>
## Copyright 2021, 2023-2024 by Thomas Bock <bockthom@cs.uni-saarland.de>
## Copyright 2018 by Barbara Eckl <ecklbarb@fim.uni-passau.de>
## Copyright 2018-2019 by Jakob Kronawitter <kronawij@fim.uni-passau.de>
## Copyright 2020 by Anselm Fehnker <anselm@muenster.de>
## Copyright 2021 by Niklas Schneider <s8nlschn@stud.uni-saarland.de>
## Copyright 2022 by Jonathan Baumann <joba00002@stud.uni-saarland.de>
## Copyright 2023-2025 by Maximilian Löffler <s8maloef@stud.uni-saarland.de>
## Copyright 2024 by Leo Sendelbach <s8lesend@stud.uni-saarland.de>
## All Rights Reserved.


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Libraries ---------------------------------------------------------------

requireNamespace("R6") # for R6 classes
requireNamespace("logging") # for logging
requireNamespace("parallel") # for parallel computation
requireNamespace("plyr") # for dlply function
requireNamespace("igraph") # networks
requireNamespace("lubridate") # for date conversion


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Vertex and edge types ---------------------------------------------------

## vertex types
TYPE.AUTHOR = "Author"
TYPE.ARTIFACT = "Artifact"
TYPE.COMMIT = "Commit"

## edge types
TYPE.EDGES.INTRA = "Unipartite"
TYPE.EDGES.INTER = "Bipartite"


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Edge-attribute handling during simplification ---------------------------

## Edge-attribute contraction: configure handling of attributes by name
## Attention: If attributes are added or change to new aggregation strategies, this
##            may influence logic in \code{split.network.by.bins}. For additional
##            documentation, see \link{https://github.com/se-sic/coronet/pull/278}.
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

    ## everything else:
    ##
    ## this helper function concatenates attribute
    ## values together into a single list
    function(attr) {
        if (any(sapply(attr, is.list))) {
            attr = do.call(base::c, attr)
        }
        return(attr)
    }
)


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Constants ---------------------------------------------------------------

## mapping of relation to data source
RELATION.TO.DATASOURCE = list(
    "cochange"  = "commits",
    "callgraph" = "commits",
    "mail"      = "mails",
    "issue"     = "issues"
)

## A value of \code{TRUE} indicates that the corresponding network will be built using the directed edge
## construction algorithm analogous \code{FALSE} means the undirected edge construction algorithm is used
ENFORCED.DIRECTEDNESS = list(
    "author"   = list(),
    "artifact" = list("cochange" = FALSE),
    "commit"   = list()
)


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## NetworkBuilder ----------------------------------------------------------

#' The class \code{NetworkBuilder} provides convenient network construction in a
#' lazy fashion.
#'
#' To configure an object of this network-construction class, a configuration
#' object of class \code{NetworkConf} and a data-carrying object of class
#' \code{ProjectData} must be passed to the initialization function.
#'
#' The following types of networks can be constructed:
#' - author networks,
#' - artifact networks,
#' - bipartite networks, and
#' - multi networks (a combination of all other types of networks).
#'
#' @seealso NetworkConf
#' @seealso ProjectData
NetworkBuilder = R6::R6Class("NetworkBuilder",

    ## * private -----------------------------------------------------------

    private = list(
        ## * * data and configuration --------------------------------------

        proj.data = NULL,
        proj.data.original = NULL,
        network.conf = NULL,

        ## * * network data caching ----------------------------------------

        author.network.cochange.data = NULL,
        author.network.mail.data = NULL,
        author.network.issue.data = NULL,
        author.network.commit.interaction.data = NULL,
        artifact.network.cochange.data = NULL,
        artifact.network.mail.data = NULL,
        artifact.network.issue.data = NULL,
        artifact.network.commit.interaction.data = NULL,
        artifact.network.callgraph.data = NULL,
        commit.network.cochange.data = NULL,
        commit.network.commit.interaction.data = NULL,
        bipartite.relations = NULL,

        ## * * relation-to-vertex-kind mapping -----------------------------

        #' Determine which vertex kind should be chosen for the vertex depending on the relation
        #' between the vertices.
        #'
        #' @param relation the given relation
        #'
        #' @return the vertex kind to be used
        get.vertex.kind.for.relation = function(relation) {

            vertex.kind = switch(relation,
                cochange           = private$proj.data$get.project.conf.entry("artifact.codeface"),
                callgraph          = private$proj.data$get.project.conf.entry("artifact.codeface"),
                mail               = "MailThread",
                issue              = "Issue",
                commit.interaction = private$proj.data$get.project.conf.entry("artifact.codeface")
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

        ## * * helper functions --------------------------------------------

        #' Construct a network data object from (possibly empty) vertex and edge data.
        #'
        #' @param vertex.data the vertex data frame
        #' @param edge.data the edge data frame
        #' @param possible.edge.attributes a list of all possible attributes and their datatypes that could be present
        #'                                 in \code{edge.data} depending on the data source of the represented network.
        #'                                 This list is only used if \code{edge.data} is \code{NULL} or empty.
        #'
        #' @return the network data object
        construct.network.data = function(vertex.data, edge.data, possible.edge.attributes) {

            configured.edge.attributes = private$network.conf$get.value("edge.attributes")

            ## add missing vertex attributes
            if (is.null(vertex.data) || nrow(vertex.data) == 0) {
                vertex.data = data.frame(name = character(0))
            }

            ## add missing edge attributes if edgelist was empty
            if (is.null(edge.data) || nrow(edge.data) == 0) {

                ## determine edge attributes to add
                possible.edge.attributes = lapply(possible.edge.attributes, function(attr) attr[1])
                required.edge.attributes = configured.edge.attributes[configured.edge.attributes %in%
                                                                      names(possible.edge.attributes)]

                ## construct empty edges with required edge attributes
                edge.data = create.empty.data.frame(
                    c("from", "to", required.edge.attributes),
                    c("character", "character", possible.edge.attributes[required.edge.attributes])
                )
            }

            ## convert edge attributes to list type
            edge.data = convert.edge.list.attributes.to.list(edge.data)

            ## add weight attribute to edges
            edge.data[["weight"]] = rep(1, nrow(edge.data))

            ## construct network data
            network.data = list(
                vertices = vertex.data,
                edges = edge.data
            )

            return(network.data)
        },

        #' Determine the directedness a to-be-built network should have
        #' based on the configured and enforced directedness
        #'
        #' @param network.type the type of network to build default: [c("author", "artifact", "commit")]
        #'
        #' @return the inferred directedness of the network
        determine.directedness = function(network.type = c("author", "artifact", "commit")) {

            network.type = match.arg.or.default(network.type, default = "author", several.ok = TRUE)

            ## collect configured and enforced directedness
            configured.directedness = list()
            enforced.directedness = list()

            for (type in network.type) {

                ## get enforced directedness
                relations = private$network.conf$get.value(paste0(type, ".relation"))
                enforced = ENFORCED.DIRECTEDNESS[[type]]
                enforced = enforced[names(enforced) %in% relations]
                enforced.directedness[[type]] = enforced

                ## get configured directedness
                configured = private$network.conf$get.value(paste0(type, ".directed"))
                configured.directedness[[type]] = configured
            }

            ## if at least one network enforces undirectedness all networks need to be undirected,
            ## i.e., \code{directed} can only be \code{TRUE} if all enforced directedness are \code{TRUE}
            if (any(sapply(enforced.directedness, length) > 0)) {
                directed = all(unlist(enforced.directedness))
            }

            ## if no directedness is enforced, use the configured values
            ## if at least one network is configured to be undirected all networks need to be undirected
            else {
                directed = all(unlist(configured.directedness))
            }

            ## print a warning if some configured directedness differ from the enforced directedness
            overwritten.configurations = names(configured.directedness)[configured.directedness != directed]
            if (length(overwritten.configurations) > 0) {
                notification.string = paste("The enforced directedness for the construction of the %s",
                                            "network(s) differs from the configured directedness. Enforced",
                                            "directedness: %s, configured directedness: %s")
                logging::logwarn(notification.string, overwritten.configurations, directed, !directed)
            }

            return(directed)
        },

        ## * * author networks ---------------------------------------------

        #' Get the co-change-based author relation as network.
        #' If it does not already exist build it first.
        #'
        #' @return the author network with cochange relation
        get.author.network.cochange = function(directed) {
            logging::logdebug("get.author.network.cochange: starting.")

            ## do not compute anything more than once
            if (!is.null(private$author.network.cochange.data)) {
                logging::logdebug("get.author.network.cochange: finished. (already existing)")
                return(private$author.network.cochange.data)
            }

            ## Get a list of all artifacts extracted from the commit data. Each artifact in this group is again a list
            ## of all authors that were involved in making changes to this artifact. In the following two steps, some of
            ## the artifacts are filtered from this list, which removes all information (including author information)
            ## about these artifacts. Since we only want to lose the edge information and not the information about
            ## authors, they will explicitly be added in a later step.
            author.groups = private$proj.data$group.authors.by.data.column("commits", "artifact")
            ## 1) if configured in the 'NetworkConf, remove the base artifact
            if (!private$network.conf$get.value("edges.for.base.artifacts")) {
                author.groups = author.groups[!(names(author.groups) %in% BASE.ARTIFACTS)]
            }
            ## 2) in any case, remove the untracked files
            author.groups = author.groups[names(author.groups) != UNTRACKED.FILE.EMPTY.ARTIFACT]

            ## construct edge list based on artifact2author data
            author.net.data = construct.edge.list.from.key.value.list(
                author.groups,
                network.conf = private$network.conf,
                directed = directed,
                respect.temporal.order = private$network.conf$get.value("author.respect.temporal.order")
            )

            ## Add author vertices back into the graph. Previously, commit information on untracked files
            ## ('UNTRACKED.FILE') and, if configured, the base artifact ('BASE.ARTIFACTS') has been removed and, hence,
            ## also corresponding author information. Re-add author vertices back to the network now by accessing the
            ## complete author list:
            ## 1) get all authors on commits
            authors = private$proj.data$get.authors.by.data.source(data.sources = "commits")
            ## 2) only select author names
            authors = authors["author.name"]
            ## 3) rename single column to "name" to correct mapping to vertex attribute "name"
            colnames(authors) = "name"

            ## construct network data
            network.data = private$construct.network.data(
                vertex.data = authors,
                edge.data = author.net.data[["edges"]],
                possible.edge.attributes = private$proj.data$get.data.columns.for.data.source("commits")
            )

            ## store network data
            private$author.network.cochange.data = network.data
            logging::logdebug("get.author.network.cochange: finished.")

            return(network.data)
        },

        #' Build and get the author network with commit-interactions as the relation.
        #'
        #'  @return the commit-interaction author network
        get.author.network.commit.interaction = function(directed) {
            logging::logdebug("get.author.network.commit.interaction: starting.")

            ## do not compute anything more than once
            if (!is.null(private$author.network.commit.interaction.data)) {
                logging::logdebug("get.author.network.commit.interaction: finished. (already existing)")
                return(private$author.network.commit.interaction.data)
            }

            ## get the authors that appear in the commit-interaction data as the vertices of the network
            vertices = unique(c(private$proj.data$get.commit.interactions()[["base.author"]],
                                private$proj.data$get.commit.interactions()[["interacting.author"]]))
            vertices = data.frame(name = vertices)

            ## get the commit-interaction data as the edge data of the network
            edges = private$proj.data$get.commit.interactions()
            ## set the authors as the 'to' and 'from' of the network and order the dataframe
            edges = edges[, c("base.author", "interacting.author", "func", "commit.hash",
                              "file", "base.hash", "base.func", "base.file")]
            colnames(edges)[1] = "to"
            colnames(edges)[2] = "from"
            colnames(edges)[4] = "hash"
            if (nrow(edges) > 0) {
                edges[["artifact.type"]] = ARTIFACT.COMMIT.INTERACTION
            }

            ## construct network data
            network.data = private$construct.network.data(
                vertex.data = vertices,
                edge.data = edges,
                possible.edge.attributes = private$proj.data$get.data.columns.for.data.source("commit.interactions")
            )

            ## store network data
            author.network.commit.interaction.data = network.data
            logging::logdebug("get.author.network.commit.interaction: finished.")

            return(network.data)
        },

        #' Get the thread-based author relation as network.
        #' If it does not already exist build it first.
        #'
        #' @return the author network with mail relation
        get.author.network.mail = function(directed) {

            logging::logdebug("get.author.network.mail: starting.")

            ## do not compute anything more than once
            if (!is.null(private$author.network.mail.data)) {
                logging::logdebug("get.author.network.mail: finished. (already existing)")
                return(private$author.network.mail.data)
            }

            ## construct edge list based on thread2author data
            author.net.data = construct.edge.list.from.key.value.list(
                private$proj.data$group.authors.by.data.column("mails", "thread"),
                network.conf = private$network.conf,
                directed = directed,
                respect.temporal.order = private$network.conf$get.value("author.respect.temporal.order")
            )

            ## construct network data
            network.data = private$construct.network.data(
                vertex.data = author.net.data[["vertices"]],
                edge.data = author.net.data[["edges"]],
                possible.edge.attributes = private$proj.data$get.data.columns.for.data.source("mails")
            )

            ## store network data
            private$author.network.mail.data = network.data
            logging::logdebug("get.author.network.mail: finished.")

            return(network.data)
        },

        ##get the issue based author relation as network
        get.author.network.issue = function(directed) {
            logging::logdebug("get.author.network.issue: starting.")

            if (!is.null(private$author.network.issue.data)) {
                logging::logdebug("get.author.network.issue: finished. (already existing)")
                return(private$author.network.issue.data)
            }

            ## construct edge list based on issue2author data
            author.net.data = construct.edge.list.from.key.value.list(
                private$proj.data$group.authors.by.data.column("issues", "issue.id"),
                network.conf = private$network.conf,
                directed = directed,
                respect.temporal.order = private$network.conf$get.value("author.respect.temporal.order")
            )

            ## construct network data
            network.data = private$construct.network.data(
                vertex.data = author.net.data[["vertices"]],
                edge.data = author.net.data[["edges"]],
                possible.edge.attributes = private$proj.data$get.data.columns.for.data.source("issues")
            )

            ## store network data
            private$author.network.issue.data = network.data
            logging::logdebug("get.author.network.issue: finished.")

            return(network.data)
        },

        ## * * artifact networks -------------------------------------------

        #' Get the co-change-based artifact network,
        #' If it does not already exist build it first.
        #'
        #' @return the artifact network with cochange realtion
        get.artifact.network.cochange = function(directed) {

            logging::logdebug("get.artifact.network.cochange: starting.")

            ## do not compute anything more than once
            if (!is.null(private$artifact.network.cochange.data)) {
                logging::logdebug("get.artifact.network.cochange: finished. (already existing)")
                return(private$artifact.network.cochange.data)
            }

            ## construct edge list based on commit--artifact data
            artifacts.net.data.raw = private$proj.data$group.artifacts.by.data.column("commits", "hash")

            artifacts.net.data = construct.edge.list.from.key.value.list(
                artifacts.net.data.raw,
                network.conf = private$network.conf,
                directed = directed,
                respect.temporal.order = TRUE,
                network.type = "artifact"
            )

            ## extract vertices and edges
            vertices = artifacts.net.data[["vertices"]]
            edges = artifacts.net.data[["edges"]]

            ## remove the artifact vertices stemming from untracked files if existing
            if ("name" %in% names(vertices) && any(vertices[["name"]] == UNTRACKED.FILE.EMPTY.ARTIFACT)) {
                vertices = vertices[vertices[["name"]] != UNTRACKED.FILE.EMPTY.ARTIFACT, , drop = FALSE]
                edges = edges[!(edges[["from"]] == UNTRACKED.FILE.EMPTY.ARTIFACT |
                        edges[["to"]] == UNTRACKED.FILE.EMPTY.ARTIFACT), ]
            }

            ## construct network data
            network.data = private$construct.network.data(
                vertex.data = vertices,
                edge.data = edges,
                possible.edge.attributes = private$proj.data$get.data.columns.for.data.source("commits")
            )

            ## store network data
            private$artifact.network.cochange.data = network.data
            logging::logdebug("get.artifact.network.cochange: finished.")

            return(network.data)
        },

        #' Build and get the commit-interaction based artifact network.
        #'
        #' @return the commit-interaction based artifact network
        get.artifact.network.commit.interaction = function(directed) {

            logging::logdebug("get.artifact.network.commit.interaction: starting.")

            ## do not compute anything more than once
            if (!is.null(private$artifact.network.commit.interaction.data)) {
                logging::logdebug("get.artifact.network.commit.interaction: finished. (already existing)")
                return(private$artifact.network.commit.interaction.data)
            }

            ## initialize the vertices. They will be set correctly depending on the used config.
            vertices = c()

            ## get the commit-interaction data as the edge data of the network
            edges = private$proj.data$get.commit.interactions()

            ## set 'to' and 'from' of the network according to the config
            ## and order the dataframe accordingly
            proj.conf.artifact = private$proj.data$get.project.conf.entry("artifact")
            if (proj.conf.artifact == "file") {

                ## change the vertices to the files from the commit-interaction data
                vertices = unique(c(private$proj.data$get.commit.interactions()[["base.file"]],
                                    private$proj.data$get.commit.interactions()[["file"]]))
                vertices = data.frame(name = vertices)

                edges = edges[, c("file", "base.file", "func", "commit.hash",
                                  "base.hash", "base.func", "base.author", "interacting.author")]

            } else if (proj.conf.artifact == "function") {

                ## change the vertices to the functions from the commit-interaction data
                vertices = unique(c(private$proj.data$get.commit.interactions()[["base.func"]],
                                    private$proj.data$get.commit.interactions()[["func"]]))
                vertices = data.frame(name = vertices)

                edges = edges[, c("func", "base.func", "commit.hash", "file", "base.hash",
                                  "base.file", "base.author", "interacting.author")]

            } else {

                ## If neither 'function' nor 'file' was configured, send a warning
                ## and return an empty network
                logging::logwarn("when creating a commit-interaction artifact network,
                                  the artifact should be either 'file' or 'function'!")
                return(create.empty.network(directed = directed))
            }

            if (nrow(edges) > 0) {
                edges[["artifact.type"]] = ARTIFACT.CODEFACE[[proj.conf.artifact]]
            }
            colnames(edges)[colnames(edges) == "commit.hash"] = "hash"

            colnames(edges)[1] = "to"
            colnames(edges)[2] = "from"

            ## construct network data
            network.data = private$construct.network.data(
                vertex.data = vertices,
                edge.data = edges,
                possible.edge.attributes = private$proj.data$get.data.columns.for.data.source("commit.interactions")
            )

            ## store network data
            private$artifact.network.commit.interaction.data = network.data
            logging::logdebug("get.artifact.network.commit.interaction: finished.")

            return(network.data)
        },

        #' Get the call-graph-based artifact network.
        #' If it does not already exist build it first.
        #' IMPORTANT: This only works for range-level analyses!
        #'
        #' @return the artifact network with callgraph relation
        get.artifact.network.callgraph = function(directed) {

            logging::logdebug("get.artifact.network.callgraph: starting.")

            ## do not compute anything more than once
            if (!is.null(private$artifact.network.callgraph.data)) {
                logging::logdebug("get.artifact.network.callgraph: finished. (already existing)")
                return(private$artifact.network.callgraph.data)
            }

            ## check if revision for call-graphs is set
            if (!("RangeData" %in% class(private$proj.data)) ||
                is.na(private$proj.data$get.revision.callgraph())) {

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
            artifacts.net = igraph::read_graph(file, format = "pajek")

            # set vertex labels properly (copy "id" attribute to "name" attribute)
            artifacts.net = igraph::set_vertex_attr(
                artifacts.net,
                "name",
                igraph::V(artifacts.net),
                igraph::vertex_attr(artifacts.net, "id")
            )

            ## process vertex names in artifact networks for consistent names:
            ## Since feature and file networks may have unique naming structures, the names
            ## need to be processed in order to match the ones coming from other analyses
            ## (e.g. Codeface):
            ## (1) retrieve parameters for processing
            names = igraph::vertex_attr(artifacts.net, "name")
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
            artifacts.net = igraph::set_vertex_attr(artifacts.net, "name", value = names)

            ## set edge attribute 'artifact.type' as the raw data do not contain this!
            artifacts.net = igraph::set_edge_attr(
                artifacts.net, "artifact.type",
                value = private$proj.data$get.project.conf.entry("artifact.codeface")
            )

            ## construct network data
            network.data = private$construct.network.data(
                vertex.data = igraph::as_data_frame(artifacts.net, "vertices"),
                edge.data = igraph::as_data_frame(artifacts.net, "edges"),
                possible.edge.attributes = private$proj.data$get.data.columns.for.data.source("commits")
            )

            ## store network data
            private$artifact.network.callgraph.data = network.data
            logging::logdebug("get.artifact.network.callgraph: finished.")

            return(network.data)

        },

        #' Get the mail-based artifact network.
        #' If it does not already exist build it first.
        #'
        #' @return the artifact network with mail relation
        get.artifact.network.mail = function(directed) {

            logging::logdebug("get.artifact.network.mail: starting.")

            ## do not compute anything more than once
            if (!is.null(private$artifact.network.mail.data)) {
                logging::logdebug("get.artifact.network.mail: finished. (already existing)")
                return(private$artifact.network.mail.data)
            }

            ## log warning as we do not have relations among threads right now
            logging::logwarn(paste(
                "There exist no actual artifact network with the relation 'mail'.",
                "Return an edge-less network now."
            ))

            ## construct edgeless network data
            network.data = private$construct.network.data(
                vertex.data = data.frame(name = private$proj.data$get.artifacts("mails")),
                edge.data = NULL,
                possible.edge.attributes = private$proj.data$get.data.columns.for.data.source("mails")
            )

            ## store network data
            private$artifact.network.mail.data = network.data
            logging::logdebug("get.artifact.network.mail: finished.")

            return(network.data)
        },

        #' Get the issue-based artifact network.
        #' If it does not already exist build it first.
        #'
        #' @return the artifact network with issue relation
        get.artifact.network.issue = function(directed) {

            logging::logdebug("get.artifact.network.issue: starting.")

            ## do not compute anything more than once
            if (!is.null(private$artifact.network.issue.data)) {
                logging::logdebug("get.artifact.network.issue: finished. (already existing)")
                return(private$artifact.network.issue.data)
            }

            if (private$proj.data$get.project.conf()$get.entry("issues.only.comments")) {
                logging::logwarn(paste(
                    "Create an edge-less artifact network as 'issues.only.comments' is set.",
                    "Comments in issues cannot create issue edges."
                ))
            }

            ## construct edge list based on issue-artifact data
            artifacts.net.data.raw = private$proj.data[[DATASOURCE.TO.ARTIFACT.FUNCTION[["issues"]]]]()

            ## obtain issue-connecting events
            add.links = artifacts.net.data.raw[artifacts.net.data.raw$event.name == "add_link" &
                                               artifacts.net.data.raw$event.info.2 == "issue", ]
            referenced.bys = artifacts.net.data.raw[artifacts.net.data.raw$event.name == "referenced_by" &
                                               artifacts.net.data.raw$event.info.2 == "issue", ]

            ## the codeface extraction for jira issues creates duplicate events, linking the referenced issue
            ## to the referencing issue, in addition to the correct events, linking the referencing issue to
            ## the referenced issue. We can only deduplicate them, if we build an undirected network, as otherwise,
            ## we would need to guess the correct direction.
            if (!directed) {

                ## obtain 'add_link' events from jira
                jira.add.links = add.links[add.links$issue.source == "jira", ]
                matched = list()

                ## iterate over all add_link events from jira
                for (i in 1:nrow(jira.add.links)) {

                    add.link = jira.add.links[i, ]

                    ## ensure not to remove both duplicate edges
                    if (any(sapply(matched, function(entry) identical(entry, add.link)))) {
                        next
                    }

                    ## match any 'add_link' events, that are the reverse direction of 'add.link',
                    ## but have the same timestamp and author information
                    match = jira.add.links[(
                        jira.add.links$issue.id == add.link$event.info.1 &
                        jira.add.links$event.info.1 == add.link$issue.id &
                        jira.add.links$date == add.link$date &
                        jira.add.links$author.name == add.link$author.name), ]

                    ## if a match is found, remove 'add.link' and its corresponding 'referenced_by' event
                    if (nrow(match) > 0) {
                        add.links = add.links[!(
                            add.links$issue.id == match$issue.id &
                            add.links$event.info.1 == match$event.info.1 &
                            add.links$date == match$date &
                            add.links$author.name == match$author.name), ]
                        referenced.bys = referenced.bys[!(
                            referenced.bys$issue.id == add.link$issue.id &
                            referenced.bys$event.info.1 == add.link$event.info.1 &
                            referenced.bys$date == add.link$date &
                            referenced.bys$author.name == add.link$author.name), ]
                        matched = append(matched, list(match))
                    }
                }
            }


            if (nrow(add.links) != nrow(referenced.bys)) {
                logging::logwarn("Inconsistent issue data. Unequally many 'add_link' and 'referenced_by' issue-events.")
            }

            vertices = unique(artifacts.net.data.raw[["issue.id"]])
            edge.list = data.frame()

            # edges in artifact networks can not have the 'artifact' attribute but should instead have
            # the 'author.name' attribute as events caused by authors connect issues
            edge.attributes = private$network.conf$get.value("edge.attributes")
            artifact.index = match("artifact", edge.attributes, nomatch = NA)
            if (!is.na(artifact.index)) {
                edge.attributes = edge.attributes[-artifact.index]
                if (!("author.name" %in% edge.attributes)) {
                    edge.attributes = c(edge.attributes, c("author.name"))
                }
            }

            ## connect corresponding add_link and referenced_by issue-events
            edge.list = plyr::rbind.fill(parallel::mclapply(split(add.links, seq_len(nrow(add.links))), function(from) {
                ## get edge attributes
                cols.which = edge.attributes %in% colnames(from)
                edge.attrs = from[, edge.attributes[cols.which], drop = FALSE]

                ## construct edge
                to = subset(referenced.bys,
                            event.info.1 == from[["issue.id"]] &
                            author.name == from[["author.name"]] &
                            date == from[["date"]])
                if (!all(is.na(to))) {
                    combination = list("from" = from[["issue.id"]], "to" = to[["issue.id"]])
                    combination = cbind(combination, edge.attrs, row.names = NULL) # add edge attributes
                    return(combination) # return the combination for this row
                }
            }))

            ## construct network data
            network.data = private$construct.network.data(
                vertex.data = data.frame(name = vertices),
                edge.data = edge.list,
                possible.edge.attributes = private$proj.data$get.data.columns.for.data.source("issues")
            )

            ## store network data
            private$artifact.network.issue.data = network.data
            logging::logdebug("get.artifact.network.issue: finished.")

            return(network.data)
        },

        #' Build and get the commit network with commit-interactions as the relation.
        #'
        #'  @return the commit-interaction commit network
        get.commit.network.commit.interaction = function(directed) {

            logging::logdebug("get.commit.network.commit.interaction: starting.")

            ## do not compute anything more than once
            if (!is.null(private$commit.network.commit.interaction.data)) {
                logging::logdebug("get.commit.network.commit.interaction: finished. (already existing)")
                return(private$commit.network.commit.interaction.data)
            }

            ## get the hashes that appear in the commit-interaction data as the vertices of the network
            vertices = unique(c(private$proj.data$get.commit.interactions()[["base.hash"]],
                                private$proj.data$get.commit.interactions()[["commit.hash"]]))
            vertices = data.frame(name = vertices)

            ## get the commit-interaction data as the edge data of the network
            edges = private$proj.data$get.commit.interactions()
            ## set the commits as the 'to' and 'from' of the network and order the dataframe
            edges = edges[, c("base.hash", "commit.hash", "func", "interacting.author",
                              "file", "base.author", "base.func", "base.file")]
            if (!is.null(edges)) {
                if (nrow(edges) > 0) {
                    edges[["artifact.type"]] = ARTIFACT.COMMIT.INTERACTION
                }
                colnames(edges)[1] = "to"
                colnames(edges)[2] = "from"
            }

            ## construct network data
            network.data = private$construct.network.data(
                vertex.data = vertices,
                edge.data = edges,
                possible.edge.attributes = private$proj.data$get.data.columns.for.data.source("commit.interactions")
            )

            ## store network data
            private$commit.network.commit.interaction.data = network.data
            logging::logdebug("get.commit.network.commit.interaction: finished.")

            return(network.data)
        },

        #' Get the cochange-based commit network,
        #' If it does not already exist build it first.
        #'
        #' @return the commit network with cochange realtion
        get.commit.network.cochange = function(directed) {

            logging::logdebug("get.commit.network.cochange: starting.")

            ## do not compute anything more than once
            if (!is.null(private$commit.network.cochange.data)) {
                logging::logdebug("get.commit.network.cochange: finished. (already existing)")
                return(private$commit.network.cochange.data)
            }

            ## construct edge list based on commit--artifact data
            commit.net.data.raw = private$proj.data$group.commits.by.data.column("artifact")

            commit.net.data = construct.edge.list.from.key.value.list(
                commit.net.data.raw,
                network.conf = private$network.conf,
                directed = directed,
                respect.temporal.order = TRUE,
                network.type = "commit"
            )

            ## construct network data
            network.data = private$construct.network.data(
                vertex.data = commit.net.data[["vertices"]],
                edge.data = commit.net.data[["edges"]],
                possible.edge.attributes = private$proj.data$get.data.columns.for.data.source("commits")
            )

            ## store network data
            private$commit.network.cochange.data = network.data
            logging::logdebug("get.commit.network.cochange: finished.")

            return(network.data)
        },

        ## * * bipartite relations ------------------------------------------

        #' Get the key-value data for the bipartite relations,
        #' which are implied by the "artifact.relation" from the network configuration.
        #'
        #' @return a named list of data for the bipartite relations, each with the attributes
        #'         'vertex.kind' denoting the artifact type for the relations
        #'         and 'relation' denoting the respective network-configuration entry; the names
        #'         are the relations configured by the attribute 'artifact.relation' of the
        #'         network configuration
        get.bipartite.relations = function() {
            logging::logdebug("get.bipartite.relations: starting.")

            ## do not compute anything more than once
            if (!is.null(private$bipartite.relations)) {
                logging::logdebug("get.bipartite.relations: finished. (already existing)")
                return(private$bipartite.relations)
            }

            relations = private$network.conf$get.variable("artifact.relation")
            logging::logdebug("Using bipartite relations '%s'.", relations)

            bip.relations = lapply(relations, function(relation) {
                ## get data for current bipartite relation
                data.source = RELATION.TO.DATASOURCE[[relation]]
                bip.relation = private$proj.data$group.artifacts.by.data.column(data.source, "author.name")

                ## set vertex.kind and relation attributes
                attr(bip.relation, "vertex.kind") = private$get.vertex.kind.for.relation(relation)
                attr(bip.relation, "relation") = relation

                return(bip.relation)
            })
            names(bip.relations) = relations
            private$bipartite.relations = bip.relations

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
            private$proj.data.original = verify.argument.for.parameter(project.data, "ProjectData", "NetworkBuilder$new")
            private$proj.data = project.data$clone()
            private$network.conf = verify.argument.for.parameter(network.conf, "NetworkConf", "NetworkBuilder$new")

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
            private$author.network.cochange.data = NULL
            private$author.network.mail.data = NULL
            private$author.network.issue.data = NULL
            private$author.network.commit.interaction.data = NULL
            private$artifact.network.cochange.data = NULL
            private$artifact.network.mail.data = NULL
            private$artifact.network.issue.data = NULL
            private$artifact.network.commit.interaction.data = NULL
            private$artifact.network.callgraph.data = NULL
            private$commit.network.cochange.data = NULL
            private$commit.network.commit.interaction.data = NULL
            private$bipartite.relations = NULL
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

        #' Get a value of the network configuration.
        #'
        #' @return the value of the given entry name
        get.network.conf.entry = function(entry) {
            return(private$network.conf$get.value(entry))
        },

        #' Set a value of the network configuration and reset the environment.
        #'
        #' @param entry the configuration option to set
        #' @param value the new value that is assigned to the configuration parameter
        set.network.conf.entry = function(entry, value) {
            private$network.conf$update.value(entry, value)
            self$reset.environment()
        },

        #' Update the network configuration based on the given list
        #' of values and reset the environment afterwards.
        #'
        #' @param updated.values the new values for the network configuration [default: list()]
        update.network.conf = function(updated.values = list()) {
            private$network.conf$update.values(updated.values = updated.values)
            self$reset.environment()
        },

        #' Get the ProjectData object of the NetworkBuilder.
        #' This method is mainly used for testing purposes at the moment.
        #'
        #' @return the project data object of the NetworkBuilder
        get.project.data = function() {
            return(private$proj.data)
        },

        ## * * networks ----------------------------------------------------

        #' Get the generic author network.
        #'
        #' @return the generic author network
        get.author.network = function() {
            logging::loginfo("Constructing author network.")

            relations = private$network.conf$get.value("author.relation")
            directed = private$determine.directedness("author")

            ## construct network
            network.data = lapply(relations, function(relation) {
                network.data = switch(
                    relation,
                    cochange = private$get.author.network.cochange(directed),
                    commit.interaction = private$get.author.network.commit.interaction(directed),
                    mail = private$get.author.network.mail(directed),
                    issue = private$get.author.network.issue(directed),
                    stop(sprintf("The author relation '%s' does not exist.", rel))
                    ## TODO construct edge lists here and merge those (inline the private methods)
                )

                ## set edge attributes on all edges
                edge.count = nrow(network.data[["edges"]])
                network.data[["edges"]][["type"]]     = rep(TYPE.EDGES.INTRA, edge.count)
                network.data[["edges"]][["relation"]] = rep(list(list(relation)), edge.count)

                return(network.data)
            })
            merged.network.data = merge.network.data(network.data)

            ## construct graph from network data
            net = igraph::graph_from_data_frame(
                merged.network.data[["edges"]],
                vertices = merged.network.data[["vertices"]],
                directed = directed
            )

            ## add all missing authors to the network if wanted
            if (private$network.conf$get.value("author.all.authors")) {
                authors.all = private$proj.data$get.authors()[[ "author.name" ]]
                authors.net = igraph::vertex_attr(net, "name")
                net = net + igraph::vertices(setdiff(authors.all, authors.net))
            }

            ## remove all authors from the corresponding network who do not have touched any artifact
            if (private$network.conf$get.value("author.only.committers")) {
                ## authors-artifact relation
                authors.from.net = igraph::vertex_attr(net, "name")
                authors.from.artifacts = lapply(private$get.bipartite.relations(), function(bipartite.relation) {
                    return(names(bipartite.relation))
                })
                authors.from.artifacts = unlist(authors.from.artifacts)
                if (!is.null(authors.from.artifacts)) {
                    net = igraph::delete_vertices(net, setdiff(authors.from.net, authors.from.artifacts))
                }
            }

            ## set vertex attributes for identifaction
            igraph::V(net)$kind = TYPE.AUTHOR
            igraph::V(net)$type = TYPE.AUTHOR

            ## simplify network if wanted
            if (private$network.conf$get.value("simplify")) {
                net = simplify.network(net, simplify.multiple.relations =
                                            private$network.conf$get.value("simplify.multiple.relations"))
            }

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
            directed = private$determine.directedness("artifact")

            network.data = lapply(relations, function(relation) {
                network.data = switch(
                    relation,
                    cochange = private$get.artifact.network.cochange(directed),
                    callgraph = private$get.artifact.network.callgraph(directed),
                    mail = private$get.artifact.network.mail(directed),
                    issue = private$get.artifact.network.issue(directed),
                    commit.interaction = private$get.artifact.network.commit.interaction(directed),
                    stop(sprintf("The artifact relation '%s' does not exist.", relation))
                )

                ## set edge attributes on all edges
                edge.count = nrow(network.data[["edges"]])
                network.data[["edges"]][["type"]]     = rep(TYPE.EDGES.INTRA, edge.count)
                network.data[["edges"]][["relation"]] = rep(list(list(relation)), edge.count)

                ## set vertex attribute 'kind' on all edges, corresponding to relation
                vertex.count = nrow(network.data[["vertices"]])
                network.data[["vertices"]][["kind"]] = rep(private$get.vertex.kind.for.relation(relation), vertex.count)

                return(network.data)
            })
            merged.network.data = merge.network.data(network.data)

            ## construct graph from network data
            net = igraph::graph_from_data_frame(
                merged.network.data[["edges"]],
                vertices = merged.network.data[["vertices"]],
                directed = directed
            )

            ## set vertex and edge attributes for identifaction
            igraph::V(net)$type = TYPE.ARTIFACT

            ## simplify network if wanted
            if (private$network.conf$get.value("simplify")) {
                net = simplify.network(net, simplify.multiple.relations =
                                            private$network.conf$get.value("simplify.multiple.relations"))
            }

            ## add range attribute for later analysis (if available)
            if ("RangeData" %in% class(private$proj.data)) {
                attr(net, "range") = private$proj.data$get.range()
            }

            return(net)
        },

        #' Get the generic commit network.
        #'
        #' @return the generic commit network
        get.commit.network = function() {
            logging::loginfo("Constructing commit network.")

            ## construct network
            relations = private$network.conf$get.value("commit.relation")
            directed = private$determine.directedness("commit")

            network.data = lapply(relations, function(relation) {
                network.data = switch(
                    relation,
                    cochange = private$get.commit.network.cochange(directed),
                    commit.interaction = private$get.commit.network.commit.interaction(directed),
                    stop(sprintf("The commit relation '%s' does not exist.", relation))
                )

                ## set edge attributes on all edges
                edge.count = nrow(network.data[["edges"]])
                network.data[["edges"]][["type"]]     = rep(TYPE.EDGES.INTRA, edge.count)
                network.data[["edges"]][["relation"]] = rep(list(list(relation)), edge.count)

                return(network.data)
            })
            merged.network.data = merge.network.data(network.data)

            ## construct graph from network data
            net = igraph::graph_from_data_frame(
                merged.network.data[["edges"]],
                vertices = merged.network.data[["vertices"]],
                directed = directed
            )

            ## set vertex and edge attributes for identifaction
            igraph::V(net)$kind = TYPE.COMMIT
            igraph::V(net)$type = TYPE.COMMIT

            ## simplify network if wanted
            if (private$network.conf$get.value("simplify")) {
                net = simplify.network(net, simplify.multiple.relations =
                                            private$network.conf$get.value("simplify.multiple.relations"))
            }

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
            directed = private$determine.directedness("author")

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
                    kind = c(
                        rep(TYPE.AUTHOR, length(author.vertices)),
                        rep(vertex.kind , length(artifact.vertices))
                    ),
                    type = c(
                        rep(TYPE.AUTHOR, length(author.vertices)),
                        rep(TYPE.ARTIFACT, length(artifact.vertices))
                    )
                )
                return(vertices)
            })

            ## Merge network data and construct a vertex-only network for now:
            ## 1) construct vertex data (without edges)
            network.data = lapply(vertex.data, function(vertices) {
                return(list(vertices = vertices, edges = NULL))
            })
            vertex.data = merge.network.data(network.data)[["vertices"]]
            ## 2) remove empty artifact, if names are available
            if ("name" %in% colnames(vertex.data)) {
                vertex.data = subset(vertex.data, !(name == UNTRACKED.FILE.EMPTY.ARTIFACT & type == TYPE.ARTIFACT))
            }
            ## 3) obtain all possible data columns, i.e., edge attributes
            possible.edge.attributes = lapply(
                private$network.conf$get.variable("artifact.relation"),
                function(relation) {
                    data.source = RELATION.TO.DATASOURCE[[relation]]
                    data.cols = private$proj.data$get.data.columns.for.data.source(data.source)
                    return(data.cols)
                }
            )
            possible.edge.attributes = unlist(possible.edge.attributes, recursive = FALSE)
            possible.edge.attributes = possible.edge.attributes[
                !duplicated(names(possible.edge.attributes)) # remove duplicates based on names
                ]
            ## 4) construct network without edges
            vertex.network = construct.network.from.edge.list(
                vertices = vertex.data,
                edge.list = create.empty.edge.list(),
                network.conf = private$network.conf,
                directed = directed,
                possible.edge.attributes = possible.edge.attributes
            )

            ## explicitly add vertex attributes if vertex data was empty
            if (nrow(vertex.data) == 0) {
                vertex.network = add.attributes.to.network(vertex.network, "vertex", attributes = list(
                    name = "character", kind = "character", type = "character"
                ))
            }

            ## add edges to the vertex network
            network = add.edges.for.bipartite.relation(
                vertex.network,
                bipartite.relations = bipartite.relation.data,
                private$network.conf,
                possible.edge.attributes = possible.edge.attributes
            )

            ## remove vertices that are not committers if wanted
            if (private$network.conf$get.value("author.only.committers")) {
                committers = unique(private$proj.data$get.commits.unfiltered()[["author.name"]])
                authors = igraph::vertex_attr(network, "name", igraph::V(network)[ type == TYPE.AUTHOR ])
                authors.to.remove = setdiff(authors, committers)
                network = igraph::delete_vertices(network, authors.to.remove)
            }

            ## simplify network if wanted
            if (private$network.conf$get.value("simplify")) {
                network = simplify.network(network, simplify.multiple.relations =
                                                    private$network.conf$get.value("simplify.multiple.relations"))
            }

            ## add range attribute for later analysis (if available)
            if ("RangeData" %in% class(private$proj.data)) {
                attr(network, "range") = private$proj.data$get.range()
            }

            network = convert.edge.attributes.to.list(network)

            return(network)
        },

        #' Get various networks in a list.
        #'
        #' @param network.type the type(s) of network(s) to be constructed
        #'                     [default: c("author", "artifact", "commit", "bipartite")]
        #'
        #' @return networks in a list
        get.networks = function(network.type = c("author", "artifact", "commit", "bipartite")) {

            logging::loginfo("Constructing networks.")

            network.type = match.arg.or.default(network.type, several.ok = TRUE)
            networks = list()

            ## author relation
            if ("author" %in% network.type) {
                networks[["authors.net"]] = self$get.author.network()
            }

            ## artifact relation
            if ("artifact" %in% network.type) {
                networks[["artifacts.net"]] = self$get.artifact.network()
            }

            ## commit relation
            if ("commit" %in% network.type) {
                networks[["commits.net"]] = self$get.commit.network()
            }

            ## bipartite network
            if ("bipartite" %in% network.type) {
                networks[["bipartite.net"]] = self$get.bipartite.network()
            }

            return(networks)
        },

        #' Get the multi network.
        #' The multi network is basically the result of 'get.networks' combined
        #' in one network.
        #'
        #' @return the multi network
        get.multi.network = function() {
            logging::loginfo("Constructing multi network.")

            ## stash configured directedness
            configured.author.directedness = private$network.conf$get.value("author.directed")
            configured.artifact.directedness = private$network.conf$get.value("artifact.directed")

            ## construct the network parts we need for the multi network with the given directedness
            directed = private$determine.directedness(c("author", "artifact"))
            private$network.conf$update.values(list(author.directed = directed,
                                                    artifact.directed = directed))

            ## construct the network parts we need for the multi network
            networks = self$get.networks(network.type = c("author", "artifact"))
            authors.to.artifacts = private$get.bipartite.relations()

            ## restore configured directedness
            private$network.conf$update.values(list(author.directed = configured.author.directedness,
                                                    artifact.directed = configured.artifact.directedness))

            authors.net = networks[["authors.net"]]
            igraph::V(authors.net)$kind = TYPE.AUTHOR
            artifacts.net = networks[["artifacts.net"]]

            ## remove authors from author-artifact relation, if needed:
            ## we only add bipartite edges for authors already present in the author network (this can be
            ## configured by 'author.only.committers', for example), thus, we need to remove any authors
            ## from the author--artifact relation that are superfluous
            authors.from.net = igraph::vertex_attr(authors.net, "name")
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

            artifacts.from.net = igraph::vertex_attr(artifacts.net, "name")
            artifacts.to.add = setdiff(artifacts.all[["data.vertices"]], artifacts.from.net)
            artifacts.to.add.kind = artifacts.all[
                artifacts.all[["data.vertices"]] %in% artifacts.to.add, "artifact.type"
            ]

            ## Adjust vertex attribute to 'Issue' in multi networks
            ## to be consistent with bipartite networks
            artifacts.to.add.kind[artifacts.to.add.kind == "IssueEvent"] = "Issue"

            if (length(artifacts.to.add) > 0) {
                artifacts.net = artifacts.net + igraph::vertices(artifacts.to.add, type = TYPE.ARTIFACT,
                                                                 kind = artifacts.to.add.kind)
            }

            ## check directedness and adapt artifact network if needed
            if (igraph::is_directed(authors.net) && !igraph::is_directed(artifacts.net)) {
                logging::logwarn(paste0("Author network is directed, but artifact network is not.",
                                        "Converting artifact network..."))
                artifacts.net = igraph::as_directed(artifacts.net, mode = "mutual")
            } else if (!igraph::is_directed(authors.net) && igraph::is_directed(artifacts.net)) {
                logging::logwarn(paste0("Author network is undirected, but artifact network is not.",
                                        "Converting artifact network..."))
                artifacts.net = igraph::as.undirected(artifacts.net, mode = "each",
                                                      edge.attr.comb = EDGE.ATTR.HANDLING)
            }

            ## reduce memory consumption by removing temporary data
            rm(networks)
            gc()

            ## combine the networks:
            ## 1) merge the existing networks
            u = igraph::disjoint_union(authors.net, artifacts.net)

            ## 2) replace NULLs in edge attributes with NAs for consistency
            u = Reduce(function(u, attr) {
                values = igraph::edge_attr(u, attr)
                NULLs = sapply(values, is.null)
                if (any(NULLs)) {
                    values[NULLs] = NA
                    u = igraph::set_edge_attr(u, attr, value = values)
                }
                return(u)
            }, igraph::edge_attr_names(u), u)

            ## 3) add the bipartite edges
            u = add.edges.for.bipartite.relation(u, authors.to.artifacts, private$network.conf)

            ## add range attribute for later analysis (if available)
            if ("RangeData" %in% class(private$proj.data)) {
                attr(u, "range") = private$proj.data$get.range()
            }

            u = convert.edge.attributes.to.list(u)
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
#' If \code{directed}, as default, the order of things in the sublist is respected (if
#' \code{respect.temporal.order} is not set manually) and the 'edge.attr's hold the
#' vector of possible edge attributes in the given list.
#'
#' @param list the list of lists with data
#' @param network.conf the network configuration
#' @param directed whether or not the network should be directed [default: FALSE]
#' @param respect.temporal.order whether to respect the temporal order when constructing edges,
#'                               i.e., whether to only add edges from the later event to the previous one.
#'                               If \code{NA} is passed, the default value is taken.
#'                               [default: directed]
#' @param network.type the type of network for which the key value data is to be used as edges
#'                     (one out of "author", "artifact", or "commit")[default: "author"]
#'
#' @return a list of two data.frames named 'vertices' and 'edges' (compatible with return value
#'         of \code{igraph::as.data.frame})
construct.edge.list.from.key.value.list = function(list, network.conf, directed = FALSE,
                                                   respect.temporal.order = directed,
                                                   network.type = c("author", "artifact", "commit")) {

    network.type = match.arg.or.default(network.type, default = "author", several.ok = FALSE)

    logging::loginfo("Create edges.")
    logging::logdebug("construct.edge.list.from.key.value.list: starting.")

    ## if no value for respect.temporal.order is specified (indicated by an NA value
    ## coming from the network configuration), use the value of directed parameter instead
    if (is.na(respect.temporal.order)) {
        respect.temporal.order = directed
    }

    ## initialize an edge list to fill and the set of vertices
    vertices.processed = c()
    edge.list = data.frame()

    keys = names(list)
    keys.number = length(list)


    ## if edges in an artifact network contain the \code{artifact} attribute
    ## replace it with the \code{author.name} attribute as artifacts cannot cause
    ## edges in artifact networks, authors can
    edge.attributes = network.conf$get.value("edge.attributes")
    if (network.type == "artifact") {
        artifact.index = match("artifact", edge.attributes, nomatch = NA)
        if (!is.na(artifact.index)) {
            edge.attributes = edge.attributes[-artifact.index]
            if (!("author.name" %in% edge.attributes)) {
                edge.attributes = c(edge.attributes, c("author.name"))
            }
        }
    }

    ## if edges in a commit network contain 'hash' or 'file' attributes, remove them
    ## as they belong to commits, which are the vertices in commit networks
    if (network.type == "commit") {
        cols.which = which(edge.attributes %in% c("hash", "file"))
        edge.attributes = edge.attributes[-cols.which]
    }

    ## construct edges
    if (respect.temporal.order) {

        ## for all subsets (sets), connect all items in there with the previous ones
        edge.list.data = parallel::mclapply(list, construct.edges.temporal.order, network.conf,
                                            edge.attributes, keys, keys.number, network.type)
    } else {

        ## for all items in the sublists, construct the cartesian product
        edge.list.data = parallel::mclapply(list, construct.edges.no.temporal.order, network.conf,
                                            edge.attributes, keys, keys.number)
    }
    edge.list = plyr::rbind.fill(edge.list.data)

    ## extract names of vertices
    vertex.names = unlist(parallel::mclapply(edge.list.data, function(data) {
        return(attr(data, "vertices.processed"))
    }))

    logging::logdebug("construct.edge.list.from.key.value.list: finished.")

    if (network.type == "commit") {

        ## extract dates of vertices
        vertex.dates = unlist(parallel::mclapply(edge.list.data, function(data) {
            return(attr(data, "vertices.dates.processed"))
        }))

        ## deduplicate vertices by name
        vertices = data.frame(name = vertex.names, date = vertex.dates)
        vertices = vertices[!duplicated(vertices[["name"]]), ]

        return(list(
            vertices = data.frame(
                name = vertices[["name"]],
                date = get.date.from.string(vertices[["date"]])
            ),
            edges = edge.list
        ))
    } else {
        return(list(
            vertices = data.frame(
                name = unique(vertex.names)
            ),
            edges = edge.list
        ))
    }
}

#' Constructs edge list from the given key value list respecting temporal order.
#' Helper method which is called by 'construct.edge.list.by.key.value.list'.
#'
#' @param set the given key value list
#' @param network.conf the network configuration
#' @param edge.attributes the attributes that should be on the edges of the network
#' @param keys the keys of the key value list
#' @param keys.number the amount of keys in the key value list
#' @param network.type the type of network that should be created
#'
#' @return the data for the edge list
construct.edges.temporal.order = function(set, network.conf, edge.attributes, keys, keys.number, network.type) {
    number.edges = sum(seq_len(nrow(set)) - 1)
    logging::logdebug("[%s/%s] Constructing edges for %s '%s': starting (%s edges to construct).",
                      match(attr(set, "group.name"), keys), keys.number,
                      attr(set, "group.type"), attr(set, "group.name"), number.edges)

    ## Skip artifacts with many, many edges
    if (number.edges > network.conf$get.value("skip.threshold")) {
        logging::logwarn("Skipping edges for %s '%s' due to amount (> %s).",
                         attr(set, "group.type"), attr(set, "group.name"), network.conf$get.value("skip.threshold"))
        return(NULL)
    }

    if (network.type == "commit") {
        set = set[order(set[["date"]]), ]
    }

    ## queue of already processed artifacts
    edge.list.set = data.frame()
    vertices.processed.set = c()

    ## connect the current item to all previous ones
    for (item.no in seq_len(nrow(set))) {
        item = set[item.no, ]

        ## get vertex data
        item.vertex = item[["data.vertices"]]
        if (network.type == "commit") {
            item.vertex = data.frame(commit = item.vertex, date = get.date.string(item[["date"]]))
        }

        ## get edge attributes
        cols.which = edge.attributes %in% colnames(item)
        item.edge.attrs = item[ , edge.attributes[cols.which], drop = FALSE]

        ## construct edges
        combinations = c()
        if (network.type == "commit") {
            combinations = expand.grid(item.vertex[["commit"]],
                                       vertices.processed.set[["commit"]], stringsAsFactors = FALSE)
        } else {
            combinations = expand.grid(item.vertex, vertices.processed.set, stringsAsFactors = FALSE)
        }
        colnames(combinations)[colnames(combinations) == "Var1"] = "from"
        colnames(combinations)[colnames(combinations) == "Var2"] = "to"

        if (nrow(combinations) > 0 && nrow(item.edge.attrs) == 1) {
            combinations = cbind(combinations, item.edge.attrs, row.names = NULL) # add edge attributes
        }
        edge.list.set = rbind(edge.list.set, combinations) # add to edge list

        ## mark current item as processed
        if (network.type == "commit") {
            vertices.processed.set = rbind(vertices.processed.set, item.vertex)
        } else {
            vertices.processed.set = c(vertices.processed.set, item.vertex)
        }
    }

    ## store set of processed vertices
    if (network.type == "commit") {
        attr(edge.list.set, "vertices.processed") = vertices.processed.set[["commit"]]
        attr(edge.list.set, "vertices.dates.processed") = vertices.processed.set[["date"]]
    } else {
        attr(edge.list.set, "vertices.processed") = vertices.processed.set
    }

    logging::logdebug("Constructing edges for %s '%s': finished.", attr(set, "group.type"), attr(set, "group.name"))

    return(edge.list.set)
}

#' Constructs edge list from the given key value list not respecting temporal order.
#' Helper method which is called by 'construct.edge.list.by.key.value.list'.
#'
#' @param set the given key value list
#' @param network.conf the network configuration
#' @param edge.attributes the attributes that should be on the edges of the network
#' @param keys the keys of the key value list
#' @param keys.number the amount of keys in the key value list
#'
#' @return the data for the edge list
construct.edges.no.temporal.order = function(set, network.conf, edge.attributes, keys, keys.number) {
    number.edges = sum(table(set[["data.vertices"]]) * (dim(table(set[["data.vertices"]])) - 1))
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
    vertices = unique(set[["data.vertices"]])

    ## break if there is no author
    if (length(vertices) < 1) {
        return(NULL)
    }

    ## if there is only one author, just create the vertex, but no edges
    if (length(vertices) == 1) {
        edges = data.frame()
        attr(edges, "vertices.processed") = vertices # store set of processed vertices
        return(edges)
    }

    ## get combinations
    combinations = combn(vertices, 2) # all unique pairs of authors

    ## construct edge list
    edges = apply(combinations, 2, function(comb) {

        ## iterate over each of the two data vertices of the current combination to determine the edges
        ## for which it is the sender of the edge and use the second one as the receiver of the edge
        edges.by.comb.item = lapply(comb, function(comb.item) {
            ## basic edge data
            edge = data.frame(from = comb.item, to = comb[comb != comb.item])

            ## get edge attibutes
            edge.attrs = set[set[["data.vertices"]] %in% comb.item, ] # get data for current combination item
            cols.which = edge.attributes %in% colnames(edge.attrs)
            edge.attrs = edge.attrs[ , edge.attributes[cols.which], drop = FALSE]

            # add edge attributes to edge list
            edgelist = cbind(edge, edge.attrs)
            return(edgelist)
        })

        ## union the edge lists for the combination items
        edges.union = plyr::rbind.fill(edges.by.comb.item)
        return(edges.union)

    })
    edges = plyr::rbind.fill(edges)

    ## store set of processed vertices
    attr(edges, "vertices.processed") = vertices

    return(edges)
}

#' Construct a network from the given lists of vertices and edges.
#' For example for a list of authors per thread, where all authors are connected if they are
#' in the same thread (sublist).
#'
#' If \code{directed}, the direction of edges in the edge list is respected and the 'edge.attr's hold the
#' vector of possible edge attributes in the given list.
#'
#' @param vertices data frame with vertex data (can be \code{NULL})
#' @param edge.list list of edges
#' @param network.conf the network configuration
#' @param directed whether or not the network should be directed [default: FALSE]
#' @param possible.edge.attributes a named vector/list of attribute classes, with their corresponding names
#'                                 as names on the list [default: list()]
#'
#' @return the built network
construct.network.from.edge.list = function(vertices, edge.list, network.conf, directed = FALSE,
                                            possible.edge.attributes = list()) {
    logging::logdebug("construct.network.from.edge.list: starting.")
    logging::loginfo("Construct network from edges.")

    ## get unique list of vertices to produce
    vertices.processed = unique(vertices)

    ## if we do not have vertices, return rightaway
    if (is.null(vertices.processed) || length(vertices.processed) == 0) {
        vertices.processed = create.empty.vertex.list()
    }

    ## if we have vertices to create, but no edges, create an empty edge list
    if (is.null(edge.list) || ncol(edge.list) < 2) {
        edge.list = create.empty.edge.list()
    }

    ## construct network from edge list if there are vertices
    net = igraph::graph_from_data_frame(edge.list, directed = directed, vertices = vertices.processed)

    ## add missing vertex attributes if vertices.processed was empty (igraph::graph_from_data_frame does add them then)
    if (nrow(vertices.processed) == 0) {
        ## vertex attributes
        needed.vertex.attributes.types = list(name = "character")
        net = add.attributes.to.network(net, "vertex", needed.vertex.attributes.types)
    }

    ## add missing edge attributes if edge.list was empty (igraph::graph_from_data_frame does add them then)
    if (nrow(edge.list) == 0) {
        ## edge attributes
        configured.edge.attributes = network.conf$get.value("edge.attributes")
        required.edge.attributes = configured.edge.attributes[configured.edge.attributes %in%
                                                              names(possible.edge.attributes)]
        required.edge.attributes.types = possible.edge.attributes[required.edge.attributes]
        net = add.attributes.to.network(net, "edge", required.edge.attributes.types)
    }

    ## initialize edge weights
    net = igraph::set_edge_attr(net, "weight", value = 1)
    net = convert.edge.attributes.to.list(net)

    logging::logdebug("construct.network.from.edge.list: finished.")

    return(net)
}

#' Merges a list vertex data frame and merges a list of edge
#' data frames
#'
#' Note that identical vertices are merged, whereas identical edges are not.
#' This will lead to duplicated edges if you merge a network with itself.
#'
#' @param network.data the network-describing data that should be merged. Each element
#'                     should be a list with two elements: \code{vertices} and \code{edges},
#'                     that contain the corresponding data for one network.
#'
#' @return list containing one edge data frame (name \code{edges}) and
#'         one vertex data frame (named \code{vertices})
merge.network.data = function(network.data) {
    logging::logdebug("merge.network.data: starting.")

    ## extract vertex and edge data
    vertex.data = lapply(network.data, function(data) data[["vertices"]])
    edge.data   = lapply(network.data, function(data) data[["edges"]])

    if (length(network.data) == 1) {
        logging::logdebug("Network data of only one network given, so a merge is not necessary")
        return(list(
            vertices = vertex.data[[1]],
            edges = edge.data[[1]]
        ))
    }

    ## combine vertices and select only unique vertices
    vertices = plyr::rbind.fill(vertex.data)
    vertices = unique.data.frame(vertices)

    ## combine all edges via the edge lists:
    ## 1) remove empty instances of edge data
    edge.data.filtered = Filter(function(ed) {
        return(nrow(ed) > 0)
    }, edge.data)
    ## 2) add in missing columns
    all.columns = Reduce(union, lapply(edge.data.filtered, colnames))
    edge.data.filtered = lapply(edge.data.filtered, function(edges) {
        missing.columns = setdiff(all.columns, colnames(edges))
        edges[missing.columns] = lapply(missing.columns, function(column) {
            return(list(list(NA)))
        })
        return(edges)
    })
    ## 3) call rbind
    edges = plyr::rbind.fill(edge.data.filtered)
    ## 4) correct empty results
    if (is.null(edges)) {
        edges = create.empty.edge.list()
    }

    ## catch case where no vertices (and no vertex attributes) are given
    if (ncol(vertices) == 0) {
        vertices = NULL # igraph::graph_from_data_frame can handle this
    }

    logging::logdebug("merge.network.data: finished.")
    return(list(
        vertices = vertices,
        edges = edges
    ))
}

#' Merges a list of networks to one big network
#'
#' Note that identical vertices are merged, whereas identical edges are not.
#' This will lead to duplicated edges if you merge a network with itself.
#'
#' @param networks the list of networks
#'
#' @return the built one network
merge.networks = function(networks) {
    logging::logdebug("merge.networks: starting.")

    if (length(networks) == 1) {
        logging::logdebug("Only one network given, so a merge is not necessary.")
        return(networks[[1]])
    }

    ## construct network data
    network.data = lapply(networks, function(network) {
        return(list(
            vertices = igraph::as_data_frame(network, what = "vertices"),
            edges = igraph::as_data_frame(network, what = "edges")
        ))
    })

    ## merge all edge and vertex data frames
    new.network.data = merge.network.data(network.data)

    ## build whole network form edge and vertex data frame
    whole.network = igraph::graph_from_data_frame(
        new.network.data[["edges"]],
        vertices = new.network.data[["vertices"]],
        directed = igraph::is_directed(networks[[1]])
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
#' @param possible.edge.attributes a named vector/list of attribute classes, with their corresponding names
#'                                 as names on the list [default: list()]
#'
#' @return the adjusted network
add.edges.for.bipartite.relation = function(net, bipartite.relations, network.conf,
                                            possible.edge.attributes = list()) {

    ## iterate about all bipartite.relations depending on the relation type
    for (relation in names(bipartite.relations)) {
        ## get the data for the current relation
        net1.to.net2 = bipartite.relations[[relation]]

        ## construct edges (i.e., a vertex sequence with c(source, target, source, target, ...))
        vertex.sequence.for.edges = parallel::mcmapply(function(d, a.df) {
            ## get list of artifacts from edge list
            a = a.df[["data.vertices"]]

            ## remove empty artifact
            a = a[a != UNTRACKED.FILE.EMPTY.ARTIFACT]

            new.edges = lapply(a, function(vert) {
                igraph::V(net)[d, vert] # get two vertices from source network:  c(author, artifact)
            })
            return(new.edges)
        }, names(net1.to.net2), net1.to.net2, SIMPLIFY = FALSE)

        ## initialize edge attributes
        configured.edge.attributes = network.conf$get.value("edge.attributes")
        possible.edge.attributes = possible.edge.attributes[names(possible.edge.attributes)
                                                            %in% configured.edge.attributes]
        net = add.attributes.to.network(net, "edge", configured.edge.attributes)

        ## get extra edge attributes
        extra.edge.attributes.df = parallel::mcmapply(vertex.sequence = vertex.sequence.for.edges, a.df = net1.to.net2,
                                                      SIMPLIFY = FALSE, function(vertex.sequence, a.df) {

            ## return empty data.frame if vertex sequence is empty
            if (length(unlist(vertex.sequence)) == 0){
                return(data.frame())
            }

            ## get the artifacts from the vertex sequence (which are the even elements of the sequence vector)
            vertex.names.in.sequence = names(unlist(vertex.sequence))
            artifacts.in.sequence = vertex.names.in.sequence[seq(2, length(vertex.names.in.sequence), 2)]

            ## get the edges that will be constructed from the artifacts,
            ## to get only the edge attributes for edges that will be present in the final network
            ## (i.e., ignore edges to removed artifacts, such as the empty artifact that has been removed above)
            constructed.edges = a.df[a.df[["data.vertices"]] %in% artifacts.in.sequence, , drop = FALSE]

            ## return empty data.frame if there will be no edges in the end
            if (nrow(constructed.edges) < 1) {
                return(data.frame())
            }

            ## select the allowed attributes from the edge data.frame's columns
            cols.which = configured.edge.attributes %in% colnames(constructed.edges)
            return(constructed.edges[ , configured.edge.attributes[cols.which], drop = FALSE])
        })
        extra.edge.attributes.df = plyr::rbind.fill(extra.edge.attributes.df)
        extra.edge.attributes = as.list(extra.edge.attributes.df)
        extra.edge.attributes["weight"] = 1 # add weight
        extra.edge.attributes["type"] = TYPE.EDGES.INTER # add egde type
        extra.edge.attributes["relation"] = relation # add relation type

        ## convert the edge attributes to list format
        edge.attributes = convert.edge.list.attributes.to.list(extra.edge.attributes)

        ## add the vertex sequences as edges to the network
        net = igraph::add_edges(net, unlist(vertex.sequence.for.edges), attr = edge.attributes)

        ## replace NULLs in edge attributes with NAs for consistency
        net = Reduce(function(net, attr) {
            values = igraph::edge_attr(net, attr)
            NULLs = sapply(values, is.null)
            if (any(NULLs)) {
                values[NULLs] = NA
                net = igraph::set_edge_attr(net, attr, value = values)
            }
            return(net)
        }, igraph::edge_attr_names(net), net)
    }

    return(net)
}

#' Create an empty network that does not break the algorithms.
#'
#' @param directed flag whether or not the network should be directed [default: TRUE]
#' @param add.attributes flag whether to add the mandatory vertex and edge attributes [default: FALSE]
#'
#' @return the new empty network
create.empty.network = function(directed = TRUE, add.attributes = FALSE) {
    ## create empty network
    net = igraph::make_empty_graph(0, directed = directed)

    # set proper attributes if wanted
    if (add.attributes) {
        mandatory.edge.attributes.classes = list(
            date = "list", artifact.type = "list", weight = "numeric",
            type = "character", relation = "list"
        )
        mandatory.vertex.attributes.classes = list(name = "character", kind = "character", type = "character")

        net = add.attributes.to.network(net, "vertex", mandatory.vertex.attributes.classes)
        net = add.attributes.to.network(net, "edge", mandatory.edge.attributes.classes)
    }

    return(net)
}

#' Create an empty data frame that does not break the algorithms and
#' represents a vertex list.
#'
#' @return a single-column data.frame (column \code{name})
create.empty.vertex.list = function() {
    return(data.frame(name = character(0)))
}

#' Create an empty data frame that does not break the algorithms and
#' represents an edge list.
#'
#' @return a two-column data.frame (columns \code{from} and \code{to})
create.empty.edge.list = function() {
    return(data.frame(from = character(0), to = character(0)))
}

#' Add the given list of \code{type} attributes to the given network.
#'
#' All added attributes are set to the default value of the respective class.
#'
#' @param network the network to which the attributes are to be added
#' @param type the type of attribute to add; either \code{"vertex"} or \code{"edge"}
#'             for vertex or edge attributes, respectively [default: "vertex"]
#' @param attributes a named list/vector of data classes for the wanted attributes,
#'                   with the attribute names as names on the list [default: list()]
#'
#' @return the given network with the attributes added
add.attributes.to.network = function(network, type = c("vertex", "edge"), attributes = list()) {

    ## get type
    type = match.arg(type, several.ok = FALSE)

    ## get corresponding attribute functions
    if (type == "vertex") {
        attribute.set.function = igraph::set_vertex_attr # sprintf("igraph::set.%s.attribute", type)
        attribute.get.function = igraph::vertex_attr # sprintf("igraph::get.%s.attribute", type)
        attribute.remove.function = igraph::delete_vertex_attr # sprintf("igraph::remove.%s.attribute", type)
    } else {
        attribute.set.function = igraph::set_edge_attr # sprintf("igraph::set.%s.attribute", type)
        attribute.get.function = igraph::edge_attr # sprintf("igraph::get.%s.attribute", type)
        attribute.remove.function = igraph::delete_edge_attr # sprintf("igraph::remove.%s.attribute", type)
    }

    ## iterate over all wanted attribute names and add the attribute with the wanted class
    for (attr.name in names(attributes)) {
        ## set and re-new the default value so that everything works
        default.value = 0
        ## set the right class for the default value
        class(default.value) = attributes[[attr.name]]

        ## make sure that the default value contains a tzone attribute if the attribute is of class 'POSIXct'
        if (lubridate::is.POSIXct(default.value)) {
            attr(default.value, "tzone") = TIMEZONE
        }

        ## check if the attribute is already present. If so, remove it and re-add it (to keep the intended order).
        ## only exception from this: the name attribute is not removed and re-added, as this would lead to problems.
        if (!is.null(attribute.get.function(network, attr.name)) && attr.name != "name") {
            logging::logwarn("Attribute %s has already been present, but is re-added now.", attr.name)
            present.value = attribute.get.function(network, attr.name)
            network = attribute.remove.function(network, attr.name)
            default.value = present.value
        }

        ## add the new attribute to the network with the proper class
        network = attribute.set.function(network, attr.name, value = default.value)
    }

    return(network)
}


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Network simplification --------------------------------------------------

#' Simplify a given network.
#'
#' This function retains all set network, vertex, and edge attributes.
#'
#' @param network the given network
#' @param remove.multiple whether to contract multiple edges between the same pair of vertices [default: TRUE]
#' @param remove.loops whether to remove loops [default: TRUE]
#' @param simplify.multiple.relations whether to combine edges of multiple relations into
#'        one simplified edge [default: FALSE]
#'
#' @return the simplified network
simplify.network = function(network, remove.multiple = TRUE, remove.loops = TRUE,
                            simplify.multiple.relations = FALSE) {
    logging::logdebug("simplify.network: starting.")
    logging::loginfo("Simplifying network.")

    ## save network attributes, otherwise they get lost
    network.attributes = igraph::graph_attr(network)

    if (!simplify.multiple.relations && length(unique(unlist(igraph::edge_attr(network, "relation")))) > 1) {
        ## data frame of the network
        edge.data = igraph::as_data_frame(network, what = "edges")
        vertex.data = igraph::as_data_frame(network, what = "vertices")

        ## helper function to check if two edges can be simplified
        relations.match = function(relation.A, relation.B) {
            diff = setdiff(sort(unique(unlist(relation.A))), sort(unique(unlist(relation.B))))
            return(length(diff) == 0)
        }

        ## group all edges that can be simplified together
        edges.by.relation = list()
        for (i in seq_len(nrow(edge.data))) {

            ## get relation of current edge
            edge.relation = edge.data[i, ][["relation"]]
            match = NULL

            ## test if current edge can be simplified with any already seen edge
            for (group in seq(edges.by.relation)) {
                group.edge.index = edges.by.relation[[group]][1]
                group.relation = edge.data[group.edge.index, ][["relation"]]
                if (relations.match(edge.relation, group.relation)) {
                    match = group
                    break
                }
            }

            ## add edge to existing group or create a new group
            if (!is.null(match)) {
                edges.by.relation[[match]] = c(edges.by.relation[[match]], i)
            } else {
                edges.by.relation[[length(edges.by.relation) + 1]] = i
            }
        }

        ## select edges of one relation, build the network and simplify this network
        networks = lapply(seq(edges.by.relation),
                          function(group) {
                              network.data = edge.data[edges.by.relation[[group]], ]
                              net = igraph::graph_from_data_frame(d = network.data,
                                                                  vertices = vertex.data,
                                                                  directed = igraph::is_directed(network))

                              ## simplify networks (contract edges and remove loops)
                              net = igraph::simplify(net, edge.attr.comb = EDGE.ATTR.HANDLING,
                                                     remove.multiple = remove.multiple,
                                                     remove.loops = remove.loops)
                              ## TODO perform simplification on edge list?
                              net = convert.edge.attributes.to.list(net)
                              return(net)
        })

        ## merge the simplified networks
        network = merge.networks(networks)
    } else {
        network = igraph::simplify(network, edge.attr.comb = EDGE.ATTR.HANDLING,
                                   remove.multiple = remove.multiple, remove.loops = remove.loops)
        network = convert.edge.attributes.to.list(network)
    }

    ## re-apply all network attributes
    for (att in names(network.attributes)) {
        network = igraph::set_graph_attr(network, att, network.attributes[[att]])
    }

    logging::logdebug("simplify.network: finished.")
    return(network)
}

#' Simplify a list of networks.
#'
#' @param networks the list of networks
#' @param remove.multiple whether to contract multiple edges between the same pair of vertices [default: TRUE]
#' @param remove.loops whether to remove loops [default: TRUE]
#' @param simplify.multiple.relations whether to combine edges of multiple relations into
#'                                    one simplified edge [default: FALSE]
#'
#' @return the simplified networks
simplify.networks = function(networks, remove.multiple = TRUE, remove.loops = TRUE,
                             simplify.multiple.relations = FALSE) {
    logging::logdebug("simplify.networks: starting.")
    logging::loginfo(
        "Simplifying networks (names = [%s]).",
        paste(names(networks), collapse = ", ")
    )

    nets = parallel::mclapply(networks, simplify.network, remove.multiple = remove.multiple,
                              remove.loops = remove.loops, simplify.multiple.relations = simplify.multiple.relations)

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
    network.no.isolates = igraph::delete_vertices(
        network,
        igraph::degree(network, mode = "all") == 0
    )
    return(network.no.isolates)
}

#' Remove duplicate edges from the given network.
#'
#' Keep exactly one edge from all equivalence classes of edges over identity.
#' This function retains all set network, vertex, and edge attributes.
#'
#' @param network the given network
#'
#' @return the simplified network
remove.duplicate.edges = function(network) {

    logging::logdebug("remove.duplicate.edges: starting.")

    ## remove all duplicates
    edges = igraph::as_data_frame(network, "edges")
    network = igraph::delete_edges(network, which(duplicated(edges)))

    logging::logdebug("remove.duplicate.edges: finished.")
    return(network)
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
    author.network = igraph::induced_subgraph(network, igraph::V(network)[type == TYPE.AUTHOR])
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
    artifact.network = igraph::induced_subgraph(network, igraph::V(network)[type == TYPE.ARTIFACT])
    ## remove isolates if wanted
    if (remove.isolates) {
        artifact.network = delete.isolates(artifact.network)
    }
    return(artifact.network)
}

#' Extract the bipartite-network part from the given (multi) network, i.e.,
#' return the subgraph induced by the edges of type TYPE.EDGES.INTER.
#'
#' **Note**: This function throws an error when the edge attribute \code{type} is missing.
#'
#' @param network the (multi) network to reduce
#' @param remove.isolates whether to remove isolated vertices during extraction [default: FALSE]
#'
#' @return the bipartite-edge-induced subgraph of \code{network}
extract.bipartite.network.from.network = function(network, remove.isolates = FALSE) {

    ## check whether there are vertices in the network, otherwise skip the extraction
    if (igraph::vcount(network) == 0) {
        return(network)
    }

    ## check whether there is an edge attibute 'type'
    if (!("type" %in% igraph::edge_attr_names(network))) {
        logging::logerror("Extraction of an bipartite network without the edge attribute 'type' does not work!")
        stop("Failed extraction of bipartite network.")
    }

    ## only retain all bipartite edges and induced vertices
    bip.network = igraph::subgraph_from_edges(network,
                                              igraph::E(network)[type == TYPE.EDGES.INTER],
                                              delete.vertices = remove.isolates)

    return(bip.network)
}

#' Delete author vertices from the given network that do not have adjacent edges of a specific type,
#' i.e., bipartite and/or unipartite edges.
#'
#' *Use case for \code{specific.edge.types = "TYPE.EDGES.INTER"}*:
#' When building multi-networks, we can make use of the configuration option \code{author.only.committers}.
#' However, when we split a project-level multi-network into range-level networks, we can obtain authors
#' in a certain time-range, that are only active on the mailing list in this range, but appear in the
#' range-level multi-network as they appear in the project-level network since they are committer in,
#' at least, one range. This function is able to remove the then-isolated author vertices from the
#' range-level networks.
#'
#' @param network the network from which the author vertices are to be removed
#' @param specific.edge.types the type of edges an author vertex needs to have to retain in the network;
#'                            one or more of \code{TYPE.EDGES.INTER} and \code{TYPE.EDGES.INTRA}
#'                            [default: c(TYPE.EDGES.INTER, TYPE.EDGES.INTRA)]
#'
#' @return the network without author vertices lacking edges of the specified
delete.authors.without.specific.edges = function(network, specific.edge.types =
                                                     c(TYPE.EDGES.INTER, TYPE.EDGES.INTRA)) {

    ## obtain the edge type to consider
    specific.edge.types = match.arg.or.default(specific.edge.types, several.ok = TRUE)

    ## get all authors
    vertex.ids.author = as.numeric(igraph::V(network)[type == TYPE.AUTHOR])
    ## get vertex IDs of all vertices with specific edges
    vertex.ids.specific = as.vector(igraph::get.edges(network, igraph::E(network)[type %in% specific.edge.types]))

    ## compute all authors without specific edges as vertex IDs
    vertex.ids.author.no.specific = setdiff(vertex.ids.author, vertex.ids.specific)
    ## remove all authors without specific edges from network
    network = igraph::delete_vertices(network, vertex.ids.author.no.specific)

    return(network)
}


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Helper functions---------------------------------------------------------

#' Calculate the data sources of a network based on its edge relations.
#'
#' @param network the network with the relations to be extracted
#'
#' @return a vector with all data.sources from the network; an element is set to \code{NA} if the network contains an
#'         empty relation, i.e. \code{character(0)}
get.data.sources.from.relations = function(network) {
    ## get all relations in the network
    data.sources = unique(unlist(igraph::E(network)$relation))

    ## map them to data sources respectively using the defined translation constant
    data.sources = sapply(data.sources, function(relation) {
        ## check for a \code{character(0)} relation and abort if there is one
        if (length(relation) == 0) {
            logging::logwarn("There seems to be an empty relation in the network. Cannot proceed.")
            return(NA)
        }

        ## use the translation constant to get the appropriate data source
        return(RELATION.TO.DATASOURCE[[relation]])
    }, USE.NAMES = FALSE) # avoid element names as we only want the data source's name

    return(data.sources)
}

#' Convert edge attributes to list type.
#'
#' This conversion is necessary to ensure merging networks works in all cases,
#' especially when merging simplified networks with unsimplified networks as
#' simplification may convert edge attributes to list type. Attributes that are
#' explicitly considered during simplification (through EDGE.ATTR.HANDLING)
#' generally do not need to be converted.
#'
#' @param network the network of which the edge attributes are to be converted
#' @param remain.as.is the edge attributes to remain as they are
#'                     [default: names(EDGE.ATTR.HANDLING)]
#'
#' @return the network with converted edge attributes
convert.edge.attributes.to.list = function(network, remain.as.is = names(EDGE.ATTR.HANDLING)) {

    ## get edge attributes
    edge.attrs = igraph::edge_attr_names(network)
    which.attrs = !(edge.attrs %in% remain.as.is)

    ## convert edge attributes to list type
    for (attr in edge.attrs[which.attrs]) {
        list.attr = as.list(igraph::edge_attr(network, attr))

        ## convert individual values to list
        listed.values = sapply(list.attr, is.list)
        if (!all(listed.values)) {
            list.attr[!listed.values] = lapply(list.attr[!listed.values], as.list)
        }

        ## replace attribute
        network = igraph::set_edge_attr(network, attr, value = list.attr)
    }

    return(network)
}

#' Convert attributes in edge list to list type.
#'
#' @param edge.list the edge list either as a data frame or as a list of which
#'                  the attributes are to be converted
#' @param remain.as.is the attributes to remain as they are
#'                  [default: names(EDGE.ATTR.HANDLING)]
#'
#' @return the edge list with converted attributes as a data frame
#'
#' @seealso convert.edge.attributes.to.list
convert.edge.list.attributes.to.list = function(edge.list, remain.as.is = names(EDGE.ATTR.HANDLING)) {

    ## the 'from' and to 'to' columns must always remain as they are
    remain.as.is = c(remain.as.is, "from", "to")

    ## if edge list is in list format, convert to data frame
    if (is.list(edge.list)) {
        edge.list = as.data.frame(edge.list, stringsAsFactors = FALSE)
    }

    ## get edge attributes
    edge.attrs = colnames(edge.list)
    which.attrs = !(edge.attrs %in% remain.as.is)

    ## convert edge attributes to list type
    for (attr in edge.attrs[which.attrs]) {
        list.attr = as.list(edge.list[[attr]])

        ## convert individual values to list
        listed.values = sapply(list.attr, is.list)
        if (!all(listed.values)) {
            list.attr[!listed.values] = lapply(list.attr[!listed.values], as.list)
        }

        ## replace attribute
        edge.list[[attr]] = list.attr
    }

    return(edge.list)

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
    proj.conf$update.values(list(commits.filter.base.artifact = FALSE))

    ## RangeData object
    range = proj.conf$get.value("ranges")[1]
    range.callgraph = proj.conf$get.callgraph.revision.from.range(range)
    proj.data = RangeData$new(proj.conf, range, range.callgraph, built.from.range.data.read = TRUE)

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
    network = igraph::set_graph_attr(network, "sample.network", TRUE)

    ## set layout for plotting
    lay = matrix(c(  20, 179, 693, 552, 956, 1091, 124, 317, 516, 615, 803, 1038,
                    245, 175, 255, 185, 253, 225,   73,   8,  75,   0,  96,   86),
                 nrow = 12, byrow = FALSE) # for sample graph
    network = igraph::set_graph_attr(network, "layout", lay)

    return(network)
}
