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
## Copyright 2017-2019 by Thomas Bock <bockthom@fim.uni-passau.de>
## Copyright 2021, 2023-2024 by Thomas Bock <bockthom@cs.uni-saarland.de>
## Copyright 2018 by Barbara Eckl <ecklbarb@fim.uni-passau.de>
## Copyright 2018-2019 by Jakob Kronawitter <kronawij@fim.uni-passau.de>
## Copyright 2020 by Anselm Fehnker <anselm@muenster.de>
## Copyright 2021 by Niklas Schneider <s8nlschn@stud.uni-saarland.de>
## Copyright 2022 by Jonathan Baumann <joba00002@stud.uni-saarland.de>
## Copyright 2023-2024 by Maximilian Löffler <s8maloef@stud.uni-saarland.de>
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
    relation = function(relation) sort(unique(relation)),

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
## Constants ---------------------------------------------------------------

## mapping of relation to data source
RELATION.TO.DATASOURCE = list(
    "cochange"  = "commits",
    "callgraph" = "commits",
    "mail"      = "mails",
    "issue"     = "issues"
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

        ## * * network caching ---------------------------------------------

        authors.network.mail = NULL, # igraph
        authors.network.cochange = NULL, # igraph
        authors.network.issue = NULL, #igraph
        artifacts.network.cochange = NULL, # igraph
        artifacts.network.callgraph = NULL, # igraph
        artifacts.network.mail = NULL, # igraph
        artifacts.network.issue = NULL, # igraph

        ## * * relation-to-vertex-kind mapping -----------------------------

        #' Determine which vertex kind should be chosen for the vertex depending on the relation
        #' between the vertices.
        #'
        #' @param relation the given relation
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
                directed = private$network.conf$get.value("author.directed"),
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
            ## 4) set author list as vertices
            author.net.data[["vertices"]] = authors

            ## construct network from obtained data
            author.net = construct.network.from.edge.list(
                author.net.data[["vertices"]],
                author.net.data[["edges"]],
                network.conf = private$network.conf,
                directed = private$network.conf$get.value("author.directed"),
                available.edge.attributes = private$proj.data$get.data.columns.for.data.source("commits")
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
                private$proj.data$group.authors.by.data.column("mails", "thread"),
                network.conf = private$network.conf,
                directed = private$network.conf$get.value("author.directed"),
                respect.temporal.order = private$network.conf$get.value("author.respect.temporal.order")
            )

            ## construct network from obtained data
            author.net = construct.network.from.edge.list(
                author.net.data[["vertices"]],
                author.net.data[["edges"]],
                network.conf = private$network.conf,
                directed = private$network.conf$get.value("author.directed"),
                available.edge.attributes = private$proj.data$get.data.columns.for.data.source("mails")
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
                private$proj.data$group.authors.by.data.column("issues", "issue.id"),
                network.conf = private$network.conf,
                directed = private$network.conf$get.value("author.directed"),
                respect.temporal.order = private$network.conf$get.value("author.respect.temporal.order")
            )

            ## construct network from obtained data
            author.net = construct.network.from.edge.list(
                author.net.data[["vertices"]],
                author.net.data[["edges"]],
                network.conf = private$network.conf,
                directed = private$network.conf$get.value("author.directed"),
                available.edge.attributes = private$proj.data$get.data.columns.for.data.source("issues")
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

            ## construct edge list based on commit--artifact data
            artifacts.net.data.raw = private$proj.data$group.artifacts.by.data.column("commits", "hash")

            artifacts.net.data = construct.edge.list.from.key.value.list(
                artifacts.net.data.raw,
                network.conf = private$network.conf,
                directed = FALSE,
                respect.temporal.order = TRUE,
                artifact.edges = TRUE
            )

            ## construct network from obtained data
            artifacts.net = construct.network.from.edge.list(
                artifacts.net.data[["vertices"]],
                artifacts.net.data[["edges"]],
                network.conf = private$network.conf,
                directed = FALSE,
                available.edge.attributes = private$proj.data$get.data.columns.for.data.source("commits")
            )

            ## remove the artifact vertices stemming from untracked files if existing
            if ("name" %in% igraph::list.vertex.attributes(artifacts.net) &&
                length(igraph::V(artifacts.net)[name == UNTRACKED.FILE.EMPTY.ARTIFACT]) > 0) {

                artifacts.net = igraph::delete.vertices(artifacts.net, UNTRACKED.FILE.EMPTY.ARTIFACT)
            }

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

            ## construct edgeless network with mandatory edge and vertex attributes
            directed = private$network.conf$get.value("artifact.directed")
            artifacts = private$proj.data$get.artifacts("mails") # thread IDs
            artifacts.net = create.empty.network(directed = directed, add.attributes = TRUE) +
                igraph::vertices(artifacts)

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
            if (!private$network.conf$get.entry("artifact.directed")) {

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

            vertices = unique(artifacts.net.data.raw["issue.id"])
            edge.list = data.frame()

            # edges in artifact networks can not have the 'artifact' attribute but should instead have
            # the 'author.name' attribute as events caused by authors connect issues
            edge.attributes = private$network.conf$get.value("edge.attributes")
            artifact.index = match("artifact", edge.attributes, nomatch = NA)
            if (!is.na(artifact.index)) {
                edge.attributes = edge.attributes[-artifact.index]
                edge.attributes = c(edge.attributes, c("author.name"))
            }

            ## connect corresponding add_link and referenced_by issue-events
            edge.list = plyr::rbind.fill(parallel::mclapply(split(add.links, seq_along(add.links)), function(from) {
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

            artifacts.net.data = list(
                vertices = data.frame(
                    name = vertices
                ),
                edges = edge.list
            )

            ## construct network from obtained data
            artifacts.net = construct.network.from.edge.list(
                artifacts.net.data[["vertices"]],
                artifacts.net.data[["edges"]],
                network.conf = private$network.conf,
                directed = private$network.conf$get.value("artifact.directed"),
                available.edge.attributes = private$proj.data$get.data.columns.for.data.source("issues")
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
        #' @return a named list of data for the bipartite relations, each with the attributes
        #'         'vertex.kind' denoting the artifact type for the relations
        #'         and 'relation' denoting the respective network-configuration entry; the names
        #'         are the relations configured by the attribute 'artifact.relation' of the
        #'         network configuration
        get.bipartite.relations = function() {
            logging::logdebug("get.bipartite.relations: starting.")

            relations = private$network.conf$get.variable("artifact.relation")
            logging::logdebug("Using bipartite relations '%s'.", relations)

            bip.relations = lapply(relations, function(relation) {
                ## get data for current bipartite relation
                data.source = RELATION.TO.DATASOURCE[[relation]]
                bip.relation = private$proj.data$group.artifacts.by.data.column(data.source, "author.name")

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
            private$authors.network.cochange = NULL
            private$authors.network.issue = NULL
            private$authors.network.mail = NULL
            private$artifacts.network.callgraph = NULL
            private$artifacts.network.cochange = NULL
            private$artifacts.network.issue = NULL
            private$artifacts.network.mail = NULL
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
                vertex.kind = private$get.vertex.kind.for.relation(relation)
                network = igraph::set.vertex.attribute(network, "kind", value = vertex.kind)

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
            vertex.data = merge.network.data(vertex.data = vertex.data, edge.data = NULL)[["vertices"]]
            ## 2) remove empty artifact, if names are available
            if ("name" %in% colnames(vertex.data)) {
                vertex.data = subset(vertex.data, !(name == UNTRACKED.FILE.EMPTY.ARTIFACT & type == TYPE.ARTIFACT))
            }
            ## 3) obtain all possible data columns, i.e., edge attributes
            available.edge.attributes = lapply(
                private$network.conf$get.variable("artifact.relation"),
                function(relation) {
                    data.source = RELATION.TO.DATASOURCE[[relation]]
                    data.cols = private$proj.data$get.data.columns.for.data.source(data.source)
                    return(data.cols)
                }
            )
            available.edge.attributes = unlist(available.edge.attributes, recursive = FALSE)
            available.edge.attributes = available.edge.attributes[
                !duplicated(names(available.edge.attributes)) # remove duplicates based on names
                ]
            ## 4) construct network without edges
            vertex.network = construct.network.from.edge.list(
                vertices = vertex.data,
                edge.list = create.empty.edge.list(),
                network.conf = private$network.conf,
                directed = directed,
                available.edge.attributes = available.edge.attributes
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
                available.edge.attributes = available.edge.attributes
            )

            ## remove vertices that are not committers if wanted
            if (private$network.conf$get.value("author.only.committers")) {
                committers = unique(private$proj.data$get.commits.unfiltered()[["author.name"]])
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

            ## Adjust vertex attribute to 'Issue' in multi networks
            ## to be consistent with bipartite networks
            artifacts.to.add.kind[artifacts.to.add.kind == "IssueEvent"] = "Issue"

            if (length(artifacts.to.add) > 0) {
                artifacts.net = artifacts.net + igraph::vertices(artifacts.to.add, type = TYPE.ARTIFACT,
                                                                 kind = artifacts.to.add.kind)
            }

            ## check directedness and adapt artifact network if needed
            if (igraph::is.directed(authors.net) && !igraph::is.directed(artifacts.net)) {
                logging::logwarn(paste0("Author network is directed, but artifact network is not.",
                                        "Converting artifact network..."))
                artifacts.net = igraph::as.directed(artifacts.net, mode = "mutual")
            } else if (!igraph::is.directed(authors.net) && igraph::is.directed(artifacts.net)) {
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

            ## As there is a bug in 'igraph::disjoint_union' in igraph versions 1.4.0, 1.4.1, and 1.4.2
            ## (see https://github.com/igraph/rigraph/issues/761), we need to adjust the type of the date attribute
            ## of the outcome of 'igraph::disjoint_union'.
            ## Note: The following temporary fix only considers the 'date' attribute. However, this problem could also
            ## affect several other attributes, whose classes are not adjusted in our temporary fix.
            ## The following code block should be redundant as soon as igraph has fixed their bug.
            u.actual.edge.attribute.date = igraph::get.edge.attribute(u, "date")
            if (!is.null(u.actual.edge.attribute.date)) {
                if (is.list(u.actual.edge.attribute.date)) {
                    u.expected.edge.attribute.date = lapply(u.actual.edge.attribute.date, get.date.from.unix.timestamp)
                } else {
                    u.expected.edge.attribute.date = get.date.from.unix.timestamp(u.actual.edge.attribute.date)
                }
                u = igraph::set.edge.attribute(u, "date", value = u.expected.edge.attribute.date)
            }

            ## 2) add the bipartite edges
            u = add.edges.for.bipartite.relation(u, authors.to.artifacts, private$network.conf)

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
#' @param artifact.edges whether the key value data represents edges in an artifact network based
#'                       on the cochange relation
#'                       [default: FALSE]
#'
#' @return a list of two data.frames named 'vertices' and 'edges' (compatible with return value
#'         of \code{igraph::as.data.frame})
construct.edge.list.from.key.value.list = function(list, network.conf, directed = FALSE,
                                                   respect.temporal.order = directed, artifact.edges = FALSE) {
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
    if (artifact.edges) {
        artifact.index = match("artifact", edge.attributes, nomatch = NA)
        if (!is.na(artifact.index)) {
            edge.attributes = edge.attributes[-artifact.index]
            edge.attributes = c(edge.attributes, c("author.name"))
        }
    }

    if (respect.temporal.order) {

        ## for all subsets (sets), connect all items in there with the previous ones
        edge.list.data = parallel::mclapply(list, function(set) {
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

            ## queue of already processed artifacts
            edge.list.set = data.frame()
            vertices.processed.set = c()

            ## connect the current item to all previous ones
            for (item.no in seq_len(nrow(set))) {
                item = set[item.no, ]

                ## get vertex data
                item.vertex = item[["data.vertices"]]

                ## get edge attributes
                cols.which = edge.attributes %in% colnames(item)
                item.edge.attrs = item[ , edge.attributes[cols.which], drop = FALSE]

                ## construct edges
                combinations = expand.grid(item.vertex, vertices.processed.set, stringsAsFactors = FALSE)
                if (nrow(combinations) > 0 & nrow(item.edge.attrs) == 1) {
                    combinations = cbind(combinations, item.edge.attrs, row.names = NULL) # add edge attributes
                }
                edge.list.set = rbind(edge.list.set, combinations) # add to edge list

                ## mark current item as processed
                vertices.processed.set = c(vertices.processed.set, item.vertex)
            }

            ## store set of processed vertices
            attr(edge.list.set, "vertices.processed") = vertices.processed.set

            logging::logdebug("Constructing edges for %s '%s': finished.", attr(set, "group.type"), attr(set, "group.name"))

            return(edge.list.set)
        })

        edge.list = plyr::rbind.fill(edge.list.data)
        vertices.processed = unlist( parallel::mclapply(edge.list.data, function(data) attr(data, "vertices.processed")) )

    } else {

        ## for all items in the sublists, construct the cartesian product
        edge.list.data = parallel::mclapply(list, function(set) {
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
        })

        edge.list = plyr::rbind.fill(edge.list.data)
        vertices.processed = unlist( parallel::mclapply(edge.list.data, function(data) attr(data, "vertices.processed")) )

    }

    logging::logdebug("construct.edge.list.from.key.value.list: finished.")

    return(list(
        vertices = data.frame(
            name = unique(vertices.processed)
        ),
        edges = edge.list
    ))
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
#' @param available.edge.attributes a named vector/list of attribute classes, with their corresponding names
#'                                  as names on the list [default: list()]
#'
#' @return the built network
construct.network.from.edge.list = function(vertices, edge.list, network.conf, directed = FALSE,
                                            available.edge.attributes = list()) {
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
    net = igraph::graph.data.frame(edge.list, directed = directed, vertices = vertices.processed)

    ## add missing vertex attributes if vertices.processed was empty (igraph::graph.data.frame does add them then)
    if (nrow(vertices.processed) == 0) {
        ## vertex attributes
        needed.vertex.attributes.types = list(name = "character")
        net = add.attributes.to.network(net, "vertex", needed.vertex.attributes.types)
    }

    ## add missing edge attributes if edge.list was empty (igraph::graph.data.frame does add them then)
    if (nrow(edge.list) == 0) {
        ## edge attributes
        allowed.attributes = network.conf$get.value("edge.attributes")
        needed.edge.attributes = allowed.attributes[allowed.attributes %in% names(available.edge.attributes)]
        needed.edge.attributes.types = available.edge.attributes[needed.edge.attributes]
        net = add.attributes.to.network(net, "edge", needed.edge.attributes.types)
    }

    ## initialize edge weights
    net = igraph::set.edge.attribute(net, "weight", value = 1)

    ## transform multiple edges to edge weights
    if (network.conf$get.value("simplify")) {
        net = simplify.network(net,
                               simplify.multiple.relations = network.conf$get.value("simplify.multiple.relations"))
    }

    logging::logdebug("construct.network.from.edge.list: finished.")

    return(net)
}

#' Merges a list vertex data frame and merges a list of edge
#' data frames
#'
#' Note that identical vertices are merged, whereas identical edges are not.
#' This will lead to duplicated edges if you merge a network with itself.
#'
#' @param vertex.data the list of vertex data frames, may be \code{NULL}
#' @param edge.data the list of edge data frames, may be \code{NULL}
#'
#' @return list containing one edge data frame (name \code{edges}) and
#'         one vertex data frame (named \code{vertices})
merge.network.data = function(vertex.data, edge.data) {
    logging::logdebug("merge.network.data: starting.")

    ## combine vertices and select only unique vertices
    vertices = plyr::rbind.fill(vertex.data)
    vertices = unique.data.frame(vertices)

    ## combine all edges via the edge lists:
    ## 1) remove empty instances of edge data
    edge.data.filtered = Filter(function(ed) {
        return(nrow(ed) > 0)
    }, edge.data)
    ## 2) call rbind
    edges = plyr::rbind.fill(edge.data.filtered)
    ## 3) correct empty results
    if (is.null(edges)) {
        edges = create.empty.edge.list()
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

    ## catch case where no vertices (and no vertex attributes) are given
    if (ncol(new.network.data[["vertices"]]) == 0) {
        new.network.data[["vertices"]] = NULL # igraph::graph.data.frame can handle this
    }

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
#' @param available.edge.attributes a named vector/list of attribute classes, with their corresponding names
#'                                  as names on the list [default: list()]
#'
#' @return the adjusted network
add.edges.for.bipartite.relation = function(net, bipartite.relations, network.conf,
                                            available.edge.attributes = list()) {

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
        allowed.edge.attributes = network.conf$get.value("edge.attributes")
        available.edge.attributes = available.edge.attributes[names(available.edge.attributes)
                                                              %in% allowed.edge.attributes]
        net = add.attributes.to.network(net, "edge", allowed.edge.attributes)

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
            cols.which = allowed.edge.attributes %in% colnames(constructed.edges)
            return(constructed.edges[ , allowed.edge.attributes[cols.which], drop = FALSE])
        })
        extra.edge.attributes.df = plyr::rbind.fill(extra.edge.attributes.df)
        extra.edge.attributes = as.list(extra.edge.attributes.df)
        extra.edge.attributes["weight"] = 1 # add weight
        extra.edge.attributes["type"] = TYPE.EDGES.INTER # add egde type
        extra.edge.attributes["relation"] = relation # add relation type

        ## add the vertex sequences as edges to the network
        net = igraph::add_edges(net, unlist(vertex.sequence.for.edges), attr = extra.edge.attributes)
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
    net = igraph::graph.empty(0, directed = directed)

    # set proper attributes if wanted
    if (add.attributes) {
        mandatory.edge.attributes.classes = list(
            date = c("POSIXct", "POSIXt"), artifact.type = "character", weight = "numeric",
            type = "character", relation = "character"
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
        attribute.set.function = igraph::set.vertex.attribute # sprintf("igraph::set.%s.attribute", type)
        attribute.get.function = igraph::get.vertex.attribute # sprintf("igraph::get.%s.attribute", type)
        attribute.remove.function = igraph::remove.vertex.attribute # sprintf("igraph::remove.%s.attribute", type)
    } else {
        attribute.set.function = igraph::set.edge.attribute # sprintf("igraph::set.%s.attribute", type)
        attribute.get.function = igraph::get.edge.attribute # sprintf("igraph::get.%s.attribute", type)
        attribute.remove.function = igraph::remove.edge.attribute # sprintf("igraph::remove.%s.attribute", type)
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
    network.attributes = igraph::get.graph.attribute(network)

    if (!simplify.multiple.relations && length(unique(igraph::get.edge.attribute(network, "relation"))) > 1) {
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

    ## re-apply all network attributes
    for (att in names(network.attributes)) {
        network = igraph::set.graph.attribute(network, att, network.attributes[[att]])
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
    if (!("type" %in% igraph::list.edge.attributes(network))) {
        logging::logerror("Extraction of an bipartite network without the edge attribute 'type' does not work!")
        stop("Failed extraction of bipartite network.")
    }

    ## only retain all bipartite edges and induced vertices
    bip.network = igraph::subgraph.edges(network, igraph::E(network)[type == TYPE.EDGES.INTER], delete.vertices = remove.isolates)

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
    network = igraph::delete.vertices(network, vertex.ids.author.no.specific)

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
            return (NA)
        }

        ## use the translation constant to get the appropriate data source
        return(RELATION.TO.DATASOURCE[[relation]])
    }, USE.NAMES = FALSE) # avoid element names as we only want the data source's name

    return(data.sources)
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
    network = igraph::set.graph.attribute(network, "sample.network", TRUE)

    ## set layout for plotting
    lay = matrix(c(  20, 179, 693, 552, 956, 1091, 124, 317, 516, 615, 803, 1038,
                    245, 175, 255, 185, 253, 225,   73,   8,  75,   0,  96,   86),
                 nrow = 12, byrow = FALSE) # for sample graph
    network = igraph::set.graph.attribute(network, "layout", lay)

    return(network)
}
