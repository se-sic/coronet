## (c) Claus Hunsen, 2016
## hunsen@fim.uni-passau.de


## libraries
library(R6) # for R6 classes
library(igraph) # networks
library(plyr) # for dlply function
library(sqldf) # for sqldf
library(logging) # for logging


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

        ## raw data
        ## commits
        commits = NULL, # data.frame
        commits.raw = NULL, # data.frame
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
        read.commits = function(filter.empty.artifacts = TRUE, filter.artifact = TRUE, filter.base.artifact = TRUE) {
            data.path = self$get.data.path()

            ## do not compute anything more than once
            if (!is.null(private$commits)) {
                return(private$commits)
            }

            ## get raw commit data
            browser()
            commit.data = self$get.commits.raw()

            ## FIXME for function-based analysis: artifact = file name + "::" . function name?

            ## only process commits with non-empty artifact
            if (filter.empty.artifacts) {
                commit.data = subset(commit.data, artifact != "")
            }

            ## only process commits with the artifact listed in the configuration
            if (filter.artifact) {
                commit.data = subset(commit.data, artifact.type == private$conf$get.artifact.codeface())
            }

            ## filter out the base artifacts (i.e., Base_Feature, File_Level)
            if (filter.base.artifact ) {
                commit.data = subset(commit.data, !(artifact %in% c("Base_Feature", "File_Level")))
            }

            ## store the commit data
            private$commits = commit.data
        },

        read.commits.raw = function() {

            ## do not compute anything more than once
            if (!is.null(private$commits.raw)) {
                return(private$commits.raw)
            }

            ## get file name of commit data
            data.path = self$get.data.path()
            file = file.path(data.path, "commits.list")

            ## read data.frame from disk (as expected from save.list.to.file) [can be empty]
            commit.data <- try(read.table(file, header = FALSE, sep = ";", strip.white = TRUE), silent = TRUE)

            ## break if the list of commits is empty
            if (inherits(commit.data, 'try-error')) {
                logging::logerror("There are no commits available for the current environment.")
                logging::logerror("Class: %s", self$get.class.name())
                logging::logerror("Configuration: %s", private$conf$get.conf.as.string())
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
                commit.data = sqldf("select *, sum(`artifact.diff.size`) as diffsum from `commit.data` group by hash, file")

                ## copy columns to match proper layout for further analyses
                commit.data["artifact"] = commit.data[["file"]]
                commit.data["artifact.type"] = "File"
                commit.data["artifact.diff.size"] = commit.data[["diffsum"]]
                commit.data["diffsum"] = NULL # remove
            }

            ## store the commit data
            private$commits.raw = commit.data
        },

        ## read the mail data for the range
        read.mails = function() {
            data.path = self$get.data.path()

            ## do not compute anything more than once
            if (!is.null(private$mails)) {
                return(private$mails)
            }

            ## get file name of commit data
            file = file.path(data.path, "emails.list")

            ## read data.frame from disk (as expected from save.list.to.file) [can be empty]
            mail.data <- try(read.table(file, header = FALSE, sep = ";", strip.white = TRUE), silent = TRUE)

            ## break if the list of mails is empty
            if (inherits(mail.data, 'try-error')) {
                logging::logerror("There are no mails available for the current environment.")
                logging::logerror("Class: %s", self$get.class.name())
                logging::logerror("Configuration: %s", private$conf$get.conf.as.string())
                stop("Stopped due to missing mails.")
            }

            ## set proper column names based on Codeface extraction:
            ##
            ## SELECT a.name AS authorName, a.messageId, a.email1, m.creationDate, m.subject, m.threadId
            colnames(mail.data) = c(
                "author.name", "author.email", # author information
                "message.id", "date", "subject", # meta information
                "thread" # thread ID
            )

            ## remove mails without a proper date as they mess up directed mail-based networks
            ## this basically only applies for project-level analysis
            empty.dates = which(mail.data[["date"]] == "" | is.na(mail.data[["date"]]))
            if (length(empty.dates) > 0)
                mail.data = mail.data[-empty.dates, ]

            ## store the mail data
            private$mails = mail.data
        },

        ## read the author data for the range
        read.authors = function() {
            data.path = self$get.data.path()

            ## do not compute anything more than once
            if (!is.null(private$authors)) {
                return(private$authors)
            }

            ## get file name of commit data
            file = file.path(data.path, "authors.list")

            ## read data.frame from disk (as expected from save.list.to.file) [can be empty]
            authors.df <- try(read.table(file, header = FALSE, sep = ";", strip.white = TRUE), silent = TRUE)

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
        },


        ## AUTHOR NETWORKS ####

        ## get the co-change-based developer relation as network
        get.author.network.cochange = function(directed = FALSE, simple.network = TRUE) {

            ## do not compute anything more than once
            if (!is.null(private$authors.network.cochange)) {
                return(private$authors.network.cochange)
            }

            ## read authors if not done yet
            if (is.null(private$authors)) {
                private$read.authors()
            }

            ## TODO 1 - do not use adjacencyMatrix!
            ## TODO 2 - add data to edges (e.g., timestamp of mails or files)
            file = file.path(self$get.data.path(), "adjacencyMatrix.txt")
            dev.relation = read.adjacency.matrix.from.file(file, private$authors, simple.network = simple.network)

            if (!directed) {
                dev.relation = as.undirected(dev.relation) ## TODO 3 - test contraction with the stuff above completely implemented
            }

            ## store network
            private$authors.network.cochange = dev.relation

            return(dev.relation)
        },

        ## get the thread-based developer relation as network
        get.author.network.mail = function(directed = FALSE, simple.network = TRUE) {

            ## do not compute anything more than once
            if (!is.null(private$authors.network.mail)) {
                return(private$authors.network.mail)
            }

            edge.attributes = c("date", "message.id")
            thread2author = self$get.thread2author(extra.data = edge.attributes)
            dev.relation = construct.dependency.network.from.list(thread2author, directed = directed, simple.network = simple.network,
                                                                  extra.edge.attr = edge.attributes)

            ## store network
            private$authors.network.mail = dev.relation

            return(dev.relation)
        },


        ## ARTIFACT NETWORKS ####

        ## co-change-based artifact network
        get.artifact.network.cochange = function(filter.empty.artifacts = TRUE,
                                                 filter.artifact = TRUE,
                                                 filter.base.artifact = TRUE,
                                                 extra.edge.attr = c()) {

            ## do not compute anything more than once
            if (!is.null(private$artifacts.network.cochange)) {
                return(private$artifacts.network.cochange)
            }

            commit2artifact = self$get.commit2artifact(filter.empty.artifacts = filter.empty.artifacts,
                                                       filter.artifact = filter.artifact,
                                                       filter.base.artifact = filter.base.artifact,
                                                       extra.data = extra.edge.attr)
            artifacts.net = construct.dependency.network.from.list(commit2artifact, extra.edge.attr = extra.edge.attr)

            ## store network
            private$artifacts.network.cochange = artifacts.net

            return(artifacts.net)
        },

        ## call-graph-based artifact network
        ## IMPORTANT: This only works for range-level analyses! (errors otherwise)
        get.artifact.network.callgraph = function() {

            ## do not compute anything more than once
            if (!is.null(private$artifacts.network.callgraph)) {
                return(private$artifacts.network.callgraph)
            }

            ## check if revision for call-graphs is set
            if (is.null(private$revision.callgraph)) {
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

            return(artifacts.net)
        }

    ),


    ## public members ####
    public = list(

        ## constructor
        initialize = function(conf) {
            if (!missing(conf) && "CodefaceConf" %in% class(conf)) {
                private$conf <- conf
            }
        },


        ## TO STRING ;) ####

        get.class.name = function() {
            return("CodefaceProjectData")
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

        ## construct the absolute path to the range's result folder
        get.data.path = function() {
            data.path = private$conf$get.datapath()
            return(data.path)
        },


        ## RAW DATA ####

        ## get the complete filtered list of commits
        get.commits = function(filter.empty.artifacts = TRUE, filter.artifact = TRUE, filter.base.artifact = TRUE) {
            ## if commits are not read already, do this
            if (is.null(private$commits)) {
                private$read.commits(filter.empty.artifacts = filter.empty.artifacts,
                                     filter.artifact = filter.artifact,
                                     filter.base.artifact = filter.base.artifact)
            }

            return(private$commits)
        },

        ## get the complete raw list of commits
        get.commits.raw = function() {
            ## if commits are not read already, do this
            if (is.null(private$commits.raw)) {
                private$read.commits.raw()
            }
            browser()

            return(private$commits.raw)
        },

        ## get the complete list of mails
        get.mails = function() {
            ## if mails are not read already, do this
            if (is.null(private$mails)) {
                private$read.mails()
            }

            return(private$mails)
        },

        ## get the ID--author mapping
        get.authors = function() {
            ## if authors are not read already, do this
            if (is.null(private$authors)) {
                private$read.authors()
            }

            return(private$authors)
        },


        ## DATA ####

        ## get the artifacts for each author
        ## (formerly Author2ArtifactExtraction, authors2{artifact}.list)
        get.author2artifact = function(filter.empty.artifacts = TRUE, filter.artifact = TRUE, filter.base.artifact = TRUE,
                                       extra.data = c()) {
            ## if commits are not read already, do this
            if (is.null(private$commits)) {
                private$read.commits(filter.empty.artifacts = filter.empty.artifacts,
                                     filter.artifact = filter.artifact,
                                     filter.base.artifact = filter.base.artifact)
            }

            ## store the authors per artifact
            mylist = get.thing2thing(private$commits, "author.name", "artifact", extra.data = c(extra.data))

            return(mylist)
        },

        ## get the files for each author
        ## (formerly Author2FileExtraction, authors2file.list)
        get.author2file = function(filter.empty.artifacts = TRUE, filter.artifact = TRUE, filter.base.artifact = TRUE) {
            ## if commits are not read already, do this
            if (is.null(private$commits)) {
                private$read.commits(filter.empty.artifacts = filter.empty.artifacts,
                                     filter.artifact = filter.artifact,
                                     filter.base.artifact = filter.base.artifact)
            }

            ## store the authors per artifact
            mylist = get.thing2thing(private$commits, "author.name", "file")

            return(mylist)
        },

        ## get the artifacts for each commits
        ## (formerly Commit2ArtifactExtraction, commit2artifact.list)
        get.commit2artifact = function(filter.empty.artifacts = TRUE, filter.artifact = TRUE,
                                       filter.base.artifact = TRUE, extra.data = c()) {
            ## if commits are not read already, do this
            if (is.null(private$commits)) {
                private$read.commits(filter.empty.artifacts = filter.empty.artifacts,
                                     filter.artifact = filter.artifact,
                                     filter.base.artifact = filter.base.artifact)
            }

            ## store the authors per artifact
            mylist = get.thing2thing(private$commits, "hash", "artifact", extra.data = c(extra.data))

            return(mylist)
        },

        ## get the files for each commits
        ## (formerly Commit2FileExtraction, commit2file.list)
        get.commit2file = function(filter.empty.artifacts = TRUE, filter.artifact = TRUE, filter.base.artifact = TRUE) {
            ## if commits are not read already, do this
            if (is.null(private$commits)) {
                private$read.commits(filter.empty.artifacts = filter.empty.artifacts,
                                     filter.artifact = filter.artifact,
                                     filter.base.artifact = filter.base.artifact)
            }

            ## store the authors per artifact
            mylist = get.thing2thing(private$commits, "hash", "file")

            return(mylist)
        },

        ## get the authors for each mail thread
        ## (formerly Thread2AuthorExtraction, thread2author.list)
        get.thread2author = function(extra.data = c()) {
            ## if mails are not read already, do this
            if (is.null(private$mails)) {
                private$read.mails()
            }

            ## store the authors per thread
            mylist = get.thing2thing(private$mails, "thread", "author.name",
                                     extra.data = extra.data)

            return(mylist)
        },

        ## get the developer relation as network (generic)
        get.author.network = function(relation = c("mail", "cochange"), directed = FALSE, simple.network = TRUE) {
            relation = match.arg(relation)
            if (relation == "cochange")
                return(private$get.author.network.cochange(directed = directed, simple.network = simple.network))
            else if (relation == "mail")
                return(private$get.author.network.mail(directed = directed, simple.network = simple.network))
            else
                stop(sprintf("The author relation '%s' does not exist.", relation))
        },

        ## get artifact relation as network (generic)
        get.artifact.network = function(relation = c("cochange", "callgraph"),
                                        filter.empty.artifacts = TRUE, filter.artifact = TRUE, filter.base.artifact = TRUE,
                                        extra.edge.attr = c()) {
            relation = match.arg(relation)
            if (relation == "cochange")
                return(private$get.artifact.network.cochange(
                    filter.empty.artifacts = filter.empty.artifacts,
                    filter.artifact = filter.artifact,
                    filter.base.artifact = filter.base.artifact,
                    extra.edge.attr = extra.edge.attr))
            else if (relation == "callgraph")
                return(private$get.artifact.network.callgraph())
            else
                stop(sprintf("The artifact relation '%s' does not exist.", relation))
        },

        ## get all networks (build unification to avoid null-pointers)
        get.networks = function(author.relation = c("mail", "cochange"), artifact.relation = c("cochange", "callgraph"),
                                author.directed = FALSE, author.only.committers = FALSE,
                                artifact.extra.edge.attr = c("date", "hash"), artifact.filter.empty = TRUE,
                                artifact.filter = TRUE, artifact.filter.base = TRUE,
                                simple.network = TRUE) {

            ## get method arguments
            author.relation = match.arg(author.relation)
            artifact.relation = match.arg(artifact.relation)

            ## authors-artifact relation
            authors.to.artifacts = self$get.author2artifact(extra.data = artifact.extra.edge.attr,
                                                            filter.empty.artifacts = artifact.filter.empty,
                                                            filter.artifact = artifact.filter,
                                                            filter.base.artifact = artifact.filter.base)

            ## authors relation
            authors.net = self$get.author.network(relation = author.relation,
                                                  directed = author.directed, simple.network = simple.network)

            ## unify vertices with developer-artifact relation
            authors.from.net = get.vertex.attribute(authors.net, "name")
            authors.from.artifacts = names(authors.to.artifacts)
            authors.net = authors.net + vertices(setdiff(authors.from.artifacts, authors.from.net))

            ## remove all authors from the corresponding network who do not have touched any artifact
            if (author.only.committers & !is.null(authors.from.artifacts))
                authors.net = delete.vertices(authors.net, setdiff(authors.from.net, authors.from.artifacts))

            ## artifact relation
            artifacts.net = self$get.artifact.network(artifact.relation,
                                                      filter.empty.artifacts = artifact.filter.empty,
                                                      filter.artifact = artifact.filter,
                                                      filter.base.artifact = artifact.filter.base,
                                                      extra.edge.attr = artifact.extra.edge.attr)
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
        get.bipartite.network = function(simple.network = TRUE, contract.edges = simple.network,
                                         artifact.extra.edge.attr = c("date", "hash"), ...) {

            networks = self$get.networks(simple.network = simple.network, artifact.extra.edge.attr = artifact.extra.edge.attr, ...)

            authors.to.artifacts = networks[["authors.to.artifacts"]]
            authors.net = networks[["authors.net"]]
            artifacts.net = networks[["artifacts.net"]]

            u = combine.networks(authors.net, artifacts.net, authors.to.artifacts,
                                 simple.network = simple.network, contract.edges = contract.edges,
                                 extra.data = artifact.extra.edge.attr)

            return(u)

        }

        ## FIXME split data by three-month ranges (e.g., giveb by argument (four months, ten days, ...))

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
        revision.callgraph = NULL # character
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
        },

        ## TO STRING ;) ####

        get.class.name = function() {
            return(
                sprintf("CodefaceRangeData<%s, %s>", private$range, private$revision.callgraph)
            )
        },


        ## PATHS ####

        ## construct the absolute path to the range's result folder
        get.data.path = function() {
            data.path = private$conf$get.datapath()
            range = private$range
            return(file.path(data.path, range))
        },

        ## construct the absolute path to the range's result folder
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
                            contract.edges = simple.network, extra.data = c()) {

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

    ## set network as undirected
    ## set contraction mode (collapse is contracting, each is not)
    contraction.mode = "collapse"
    if (!contract.edges)
        contraction.mode = "each"
    ## convert to undirected
    u = as.undirected(u, mode = contraction.mode, edge.attr.comb = list(weight = sum, type = "first", "concat"))

    return(u)
}


## helper function to add dependencies from dev--art mapping to the bipartite network
add.edges.for.devart.relation = function(net, auth.to.arts, edge.type = TYPE.EDGES.INTER, extra.data = c()) {

    # construct edges (i.e., a vertex sequence with c(source, target, source, target, ...))
    vertex.sequence.for.edges = mapply(function(d, a.df) {
        a = a.df[["artifact"]]
        new.edges = lapply(a, function(art) {
            V(net)[d, art] # get two vertices from source network:  c(developer, artifact)
        })
        return(new.edges)
    }, names(auth.to.arts), auth.to.arts)

    ## get extra edge attributes
    extra.edge.attributes.df = lapply(auth.to.arts, function(a.df) {
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
