## (c) Claus Hunsen, 2016
## hunsen@fim.uni-passau.de


## libraries
library(plyr) # for rbind.fill
library(parallel) # for parallel computation


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Utility function for reading file formats
##

## Transform base.data (data.frame) to a list
## - group by thing1
## - use thing2 as sublist items
get.thing2thing = function(base.data, thing1, thing2, extra.data = c()) {
    if (nrow(base.data) == 0) {
        logging::logwarn("Trying to get subset of non-existent data.")
        logging::logwarn(sprintf("Stacktrace:  %s", get.stacktrace(sys.calls())))
        return(list())
    }
    logging::logdebug("get.thing2thing: starting.")

    # get right portion of data
    data = base.data[c(thing1, thing2)]

    cols = c(thing1, thing2, extra.data)
    extra.data.df = base.data[cols]
    # extra.data.df = extra.data.df[order(extra.data.df[[thing1]]), ] # if wanted, sort data.frame while debugging
    colnames(extra.data.df) = cols

    # group list by thing1 and construct a list: thing1 -> (thing2, extra.data)
    transform.df.per.item = function(df) {
        group = unique(df[[thing1]])
        df = df[, -match(c(thing1), names(df)), drop = FALSE] # remove thing1 from columns and keep data.frame
        attr(df, "group.type") = thing1
        attr(df, "group.name") = group
        return(df)
    }
    mylist = dlply(extra.data.df, thing1, transform.df.per.item)

    # remove object attributes introduced by dlply
    attr(mylist, "split_labels") = NULL
    attr(mylist, "split_type") = NULL

    logging::logdebug("get.thing2thing: finished.")

    return(mylist)
}


## Read adjacency matrix as given by Codeface output:
## - headers exist (developer IDs)
## - no row names
## Column names are mapped to the developers' names, row names are set identically
read.adjacency.matrix.from.file = function(file, authors, simple.network = TRUE) {

    if(!file.exists(file)) { # no analysis for the current range
        return(create.empty.network())
    }

    # read data.frame from disk (as expected from Codeface output)
    dat = read.table(file, header = TRUE)

    # get author names whose IDs are
    authors.id = sapply(colnames(dat), function(x) { gsub("X", "", x) }) # remove "X" from col names
    authors.name = authors[ match(authors.id, authors[["ID"]]), "author.name"]

    # set row and col names
    colnames(dat) = authors.name
    rownames(dat) = authors.name

    # construct igraph object from adjacency matrix
    g = graph_from_adjacency_matrix(as.matrix(dat), mode = "directed", weighted = TRUE)

    # transform multiple edges to edge weights
    if (simple.network)
        g = simplify.network(g)

    # return constructed igraph object
    return(g)
}


## Construct a dependency network from the given list of lists
## - e.g., for a list of authors per thread, all authors are connected if they are in the same thread (sublist)
## - if directed, the order of things in the sublists is respected
## - if directed, edge.attrs hold the vector of possible edge attributes in the given list
construct.dependency.network.from.list = function(list, directed = FALSE, simple.network = TRUE,
                                                  extra.edge.attr = c(), skip.threshold = Inf) {

    logging::loginfo("Create edges.")
    logging::logdebug("construct.dependency.network.from.list: starting.")

    # initialize an edge list to fill and the set of nodes
    nodes.processed = c()
    edge.list = data.frame()

    if (directed) {

        ## for all subsets (sets), connect all items in there with the previous ones
        edge.list.data = mclapply(list, function(set) {
            number.edges = sum(1:nrow(set)) - 1
            logging::logdebug("Constructing edges for %s '%s': starting (%s edges to construct).",
                              attr(set, "group.type"), attr(set, "group.name"), number.edges)

            ## Skip artifacts with many, many edges
            if (number.edges > skip.threshold) {
                logging::logwarn("Skipping edges for %s '%s' due to amount (> %s).",
                                 attr(set, "group.type"), attr(set, "group.name"), skip.threshold)
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
                item.edge.attrs = item [1, extra.edge.attr, drop = FALSE]

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

        edge.list = rbind.fill(edge.list.data)
        nodes.processed = unlist( mclapply(edge.list.data, function(data) attr(data, "nodes.processed")) )

    } else {

        ## for all items in the sublists, construct the cartesian product
        edge.list.data = mclapply(list, function(set) {
            number.edges = sum(table(set[,1]) * (dim(table(set[,1])) - 1))
            logging::logdebug("Constructing edges for %s '%s': starting (%s edges to construct).",
                              attr(set, "group.type"), attr(set, "group.name"), number.edges)

            ## Skip artifacts with many, many edges
            if (number.edges > skip.threshold) {
                logging::logwarn("Skipping edges for %s '%s' due to amount (> %s).",
                                 attr(set, "group.type"), attr(set, "group.name"), skip.threshold)
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
                edge.attrs = set[ set[,1] %in% comb, -c(1) ] # everything without first column (i.e., the nodes)
                edgelist = cbind(edge, edge.attrs) # add edge attributes to edge list

                return(edgelist)
            })
            edges = rbind.fill(edges)

            ## store set of processed nodes
            attr(edges, "nodes.processed") = nodes

            return(edges)
        })

        edge.list = rbind.fill(edge.list.data)
        nodes.processed = unlist( mclapply(edge.list.data, function(data) attr(data, "nodes.processed")) )

    }

    logging::loginfo("Construct network from edges.")

    ## get unique list of vertices to produce
    nodes.processed = unique(nodes.processed)

    # if we do not have nodes AND the edge.list is empty, return rightaway
    if (length(nodes.processed) == 0) {
            return(create.empty.network())
    }

    ## if we have nodes to create, but no edges
    if (is.null(edge.list) || nrow(edge.list) == 0) {
        ## create network with only the vertices
        net = make_empty_graph(n = 0, directed = directed) + vertices(nodes.processed)
    }
    ## if we have nodes and edges
    else {
        ## construct network from edge list
        net = graph.data.frame(edge.list, directed = directed, vertices = nodes.processed)
    }

    net = set.vertex.attribute(net, "id", value = get.vertex.attribute(net, "name"))
    net = set.edge.attribute(net, "weight", value = 1)

    # transform multiple edges to edge weights
    if (simple.network)
        net = simplify.network(net)

    logging::logdebug("construct.dependency.network.from.list: finished.")

    return(net)
}


## Read a basic igraph network from disk
##
## In this environment, we only use the 'pajek' format.
read.network.from.file = function(file, format = "pajek") {
    # read the basic graph
    g = read.graph(file, format = "pajek")

    # set vertex labels properly (copy "id" attribute to "name" attribute)
    g = set.vertex.attribute( g, "name", index = V(g), get.vertex.attribute(g, "id") )

    return(g)
}


## Unify the set of vertices in artifacts and author2artifact mapping
##
## As there may be artifacts existing that have not been touched by a developer,
## the two sets need to be unified to avoid null-pointer exceptions
unify.artifact.vertices = function(artifacts.net, author.to.artifact) {

    # get vertex names and set of all related artifacts
    artifacts.net.vertices = get.vertex.attribute(artifacts.net, "name")
    artifacts = unique( rbind.fill(author.to.artifact)[["artifact"]] )

    # get only the missing ones
    diff = setdiff(artifacts, artifacts.net.vertices)

    # add missing vertices to existing network
    net = artifacts.net + vertices(diff)
    net = set.vertex.attribute(net, "id", index = V(net), get.vertex.attribute(net, "name"))

    return(net)

}


## Process vertex names in artifact networks for consistent names
##
## Feature and file networks can have unique naming structures existent
## (especially in the call-graph networks), so the names need to be processed
## to have the same look as the ones from Codeface.
postprocess.artifact.names.callgraph = function(net, artifact) {
    names = vertex_attr(net, "name")

    ## FEATURE
    if (artifact == "feature") {
        names = gsub("^CONFIG_", "ENABLE_", names) # BusyBox
        names = gsub("^1$", "Base_Feature", names) # Base feature
    }
    ## FILE
    else if (artifact == "file") {
        ## transform to relative paths
        names = gsub("^/local/bockthom/TypeChef-BusyboxAnalysis/gitbusybox/", "", names) # BusyBox
        names = gsub("^/local/bockthom/openssl/", "", names) # OpenSSLl
        names = gsub("^/local/bockthom/sqlite/\\./", "", names) # SQLite
        names = gsub("^/local/bockthom/sqlite/", "", names) # SQLite

        ## remove call-graph extension
        names = gsub(".cg", "", names, fixed = TRUE)
    }

    ## set processed names inside graph object
    vertex_attr(net, "name") = names

    return(net)
}


## Construct an edge list for the given network, with timestamps as an extra attribute column
##
## If there are problems accessing the 'date' attribute of a network, this attribute needs to
## be added to it during network construction (defined as a default edge attribute).
get.edgelist.with.timestamps = function(net) {
  ## get edge list as data.frame
  edges = as.data.frame(get.edgelist(net))
  colnames(edges) = c("from", "to")
  ## get timestamps
  dates = get.edge.attribute(net, "date")
  ## bind everything together
  edges = cbind(edges, date = dates)

  return(edges)
}


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Utility function for network simplification
##

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

## Simplify a network
simplify.network = function(network) {

    ## simplify networks (contract edges and remove loops)
    network = igraph::simplify(network, edge.attr.comb = EDGE.ATTR.HANDLING, remove.loops = TRUE)

    return(network)
}

## Simplify a list of networks
simplify.networks = function(networks){
    nets = mclapply(networks, simplify.network)
}


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Utility function to create empty networks that do not break the algorithms
##

create.empty.network = function(directed = TRUE) {

    ## create empty network
    net = make_empty_graph(0, directed = directed)

    # set proper attributes
    vertex_attr(net, "name") = ""
    vertex_attr(net, "type") = 3
    edge_attr(net, "type") = 6

    return(net)

}


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Exemplary network for illustration purposes
##

get.sample.network = function() {

    ## INDEPENDENT NETWORKS
    authors = make_empty_graph(directed = TRUE) + vertices("D1", "D2", "D3", "D4", "D5", "D6") +
        edges("D1", "D2", "D1", "D4", "D3", "D4", "D4", "D5")

    artifacts = make_empty_graph(directed = FALSE) + vertices("A1", "A2", "A3", "A4", "A5", "A6") +
        edges("A1", "A2", "A1", "A3", "A2", "A3", "A2", "A4", "A5", "A6")
    artifacts = as.directed(artifacts, mode = "mutual")

    authors.to.artifacts.df = data.frame(
        author.name = c("D1", "D2", "D3", "D4", "D4", "D5", "D6"),
        artifact    = c("A1", "A1", "A3", "A4", "A5", "A6", "A6")
    )
    authors.to.artifacts = get.thing2thing(authors.to.artifacts.df, "author.name", "artifact")

    ## combine networks
    combined.network = combine.networks(authors, artifacts, authors.to.artifacts) %>%
        set.graph.attribute("sample.network", TRUE)

    return(combined.network)
}


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Stacktrace
##

get.stacktrace = function(calls) {
    lapply(calls, deparse)
}


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Intermediate data
##

#' Save the given 'variable' on the file system (in 'dump.path') if it does not exist already,
#' and load the saved data if it exists.
#'
#' With skip, the check for data existance can be skipped (i.e., force a re-save).
#'
#' This function is for repetitive runs of the same script: It saves intermediate data to disk and
#' loads it from a previous runs if possible. This way, computation time can be saved.
#'
#' @param variable a character naming the data variable to be saved to disk
#' @param dump.path the path where the data is to be saved
#' @param if.not.found if the data does not exist on disk, run this function ('variable' must be a variable there!)
#' @param skip re-save although data exists on the disk?
#'
#' @return the data computed by 'if.not.found' or loaded from 'dump.path'
save.and.load = function(variable, dump.path, if.not.found, skip = FALSE) {
    if (!skip && file.exists(dump.path)) {
        logging::logdebug("Load %s from previously dumped object: %s.", variable, dump.path)
        load(file = dump.path) # load the list named "variable" into the current environment
    } else {
        res = if.not.found()

        assign(variable, res) # rewrite to variable name
        rm(res) # clear memory

        logging::logdebug("Dumping object %s to %s.", variable, dump.path)
        save(list = variable, file = dump.path) # save automatically
    }

    return(get0(variable))
}
