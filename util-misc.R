## (c) Claus Hunsen, 2016, 2017
## hunsen@fim.uni-passau.de
## (c) Raphael Nömmer, 2017
## noemmer@fim.uni-passau.de
## (c) Christian Hechtl, 2017
## hechtl@fim.uni-passau.de


## libraries
requireNamespace("plyr") # for rbind.fill and dlply
requireNamespace("parallel") # for parallel computation
requireNamespace("igraph") # networks
requireNamespace("logging")


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
construct.dependency.network.from.list = function(list, network.conf, directed = FALSE) {
    logging::loginfo("Create edges.")
    logging::logdebug("construct.dependency.network.from.list: starting.")

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
            return(create.empty.network())
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

    logging::logdebug("construct.dependency.network.from.list: finished.")

    return(net)
}


#' Read a basic igraph network from disk.
#' The format for this environment is 'pajek'.
#'
#' @param file the path to the network file
#' @param format the format
#'
#' @return the read network
read.network.from.file = function(file, format = "pajek") {
    # read the basic graph
    g = igraph::read.graph(file, format = "pajek")

    # set vertex labels properly (copy "id" attribute to "name" attribute)
    g = igraph::set.vertex.attribute(g, "name", index = igraph::V(g), igraph::get.vertex.attribute(g, "id"))

    return(g)
}


#' Process vertex names in artifact networks for consistent names.
#'
#' Since feature and file networks may have unique naming structures, the names
#' need to be processed in order to match the ones coming from different analyses
#' (e.g. Codeface)
#'
#' @param net the network to be processed
#' @param artifact the artifact of the network
#'
#' @return the processed network
postprocess.artifact.names.callgraph = function(net, artifact) {
    names = igraph::get.vertex.attribute(net, "name")

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
    net = igraph::set.vertex.attribute(net, "name", value = names)

    return(net)
}


#' Construct an edge list for the given network, with timestamps as an extra attribute column.
#'
#' The 'date' attribute has to be added during network construction as default edge attribute
#' in order to avoid problems accessing it.
#'
#' @param net the given network
#'
#' @return the new edgelist
get.edgelist.with.timestamps = function(net) {
  ## get edge list as data.frame
  edges = as.data.frame(igraph::get.edgelist(net))
  colnames(edges) = c("from", "to")
  ## get timestamps
  dates = igraph::get.edge.attribute(net, "date")
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


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Utility function to create empty networks that do not break the algorithms
##

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


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Stacktrace
##

#' Get the stacktrace.
#'
#' @param calls the calls of the stacktrace
#'
#' @return the built stacktrace
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
