# function that manages the process to calculate EDCPTD centrality for a project
EDCPTD.centrality = function(network.builder){

    # get the single-layer developer networks
    networks = get.author.networks(network.builder)

    # get and order all active developers from the single-layer nteworks
    active.developers = get.developers.from.networks(networks)
    active.developers = active.developers[order(active.developers)]

    # build the tensor representing the single-layer networks
    tensor = build.tensor.from.adjacency(networks, active.developers)

    # calculate EDCPTD centrality
    edcptd = calculate.EDCPTD.centrality(active.developers, tensor)

    # return a data frame containing the names of the active developers and their EDCPTD score
    return(edcptd)
}



# function that gets the single-layer developer networks for the mail, cochange and issue data for the project.
# A list containing all available single-layer networks is returned.
get.author.networks = function(network.builder){

    networks = list()

    # get the single-layer mail developer network if available
    network.builder$update.network.conf(updated.values = list(author.relation = "mail"))

    mail.network = network.builder$get.author.network()


    # only if the mail data is available for the project, the network is added to the list
    if(vcount(mail.network) > 0){
        networks = union(networks, list(mail.network))
    }

    # get the single-layer cochange developer network if available
    network.builder$update.network.conf(updated.values = list(author.relation = "cochange"))

    cochange.network = network.builder$get.author.network()

    # only if the cochange data is available for the project, the network is added to the list
    if(vcount(cochange.network) > 0){
        networks = union(networks, list(cochange.network))
    }

    # get the single-layer issue developer network if available
    network.builder$update.network.conf(updated.values = list(author.relation = "issue"))

    issue.network = network.builder$get.author.network()

    # only if the issue data is available for the project, the network is added to the list
    if(vcount(issue.network) > 0){
        networks = union(networks, list(issue.network))
    }

    return(networks)
}

# Get an ordered vector all developers (i.e. their names) who are in at least one of the networks. If globally == FALSE,
# get a seperate list for each network which contains all the authors in the specific network.
get.developers.from.networks = function(networks, globally = TRUE) {

    # for each network, get a list of authors that are in this network
    active.authors.list = lapply(networks, function(network) {
        active.authors = V(network)$name
        return (active.authors)
    })

    if (globally) {
        # flatten the list of lists to one list of authors
        active.authors = unlist(active.authors.list, recursive = FALSE)

        # remove distracting named list members
        names(active.authors) = NULL

        # remove duplicates and order alphabetically ascending
        active.authors = active.authors[!duplicated(active.authors)]
        active.authors = active.authors[order(active.authors)]
        return (active.authors)
    } else {
        return (active.authors.list)
    }
}

# function that builds a forth-order tensor containing the same information as the single-layer developer networks
build.tensor.from.adjacency = function(networks, active.developers){

    # calculate dimensions for the forth-order tensor
    number.layers = length(networks)
    number.nodes = length(active.developers)

    # get the adjacency matrices of the single-layer developer networks
    adjacency.matrices = parallel::mclapply(networks, get.expanded.adjacency, active.developers)

    # create an array with the size of the forth-order tensor that only contains zeros
    array <-array(0, dim = c(number.nodes, number.layers, number.nodes, number.layers))

    # the entries from every adjacency matrix are transfered to the array
    for (l in 1:length(adjacency.matrices)) {

        mat = as(adjacency.matrices[[l]], "dgTMatrix")

        for (entry in 1:length(mat@x)) {
            array[mat@i[entry]+1, l, mat@j[entry]+1, l] =mat@x[entry]
        }
    }

    # the array is converted into a tensor
    tensor <- rTensor::as.tensor(array)

    return(tensor)

}


# The expanded adjacency is a 3-dimensional matrix where each slice represents
# one time window (i.e. one network in the given list of networks). The rows
# and vertices are the developers who are active in at
# least one network. It is possible to choose whether to consider edge weights.
#
# If a fields value is 0, the two developers were not linked to each other, if
# it is 1 (any positive integer for the weighted version) both developers were
# active and a link between them existed (with the indicated edge weight).
get.expanded.adjacency = function(network, authors, weighted = FALSE){

    # create the matrix for this time step in the appropriate format, adding developer names
    matrix <- sparseMatrix(i = c(), j = c(), dims = c(length(authors), length(authors)), giveCsparse = FALSE)
    matrix <- as(matrix, "dgTMatrix")
    rownames(matrix) <- authors
    colnames(matrix) <- authors

    if(weighted && vcount(network)>0){
      # get the weighted adjacency matrix for the current network
      A <- get.adjacency(network, attr = "weight")
    }else{
      # get the unweighted adjacency matrix for the current network
      A <- get.adjacency(network)
    }

    # order the adjacency matrix
    if(nrow(A)>1){ # for a 1x1 matrix ordering doesn't work
      A <- A[order(rownames(A)),order(colnames(A))]
    }

    # save the activity data per developer
    if(nrow(A)>0){
      matrix[rownames(A), colnames(A)] <- A
    }

    if(!weighted){
      matrix[matrix > 0] <- 1
    }

    return(matrix)
}

# method that calculates the EDCPTD centrality from the tensor
calculate.EDCPTD.centrality = function(authors, tensor){

    # create data frame for results
    data = data.frame(
    names = authors
    )

    # decompose tensor
    decomposition <-rTensor::cp(tensor, num_components = 1, max_iter = 50, tol = 1e-05)


    # calculate EDCPTD centrality

    data[["EDCPTD.score"]] = 0

    for (y in 1:length(decomposition[["U"]][[2]][,1])) {
      data[["EDCPTD.score"]] = data[["EDCPTD.score"]] + abs(decomposition[["U"]][[1]][,1]*decomposition[["U"]][[2]][,1][y]) +abs(decomposition[["U"]][[3]][,1]*decomposition[["U"]][[4]][,1][y])
    }

    data[["EDCPTD.score"]] = data[["EDCPTD.score"]]/2

    return(data)
}



