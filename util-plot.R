## (c) Claus Hunsen, 2016
## hunsen@fim.uni-passau.de



## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Utility function for plotting author networks
##
## Given networks need type attributes on all vertices and edges

plot.network = function(net) {

    ## colors and shapes
    vertex.colors <- c("skyblue", "orange", "red")
    vertex.colors.dark = c("deepskyblue", "darkorange", "red")
    edge.colors = c("gray80", "chartreuse3", "red")
    edge.lty = c(2, 1, 3)
    vertex.shapes = c("circle", "square", "triangle")
    vertex.shapes.pch = c(16, 15, 17)

    ## vertex color by "type" attribute
    V(net)$color <- vertex.colors.dark[V(net)$type]
    V(net)$shape <- vertex.shapes[V(net)$type]
    # ## vertex color by "community" attribute
    # plot(net, vertex.color = vertex.colors.dark[ V(net)$community ])

    ##  Compute node degrees (#links) and use that to set node size:
    # V(net)$size = degree(net, mode = "all") * 4
    V(net)$label.cex = .7
    V(net)$label.color = "black"

    ## change arrow size and edge color:
    E(net)$arrow.size <- .2
    E(net)$width <- 1 + E(net)$weight/6
    # E(net)$curved = .1 # curvy edges
    E(net)$type = unlist(E(net)$type) - 2 # as TYPE.EDGE.INTER is 3, we need to re-map this to 1
    E(net)$color = edge.colors[unlist(E(net)$type)]
    E(net)$lty = edge.lty[unlist(E(net)$type)]

    # ## layout of graph
    # lay = matrix(c(56, 89, 189, 245, 349, 405, 60, 56, 145, 172, 272, 341, 282, 223, 214, 276, 270, 289, 90, 30, 67, 0, 86, 65),
    #              nrow = 12, byrow = FALSE) # for sample graph
    # graph_attr(net, "layout") = lay

    # plot network
    if (!is.plot.empty(net))
        plot(net)
    else {
        plot.new()
    }
    legend("bottomleft", c("Developers", "Artifacts"),
           pch = vertex.shapes.pch, col = vertex.colors.dark, pt.bg = vertex.colors.dark, pt.cex = 2, cex = .8, bty = "n", ncol = 1)

}


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Utility function for plotting bipartite networks
##

plot.bipartite.network = function(net) {

    ## correct missing type attributes
    E(net)[ is.na(type) ]$type = 5
    V(net)[ is.na(type) ]$type = 5

    ## plot
    plot.network(net)

}


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Utility function for plotting author networks
##

plot.author.network = function(net) {

    ## correct missing type attributes
    net = set.edge.attribute(net, "type", value = TYPE.EDGES.INTRA)
    net = set.vertex.attribute(net, "type", value = TYPE.AUTHOR)

    ## plot
    plot.network(net)

}


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Utility function for plotting artifact networks
##

plot.artifact.network = function(net) {

    ## correct missing type attributes
    net = set.edge.attribute(net, "type", value = TYPE.EDGES.INTRA)
    net = set.vertex.attribute(net, "type", value = TYPE.ARTIFACT)

    ## plot
    plot.network(net)

}


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Define triangle shape for vertices
##
## http://igraph.org/r/doc/shapes.html

mytriangle <- function(coords, v=NULL, params) {
    vertex.color <- params("vertex", "color")
    if (length(vertex.color) != 1 && !is.null(v)) {
        vertex.color <- vertex.color[v]
    }
    vertex.size <- 1/150 * params("vertex", "size")
    if (length(vertex.size) != 1 && !is.null(v)) {
        vertex.size <- vertex.size[v]
    }

    symbols(x=coords[,1], y=coords[,2], bg=vertex.color,
            stars=cbind(vertex.size, vertex.size, vertex.size),
            add=TRUE, inches=FALSE)
}
# clips as a circle
add_shape("triangle", clip=shapes("circle")$clip, plot=mytriangle)


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Ability to check emptiness of a network
##

is.plot.empty = function(net) {
    empty = vcount(net) == 0
    return(empty)
}
