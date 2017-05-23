## (c) Claus Hunsen, 2016
## hunsen@fim.uni-passau.de


## libraries
library(igraph) # networks


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Utility function for plotting author networks
##
## Given networks need type attributes on all vertices and edges

plot.network = function(net, labels = TRUE, grayscale = FALSE) {

    ## colors and shapes
    vertex.colors = c("#00AEFF", "#FF8B00", "#FF0000")
    edge.colors = c("#999999", "#14CC3B", "#FF0000")
    edge.lty = c(2, 1, 3)
    vertex.shapes = c("circle", "square", "triangle")
    vertex.shapes.pch = c(16, 15, 17)

    ## b/w version
    vertex.colors.bw = c("gray40", "gray30", "gray20")
    edge.colors.bw = c("gray60", "gray40")
    if (grayscale) {
        vertex.colors = vertex.colors.bw
        edge.colors = edge.colors.bw
    }

    ## legend colors and types
    vertex.colors.legend = c(vertex.colors[1:2], edge.colors[1:2])
    edge.lty.legend = c(NA, NA, edge.lty[1:2])
    vertex.shapes.pch.legend = c(vertex.shapes.pch[1:2], NA, NA)

    ## vertex color by "type" attribute
    V(net)$color <- vertex.colors[V(net)$type]
    V(net)$shape <- vertex.shapes[V(net)$type]
    # ## vertex color by "community" attribute
    # plot(net, vertex.color = vertex.colors[ V(net)$community ])

    ##  Compute node degrees (#links) and use that to set node size:
    # V(net)$size = degree(net, mode = "all") * 4
    V(net)$label.cex = .8
    V(net)$label.color = ifelse(grayscale, "white", "black")

    ## change arrow size and edge color:
    E(net)$arrow.size <- .2
    E(net)$width <- 1 + log10(E(net)$weight/2)
    # E(net)$curved = .1 # curvy edges
    E(net)$type = unlist(E(net)$type) - 2 # as TYPE.EDGE.INTER is 3, we need to re-map this to 1
    E(net)$color = edge.colors[unlist(E(net)$type)]
    E(net)$lty = edge.lty[unlist(E(net)$type)]

    ## omit labels if wanted
    if (labels == FALSE) {
        V(net)$label = NA
    }

    ## plot network
    if (!is.plot.empty(net)) {

        ## if we have a sample network, set the layout globally
        is.sample.network = !is.null(get.graph.attribute(net, "sample.network")) && get.graph.attribute(net, "sample.network") == TRUE
        if (is.sample.network) {
            par(mai = c(2,0,0,0), mar = c(0,0,0,0))

            lay = matrix(c(  20, 179, 552, 693, 956, 1091, 124, 317, 516, 615, 803, 1038,
                            245, 175, 185, 255, 253, 225,   73,   8,  75,   0,  96,   86),
                         nrow = 12, byrow = FALSE) # for sample graph
            graph_attr(net, "layout") = lay

            # id = tkplot(net, canvas.width = 1450, canvas.height = 450, rescale = FALSE)
            # browser()
            # lay = tk_coords(id)

            V(net)$label.cex = 1.25

            plot(net, layout = lay, asp = 0, margin = c(0.7,0,0.1,0))
        }
        ## else plot the network with the default igraph layout
        else {
            plot(net, margin = c(0.7,0,0.1,0))
        }
    }
    else {
        plot.new()
    }
    legend("bottom", c("Developers", "Artifacts", "Unipartite Edges", "Bipartite Edges"), bty = "n", ncol = 2,
           pch = vertex.shapes.pch.legend, col = vertex.colors.legend, pt.bg = vertex.colors.legend, lty = edge.lty.legend,
           cex = 1.25, pt.cex = 2, lwd = 2)

}


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Utility function for plotting bipartite networks
##

plot.bipartite.network = function(net, labels = TRUE, grayscale = FALSE) {

    ## correct missing type attributes
    E(net)[ is.na(type) ]$type = 5
    V(net)[ is.na(type) ]$type = 5

    ## plot
    plot.network(net, labels = labels, grayscale = grayscale)

}


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Utility function for plotting author networks
##

plot.author.network = function(net, labels = TRUE, grayscale = FALSE) {

    ## correct missing type attributes
    net = set.edge.attribute(net, "type", value = TYPE.EDGES.INTRA)
    net = set.vertex.attribute(net, "type", value = TYPE.AUTHOR)

    ## plot
    plot.network(net, labels = labels, grayscale = grayscale)

}


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Utility function for plotting artifact networks
##

plot.artifact.network = function(net, labels = TRUE, grayscale = FALSE) {

    ## correct missing type attributes
    net = set.edge.attribute(net, "type", value = TYPE.EDGES.INTRA)
    net = set.vertex.attribute(net, "type", value = TYPE.ARTIFACT)

    ## plot
    plot.network(net, labels = labels, grayscale = grayscale)

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
