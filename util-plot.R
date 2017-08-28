## (c) Claus Hunsen, 2017
## hunsen@fim.uni-passau.de


## libraries
requireNamespace("igraph") # networks
requireNamespace("ggplot2") ## plotting
requireNamespace("ggraph") ## plotting networks


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Global plot options (e.g., vertex size and colors)
##

## vertex size
VERTEX.SIZE = 10

## colors for vertices and edges (colored)
PLOT.COLORS.BY.TYPE.VERTEX = c("#00AEFF", "#FF8B00")
names(PLOT.COLORS.BY.TYPE.VERTEX) = c(TYPE.AUTHOR, TYPE.ARTIFACT)
PLOT.COLORS.BY.TYPE.EDGE = c("#999999", "#14CC3B")
names(PLOT.COLORS.BY.TYPE.EDGE) =  c(TYPE.EDGES.INTRA, TYPE.EDGES.INTER)

## colors for vertices and edges (grayscale)
PLOT.COLORS.BY.TYPE.VERTEX.GRAY = c("gray40", "gray30")
names(PLOT.COLORS.BY.TYPE.VERTEX) = c(TYPE.AUTHOR, TYPE.ARTIFACT)
PLOT.COLORS.BY.TYPE.EDGE.GRAY = c("gray60", "gray40")
names(PLOT.COLORS.BY.TYPE.EDGE) =  c(TYPE.EDGES.INTRA, TYPE.EDGES.INTER)

## names for vertex and edge types
PLOT.NAMES.BY.TYPE.VERTEX = c("Developer", "Artifact")
names(PLOT.NAMES.BY.TYPE.VERTEX) = c(TYPE.AUTHOR, TYPE.ARTIFACT)
PLOT.NAMES.BY.TYPE.EDGE = c("unipartite", "bipartite")
names(PLOT.NAMES.BY.TYPE.EDGE) =  c(TYPE.EDGES.INTRA, TYPE.EDGES.INTER)


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Plot functions
##

#' Construct a ggplot2/ggraph plot object for the given network and print it directly.
#'
#' @param network the network to plot and print
#' @param labels logical indicating whether vertex lables should be plotted [default: TRUE]
#' @param grayscale logical indicating whether the plot is to be in grayscale, by default, it is colored
#'                  [default: FALSE]
#'
#' @return the network (invisibly)
#'
#' @aliases plot.print.network
plot.network = function(network, labels = TRUE, grayscale = FALSE) {
    plot.print.network(network, labels = labels, grayscale = grayscale)
}

#' Construct a ggplot2/ggraph plot object for the given network and print it directly.
#'
#' @param network the network to plot and print
#' @param labels logical indicating whether vertex lables should be plotted [default: TRUE]
#' @param grayscale logical indicating whether the plot is to be in grayscale, by default, it is colored
#'                  [default: FALSE]
#'
#' @return the network (invisibly)
#'
#' @aliases plot.network
plot.print.network = function(network, labels = TRUE, grayscale = FALSE) {
    p = plot.get.plot.for.network(network, labels = labels, grayscale = grayscale)
    print(p)
}

#' Construct a ggplot2/ggraph plot object for the given network.
#'
#' @param network the network to plot
#' @param labels logical indicating whether vertex lables should be plotted [default: TRUE]
#' @param grayscale logical indicating whether the plot is to be in grayscale, by default, it is colored
#'                  [default: FALSE]
#'
#' @return a ggplot2/ggraph plot object
plot.get.plot.for.network = function(network, labels = TRUE, grayscale = FALSE) {
    ## set colors for grayscale
    if (grayscale) {
        colors.vertex = PLOT.COLORS.BY.TYPE.VERTEX.GRAY
        colors.edge = PLOT.COLORS.BY.TYPE.EDGE.GRAY
        colors.vertex.label = "white"
    }
    ## set colors for colored
    else {
        colors.vertex = PLOT.COLORS.BY.TYPE.VERTEX
        colors.edge = PLOT.COLORS.BY.TYPE.EDGE
        colors.vertex.label = "black"
    }

    ## set size of vertices in legend
    VERTEX.SIZE.LEGEND = VERTEX.SIZE / 2

    ## check if network is empty
    if (igraph::vcount(network) == 0) {
        network = create.empty.network(directed = igraph::is.directed(network)) +
            igraph::vertices(c("", ""), type = c(TYPE.AUTHOR, TYPE.ARTIFACT)) # + igraph::edges(c(1,2), type = TYPE.EDGES.INTER)
        VERTEX.SIZE = 0
    }

    ## fix the type attributes (add new ones, also named)
    network = plot.fix.type.attributes(network, colors.vertex = colors.vertex, colors.edge = colors.edge)

    ## create a ggraph object
    p = ggraph::ggraph(network, layout = "igraph", algorithm = "nicely")

    ## plot edges if there are any
    if (igraph::ecount(network) > 0) {
        p = p +
            ggraph::geom_edge_fan(
                mapping = ggplot2::aes(colour = edge.type, linetype = edge.type),
                end_cap = ggraph::circle(VERTEX.SIZE + 3, 'pt'),
                start_cap = ggraph::circle(VERTEX.SIZE + 3, 'pt'),
                arrow = if (igraph::is.directed(network)) {
                        ggplot2::arrow(length = ggplot2::unit(VERTEX.SIZE / 2, 'pt'), ends = "last", type = "closed")
                    } else {
                        NULL
                    }
            )
    }

    p = p +

        ## plot vertices
        ggraph::geom_node_point(ggplot2::aes(color = vertex.type, shape = vertex.type), size = VERTEX.SIZE) +
        ggraph::geom_node_text(ggplot2::aes(label = if (labels) name else c("")), size = 3.5, color = colors.vertex.label) +

        ## scale vertices (colors and styles)
        ggplot2::scale_color_manual("Vertices", values = colors.vertex, labels = PLOT.NAMES.BY.TYPE.VERTEX) +
        ggplot2::scale_shape_manual("Vertices", values = c(16, 15), labels = PLOT.NAMES.BY.TYPE.VERTEX) +

        ## scale edges (colors and styles)
        ggraph::scale_edge_linetype_manual("Relations", values = c("dashed", "solid"), labels = PLOT.NAMES.BY.TYPE.EDGE) +
        ggraph::scale_edge_colour_manual("Relations", values = colors.edge, labels = PLOT.NAMES.BY.TYPE.EDGE) +

        ## theme
        ggplot2::theme_light() +
        ggplot2::guides(
            ## reduce size of symbols in legend
            shape = ggplot2::guide_legend(override.aes = list(size = VERTEX.SIZE.LEGEND))
        ) +
        ggplot2::theme(
            legend.position = "bottom",
            axis.line=ggplot2::element_blank(),
            axis.text.x=ggplot2::element_blank(),
            axis.text.y=ggplot2::element_blank(),
            axis.ticks=ggplot2::element_blank(),
            axis.title.x=ggplot2::element_blank(),
            axis.title.y=ggplot2::element_blank(),
            panel.background=ggplot2::element_blank(),
            panel.grid.major=ggplot2::element_blank(),
            panel.grid.minor=ggplot2::element_blank(),
            plot.background=ggplot2::element_blank()
        )

    return(p)
}


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Low-level functionality
##

#' Extend and transform the 'type' attribute of vertices and edges in the given network to get
#' more flexibility while plotting.
#'
#' Note: By default, the 'type' attribute of vertices is set either to 'TYPE.AUTHOR' or 'TYPE.ARTIFACT',
#' the 'type' attribute of edges is set to 'TYPE.EDGES.INTER' or 'TYPE.EDGES.INTRA'.
#'
#' This function transforms the vertex attribute 'type' to logical values for better plotting with the
#' plot layout 'bipartite'. The mapping is as follows:
#' - TYPE.AUTHOR = FALSE, and
#' - TYPE.ARTIFACT = TRUE.
#'
#' Furthermore, the following attributes are added to either vertices or edges:
#' - vertex.color = a color code (hex or else) for coloring the vertices (see 'colors.vertex' parameter),
#' - edge.color = a color code (hex or else) for coloring the edges (see 'colors.edge' parameter),
#' - vertex.type = the old vertex attribute 'type', but as a character,
#' - edge.type = the old edge attribute 'type', but as a character,
#' - vertex.type.char = a word/name describing the vertex type (see PLOT.NAMES.BY.TYPE.VERTEX), and
#' - edge.type.char = a word/name describing the edge type (see PLOT.NAMES.BY.TYPE.EDGE).
#'
#' @param network the igraph object to augment
#' @param colors.vertex a vector of length 2, the entries named with 'TYPE.AUTHOR' and 'TYPE.ARTIFACT'
#'                      [default: PLOT.COLORS.BY.TYPE.VERTEX]
#' @param colors.edge a vector of length 2, the entries named with 'TYPE.EDGES.INTER' and 'TYPE.EDGES.INTRA'
#'                    [default: PLOT.COLORS.BY.TYPE.EDGE]
#'
#' @return the old network with the new and changed vertex and edge attributes
plot.fix.type.attributes = function(network, colors.vertex = PLOT.COLORS.BY.TYPE.VERTEX, colors.edge = PLOT.COLORS.BY.TYPE.EDGE) {
    ## copy type attribute to vertex.type and edge.type
    network = igraph::set.vertex.attribute(network, "vertex.type", value = as.character(igraph::get.vertex.attribute(network, "type")))
    network = igraph::set.vertex.attribute(network, "vertex.type.char",
                                           value = as.character(PLOT.NAMES.BY.TYPE.VERTEX[ igraph::get.vertex.attribute(network, "vertex.type") ]))
    network = igraph::set.edge.attribute(network, "edge.type", value = as.character(igraph::get.edge.attribute(network, "type")))
    network = igraph::set.edge.attribute(network, "edge.type.char",
                                         value = as.character(PLOT.NAMES.BY.TYPE.EDGE[ igraph::get.edge.attribute(network, "edge.type") ]))

    ## add edge and vertex colors
    network = igraph::set.vertex.attribute(network, "vertex.color",
                                       value = colors.vertex[ igraph::get.vertex.attribute(network, "vertex.type") ])
    network = igraph::set.edge.attribute(network, "edge.color",
                                     value = colors.edge[ igraph::get.edge.attribute(network, "edge.type") ])

    ## adjust 'type' attribute for vertices for bipartite plotting
    types = igraph::get.vertex.attribute(network, "type")
    network = igraph::remove.vertex.attribute(network, "type")
    network = igraph::set.vertex.attribute(network, "type", value = sapply(
        types, function(t) switch(t, FALSE, TRUE)
    ))

    return(network)
}
