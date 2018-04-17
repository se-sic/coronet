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
## Copyright 2017-2018 by Claus Hunsen <hunsen@fim.uni-passau.de>
## Copyright 2018 by Barbara Eckl <ecklbarb@fim.uni-passau.de>
## All Rights Reserved.


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Libraries ---------------------------------------------------------------

requireNamespace("igraph") # networks
requireNamespace("ggplot2") ## plotting
requireNamespace("ggraph") ## plotting networks


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Global plot options -----------------------------------------------------
## (e.g., vertex size and colors)

## vertex-type names
PLOT.VERTEX.TYPE.AUTHOR = "Developer" # TYPE.AUTHOR
PLOT.VERTEX.TYPE.ARTIFACT = TYPE.ARTIFACT # "Artifact"

## vertex size
PLOT.VERTEX.SIZE = 10
PLOT.VERTEX.SIZE.LEGEND = PLOT.VERTEX.SIZE / 2

## colors for vertices and edges (colored)
PLOT.COLORS.BY.TYPE.VERTEX = c("#00AEFF", "#FF8B00")
names(PLOT.COLORS.BY.TYPE.VERTEX) = c(TYPE.AUTHOR, TYPE.ARTIFACT)
PLOT.COLORS.BY.TYPE.EDGE = c("#999999", "#14CC3B")
names(PLOT.COLORS.BY.TYPE.EDGE) =  c(TYPE.EDGES.INTRA, TYPE.EDGES.INTER)

PLOT.COLORS.BY.RELATION.EDGE = c("#142ACC", "#0BE5E0", "#960BE5")
names(PLOT.COLORS.BY.RELATION.EDGE) = c(RELATION.EDGES.INTRA.COCHANGE, RELATION.EDGES.INTRA.ISSUE,
                                        RELATION.EDGES.INTRA.MAIL)


## colors for vertices and edges (grayscale)
PLOT.COLORS.BY.TYPE.VERTEX.GRAY = c("gray40", "gray30")
names(PLOT.COLORS.BY.TYPE.VERTEX.GRAY) = c(TYPE.AUTHOR, TYPE.ARTIFACT)
PLOT.COLORS.BY.TYPE.EDGE.GRAY = c("gray60", "gray40")
names(PLOT.COLORS.BY.TYPE.EDGE.GRAY) =  c(TYPE.EDGES.INTRA, TYPE.EDGES.INTER)
PLOT.COLORS.BY.RELATION.EDGE.GRAY = c("gray60", "gray62", "gray64")
names(PLOT.COLORS.BY.RELATION.EDGE.GRAY) = c(RELATION.EDGES.INTRA.COCHANGE, RELATION.EDGES.INTRA.ISSUE,
                                             RELATION.EDGES.INTRA.MAIL)

## shapes of vertices and edges
PLOT.SHAPE.VERTEX = c(16, 15) # (authors, artifacts)
names(PLOT.SHAPE.VERTEX) = c(TYPE.AUTHOR, TYPE.ARTIFACT)
PLOT.SHAPE.EDGE = c("dashed", "solid") # (unipartite, bipartite)
names(PLOT.SHAPE.EDGE) = c(TYPE.EDGES.INTRA, TYPE.EDGES.INTER)
PLOT.NAMES.BY.RELATION.EDGE = c("mail", "issue", "cochange")
names(PLOT.NAMES.BY.RELATION.EDGE) = c(RELATION.EDGES.INTRA.MAIL, RELATION.EDGES.INTRA.ISSUE,
                                       RELATION.EDGES.INTRA.COCHANGE)

## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Plot functions ----------------------------------------------------------

#' Construct a ggplot2/ggraph plot object for the given network and print it directly.
#'
#' As a layout, by default, \code{igraph::layout.kamada.kawai} (also known as \code{igraph::layout_with_kk})
#' is used, unless a graph attribute "layout" is set. For a comprehensive list of layouts and more information
#' on layouts in general, see \link{http://igraph.org/r/doc/layout_.html}.
#' To set the graph attribute on your network, run the following code while replacing \code{layout.to.set}
#' to your liking: \code{network = igraph::set.graph.attribute(network, "layout", layout.to.set)}.
#'
#' Note: The names for the vertex types are taken from the variables \code{PLOT.VERTEX.TYPE.AUTHOR} and
#' \code{PLOT.VERTEX.TYPE.ARTIFACT}. The defaults are \code{"Developer"} and \code{TYPE.ARTIFACT}, respectively.
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
#' As a layout, by default, \code{igraph::layout.kamada.kawai} (also known as \code{igraph::layout_with_kk})
#' is used, unless a graph attribute "layout" is set. For a comprehensive list of layouts and more information
#' on layouts in general, see \link{http://igraph.org/r/doc/layout_.html}.
#' To set the graph attribute on your network, run the following code while replacing \code{layout.to.set}
#' to your liking: \code{network = igraph::set.graph.attribute(network, "layout", layout.to.set)}.
#'
#' Note: The names for the vertex types are taken from the variables \code{PLOT.VERTEX.TYPE.AUTHOR} and
#' \code{PLOT.VERTEX.TYPE.ARTIFACT}. The defaults are \code{"Developer"} and \code{TYPE.ARTIFACT}, respectively.
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
#' As a layout, by default, \code{igraph::layout.kamada.kawai} (also known as \code{igraph::layout_with_kk})
#' is used, unless a graph attribute "layout" is set. For a comprehensive list of layouts and more information
#' on layouts in general, see \link{http://igraph.org/r/doc/layout_.html}.
#' To set the graph attribute on your network, run the following code while replacing \code{layout.to.set}
#' to your liking: \code{network = igraph::set.graph.attribute(network, "layout", layout.to.set)}.
#'
#' Note: The names for the vertex types are taken from the variables \code{PLOT.VERTEX.TYPE.AUTHOR} and
#' \code{PLOT.VERTEX.TYPE.ARTIFACT}. The defaults are \code{"Developer"} and \code{TYPE.ARTIFACT}, respectively.
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
        colors.edge.relation = PLOT.COLORS.BY.RELATION.EDGE.GRAY
        colors.vertex.label = "white"
    }
    ## set colors for colored
    else {
        colors.vertex = PLOT.COLORS.BY.TYPE.VERTEX
        colors.edge = PLOT.COLORS.BY.TYPE.EDGE
        colors.edge.relation = PLOT.COLORS.BY.RELATION.EDGE
        colors.vertex.label = "black"
    }

    ## check if network is empty
    if (igraph::vcount(network) == 0) {
        network = create.empty.network(directed = igraph::is.directed(network)) +
            igraph::vertices(c("", ""), type = c(TYPE.AUTHOR, TYPE.ARTIFACT))
        PLOT.VERTEX.SIZE = 0
    }

    ## properly set vertex-type names for legend
    PLOT.VERTEX.TYPES = c(PLOT.VERTEX.TYPE.AUTHOR, PLOT.VERTEX.TYPE.ARTIFACT)
    names(PLOT.VERTEX.TYPES) = c(TYPE.AUTHOR, TYPE.ARTIFACT)

    ## fix the type attributes (add new ones, also named)
    network = plot.fix.type.attributes(network)

    ## set network layout
    if (!("layout" %in% igraph::list.graph.attributes(network))) {
        network = igraph::set.graph.attribute(network, "layout", igraph::layout.kamada.kawai)
    }

    ## create a ggraph object
    p = ggraph::ggraph(network)

    ## plot edges if there are any
    if (igraph::ecount(network) > 0) {
        p = p +
            ggraph::geom_edge_fan(
                mapping = ggplot2::aes(colour = relation, linetype = edge.type),
                end_cap = ggraph::circle(PLOT.VERTEX.SIZE + 3, "pt"),
                start_cap = ggraph::circle(PLOT.VERTEX.SIZE + 3, "pt"),
                arrow = if (igraph::is.directed(network)) {
                        ggplot2::arrow(length = ggplot2::unit(PLOT.VERTEX.SIZE / 2, 'pt'), ends = "last", type = "closed")
                    } else {
                        NULL
                    }
            )
    }

    ## construct plot with proper colors and shapes everywhere
    p = p +

        ## plot vertices
        ggraph::geom_node_point(ggplot2::aes(color = vertex.type, shape = vertex.type), size = PLOT.VERTEX.SIZE) +
        ggraph::geom_node_text(ggplot2::aes(label = if (labels) name else c("")), size = 3.5, color = colors.vertex.label) +

        ## scale vertices (colors and styles)
        ggplot2::scale_shape_manual("Vertices", values = PLOT.SHAPE.VERTEX, labels = PLOT.VERTEX.TYPES) +
        ggplot2::scale_color_manual("Vertices", values = colors.vertex, labels = PLOT.VERTEX.TYPES) +

        ## scale edges (colors and styles)
        ggraph::scale_edge_linetype_manual("Relation Types", values = PLOT.SHAPE.EDGE) +
        ggraph::scale_edge_colour_manual("Relations", values = colors.edge.relation) +

        ## theme
        ggplot2::theme_light() +
        ggplot2::guides(
            ## reduce size of symbols in legend
            shape = ggplot2::guide_legend(override.aes = list(size = PLOT.VERTEX.SIZE.LEGEND))
        ) +
        ggplot2::theme(
            legend.position = "bottom",
            axis.line = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_blank(),
            axis.text.y = ggplot2::element_blank(),
            axis.ticks = ggplot2::element_blank(),
            axis.title.x = ggplot2::element_blank(),
            axis.title.y = ggplot2::element_blank(),
            panel.background = ggplot2::element_blank(),
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            plot.background = ggplot2::element_blank()
        )

    return(p)
}


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Low-level functionality -------------------------------------------------

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
#' - vertex.type = a copy of the old vertex attribute 'type', and
#' - edge.type = a copy of the old edge attribute 'type'.
#'
#' @param network the igraph object to augment
#' @param colors.vertex a vector of length 2, the entries named with the values of 'TYPE.AUTHOR' and 'TYPE.ARTIFACT'
#'                      [default: PLOT.COLORS.BY.TYPE.VERTEX]
#' @param colors.edge a vector of length 2, the entries named with the values of 'TYPE.EDGES.INTER' and 'TYPE.EDGES.INTRA'
#'                    [default: PLOT.COLORS.BY.TYPE.EDGE]
#' @param colors.edge.relation a vector of length 3, the entries named with 'RELATION.EDGES.INTRA.MAIL', 'RELATION.EDGES.INTRA.ISSUE'
#'                    and 'RELATION.COLORS.BY.RELATION.EDGE'
#'                    [default: PLOT.COLORS.BY.RELATION.EDGE]
#'
#' @return the old network with the new and changed vertex and edge attributes
plot.fix.type.attributes = function(network) {
    ## copy type attribute to vertex.type and edge.type
    network = igraph::set.vertex.attribute(network, "vertex.type", value = igraph::get.vertex.attribute(network, "type"))
    network = igraph::set.edge.attribute(network, "edge.type", value = igraph::get.edge.attribute(network, "type"))

    ## adjust 'type' attribute for vertices for bipartite plotting (we need Booleans there)
    types = igraph::get.vertex.attribute(network, "type")
    network = igraph::remove.vertex.attribute(network, "type")
    network = igraph::set.vertex.attribute(network, "type", value = sapply(
        types, function(t) return(t == TYPE.ARTIFACT)
    ))

    return(network)
}
