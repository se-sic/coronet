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
## Copyright 2017-2018, 2020 by Claus Hunsen <hunsen@fim.uni-passau.de>
## Copyright 2018 by Barbara Eckl <ecklbarb@fim.uni-passau.de>
## Copyright 2018 by Thomas Bock <bockthom@fim.uni-passau.de>
## Copyright 2020-2021, 2025 by Thomas Bock <bockthom@cs.uni-saarland.de>
## Copyright 2024 by Maximilian Löffler <s8maloef@stud.uni-saarland.de>
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

## vertex-label color
PLOT.VERTEX.LABEL.COLOR = "gray60"


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Plot functions ----------------------------------------------------------

#' Construct a ggplot2/ggraph plot object for the given network and print it directly.
#'
#' As a layout, by default, the "kk" layout from igraph (also known as "layout_kamada_kawai") is used,
#' is used, unless a graph attribute "layout" is set. For a comprehensive list of layouts and more information
#' on layouts in general, see \link{https://igraph.org/python/doc/tutorial/tutorial.html#layout-algorithms}.
#' To set the graph attribute on your network, run the following code while replacing \code{layout.to.set}
#' to your liking: \code{network = igraph::set_graph_attr(network, "layout", layout.to.set)}.
#' Note that \code{layout.to.set} refers to one of the "short names" of the recpective igraph layout, as
#' specified on the Web site in the link given above.
#'
#' Note: The names for the vertex types are taken from the variables \code{PLOT.VERTEX.TYPE.AUTHOR} and
#' \code{PLOT.VERTEX.TYPE.ARTIFACT}. The defaults are \code{"Developer"} and \code{TYPE.ARTIFACT}, respectively.
#' All loops are deleted for plotting the network.
#'
#' @param network the network to plot and print
#' @param labels logical indicating whether vertex lables should be plotted [default: TRUE]
#'
#' @return the network (invisibly)
#'
#' @aliases plot.print.network
plot.network = function(network, labels = TRUE) {
    plot.print.network(network, labels = labels)
}

#' Construct a ggplot2/ggraph plot object for the given network and print it directly.
#'
#' As a layout, by default, the "kk" layout from igraph (also known as "layout_kamada_kawai") is used,
#' is used, unless a graph attribute "layout" is set. For a comprehensive list of layouts and more information
#' on layouts in general, see \link{https://igraph.org/python/doc/tutorial/tutorial.html#layout-algorithms}.
#' To set the graph attribute on your network, run the following code while replacing \code{layout.to.set}
#' to your liking: \code{network = igraph::set_graph_attr(network, "layout", layout.to.set)}.
#' Note that \code{layout.to.set} refers to one of the "short names" of the recpective igraph layout, as
#' specified on the Web site in the link given above.
#'
#' Note: The names for the vertex types are taken from the variables \code{PLOT.VERTEX.TYPE.AUTHOR} and
#' \code{PLOT.VERTEX.TYPE.ARTIFACT}. The defaults are \code{"Developer"} and \code{TYPE.ARTIFACT}, respectively.
#' All loops are deleted for plotting the network.
#'
#' @param network the network to plot and print
#' @param labels logical indicating whether vertex lables should be plotted [default: TRUE]
#'
#' @return the network (invisibly)
#'
#' @aliases plot.network
plot.print.network = function(network, labels = TRUE) {
    p = plot.get.plot.for.network(network, labels = labels)
    print(p)
}

#' Construct a ggplot2/ggraph plot object for the given network.
#'
#' As a layout, by default, the "kk" layout from igraph (also known as "layout_kamada_kawai") is used,
#' is used, unless a graph attribute "layout" is set. For a comprehensive list of layouts and more information
#' on layouts in general, see \link{https://igraph.org/python/doc/tutorial/tutorial.html#layout-algorithms}.
#' To set the graph attribute on your network, run the following code while replacing \code{layout.to.set}
#' to your liking: \code{network = igraph::set_graph_attr(network, "layout", layout.to.set)}.
#' Note that \code{layout.to.set} refers to one of the "short names" of the recpective igraph layout, as
#' specified on the Web site in the link given above.
#'
#' Note: The names for the vertex types are taken from the variables \code{PLOT.VERTEX.TYPE.AUTHOR} and
#' \code{PLOT.VERTEX.TYPE.ARTIFACT}. The defaults are \code{"Developer"} and \code{TYPE.ARTIFACT}, respectively.
#' All loops are deleted for plotting the network.
#'
#' @param network the network to plot
#' @param labels logical indicating whether vertex lables should be plotted [default: TRUE]
#'
#' @return a ggplot2/ggraph plot object
plot.get.plot.for.network = function(network, labels = TRUE) {
    ## check if network is empty
    if (igraph::vcount(network) == 0) {
        network = create.empty.network(directed = igraph::is_directed(network), add.attributes = TRUE)
        PLOT.VERTEX.SIZE = 0
    }

    ## properly set vertex-type names for legend
    PLOT.VERTEX.TYPES = c(PLOT.VERTEX.TYPE.AUTHOR, PLOT.VERTEX.TYPE.ARTIFACT)
    names(PLOT.VERTEX.TYPES) = c(TYPE.AUTHOR, TYPE.ARTIFACT)

    ## remove loops because of weird behavior when plotting
    network = igraph::delete_edges(network, igraph::E(network)[igraph::which_loop(network)])

    ## fix the type attributes (add new ones, also named)
    network = plot.fix.type.attributes(network)

    ## set igraph network layout if no layout is set yet
    if (!("layout" %in% igraph::graph_attr_names(network))) {
        network = igraph::set_graph_attr(network, "layout", "kk")
    }
    layout.algorithm = igraph::graph_attr(network, "layout")

    ## create a ggraph object using the specified igraph layout
    p = ggraph::ggraph(network, layout = layout.algorithm)

    ## plot edges if there are any
    if (igraph::ecount(network) > 0) {
        p = p +
            ggraph::geom_edge_fan(
                mapping = ggplot2::aes(colour = paste(relation, sep = " "), linetype = edge.type, width = 0.3 + 0.5 * log(weight)),
                end_cap = ggraph::circle(PLOT.VERTEX.SIZE + 3, "pt"),
                start_cap = ggraph::circle(PLOT.VERTEX.SIZE + 3, "pt"),
                arrow = if (igraph::is_directed(network)) {
                        ggplot2::arrow(length = ggplot2::unit(PLOT.VERTEX.SIZE / 2, 'pt'), ends = "last", type = "closed")
                    } else {
                        NULL
                    }
            )
    }

    ## construct plot with proper colors and shapes everywhere
    p = p +

        ## plot vertices
        ggraph::geom_node_point(ggplot2::aes(color = kind, shape = vertex.type), size = PLOT.VERTEX.SIZE) +
        ggraph::geom_node_text(ggplot2::aes(label = if (labels) name else c("")), size = 3.5, color = PLOT.VERTEX.LABEL.COLOR) +

        ## scale vertices (colors and styles)
        ggplot2::scale_shape_discrete(name = "Vertex Types", solid = TRUE) +
        viridis::scale_color_viridis(name = "Vertices", option = "plasma", discrete = TRUE,
                                     end = 0.8, begin = 0.05) +

        ## scale edges (colors and styles)
        ggraph::scale_edge_linetype(name = "Relation Types") +
        ggplot2::discrete_scale(name = "Relations", aesthetics = "edge_colour",
                                palette = viridis::viridis_pal(option = "viridis", end = 0.8, begin = 0.25)) +
        ## BROKEN RIGHT NOW due to bug in scale_edge_colour_viridis():
        # ggraph::scale_edge_colour_viridis(name = "Relations", option = "magma", discrete = TRUE,
        #                                   end = 0.85, begin = 0, direction = 1) +

        ## theme
        ggplot2::theme_light() +
        ggplot2::guides(
            ## reduce size of symbols in legend
            shape = ggplot2::guide_legend(override.aes = list(size = PLOT.VERTEX.SIZE.LEGEND)),
            color = ggplot2::guide_legend(override.aes = list(size = PLOT.VERTEX.SIZE.LEGEND, shape = 15))
        ) +
        ggplot2::theme(
            legend.position = "bottom",
            legend.box = "horizontal", # orientation for complete legend
            legend.direction = "vertical", # orientation for each sublegend/guide
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
#' - vertex.type = a copy of the old vertex attribute 'type', and
#' - edge.type = a copy of the old edge attribute 'type'.
#'
#' @param network the igraph object to augment
#'
#' @return the old network with the new and changed vertex and edge attributes
plot.fix.type.attributes = function(network) {
    ## copy type attribute to vertex.type and edge.type
    if (igraph::vcount(network) == 0) {
        network = igraph::set_vertex_attr(network, "vertex.type", value = NA)
    } else {
        network = igraph::set_vertex_attr(network, "vertex.type", value = igraph::vertex_attr(network, "type"))
    }
    network = igraph::set_edge_attr(network, "edge.type", value = igraph::edge_attr(network, "type"))

    ## adjust 'type' attribute for vertices for bipartite plotting (we need Booleans there)
    types = igraph::vertex_attr(network, "type")
    network = igraph::delete_vertex_attr(network, "type")
    network = igraph::set_vertex_attr(network, "type", value = sapply(
        types, function(t) return(t == TYPE.ARTIFACT)
    ))

    return(network)
}
