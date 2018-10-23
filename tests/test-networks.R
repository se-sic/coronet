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
## Copyright 2018 by Claus Hunsen <hunsen@fim.uni-passau.de>
## All Rights Reserved.


context("Basic network-building functionality.")

##
## Context
##

CF.DATA = file.path(".", "codeface-data")
CF.SELECTION.PROCESS = "testing"
CASESTUDY = "test"
ARTIFACT = "feature" # function, feature, file, featureexpression

## use only when debugging this file independently
if (!dir.exists(CF.DATA)) CF.DATA = file.path(".", "tests", "codeface-data")


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Simplification ----------------------------------------------------------

test_that("Simplify network with more than one relation", {

    ##
    ## Simplify networks with two relations, but only one loop edge
    ##

    ## create network configuration
    net.conf = NetworkConf$new()
    # net.conf$update.values(updated.values = list(author.relation = "mail", artifact.relation = "cochange"))

    ## create author network (relation = "mail") and artifact network and join them
    author.net =
        igraph::make_empty_graph(n = 0, directed = FALSE) +
        igraph::vertices("A", "B", "C", type = TYPE.AUTHOR, kind = TYPE.AUTHOR) +
        igraph::edges("A", "A", type = TYPE.EDGES.INTRA, relation = "mail")
    artifact.net =
        igraph::make_empty_graph(n = 0, directed = FALSE) +
        igraph::vertices("Base_Feature", type = TYPE.ARTIFACT, kind = "feature")
    base.net = igraph::disjoint_union(author.net, artifact.net)

    ## create bipartite relations between authors and artifacts (relation = "cochange")
    bip.relations = list(
        cochange =
            list(
                Base_Feature = data.frame(
                    data.vertices = c("A", "A"),
                    date = get.date.from.string(c("2010-01-01", "2010-01-02")),
                    artifact = "feature"
                )
            )
    )

    ## add bipartite relations to base network
    g = add.edges.for.bipartite.relation(base.net, bip.relations, net.conf)

    ## simplify the network without any errors (hopefully)
    expect_error(simplify.network(g), NA) # expect that no error occurs
    expect_identical(igraph::V(simplify.network(g))$name, c("A", "B", "C", "Base_Feature")) # vertices
    expect_identical(igraph::ecount(simplify.network(g)), 1) # edges
    expect_true(igraph::are.connected(simplify.network(g), "A", "Base_Feature")) # specific edge

})


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Merge -------------------------------------------------------------------

test_that("Merge networks", {

    ##
    ## Merge two empty networks (i.e., no vertices at all)
    ##

    network.empty = create.empty.network(directed = FALSE)
    expect_error(merge.networks(list(network.empty, network.empty)), NA) # expect that no error occurs
    expect_true(igraph::vcount(merge.networks(list(network.empty, network.empty))) == 0) # vertices
    expect_true(igraph::ecount(merge.networks(list(network.empty, network.empty))) == 0) # edges

})

