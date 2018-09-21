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


test_that("Merge networks", {

    ##
    ## Simplify networks with two relations, but only one loop edge
    ## (i.e., merge two networks without any edges)
    ##

    ## create network configuration
    net.conf = NetworkConf$new()
    # net.conf$update.values(updated.values = list(author.relation = "mail", artifact.relation = "cochange"))

    ## create author network (relation = "mail") and artifact network and join them
    author.net =
        igraph::make_empty_graph(n = 0, directed = FALSE) +
        igraph::vertices(LETTERS[1:3], type = TYPE.AUTHOR, kind = TYPE.AUTHOR) +
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


    ##
    ## Merge two empty networks (i.e., no vertices at all)
    ##

    network.empty = create.empty.network(directed = FALSE)
    expect_error(merge.networks(list(network.empty, network.empty)), NA) # expect that no error occurs

})

