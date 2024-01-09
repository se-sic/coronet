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
## Copyright 2019 by Jakob Kronawitter <kronawij@fim.uni-passau.de>
## Copyright 2019 by Claus Hunsen <hunsen@fim.uni-passau.de>
## Copyright 2019 by Thomas Bock <bockthom@fim.uni-passau.de>
## Copyright 2022 by Thomas Bock <bockthom@cs.uni-saarland.de>
## Copyright 2019 by Christian Hechtl <hechtl@fim.uni-passau.de>
## Copyright 2021 by Christian Hechtl <hechtl@cs.uni-saarland.de>
## All Rights Reserved.


context("Tests for the file 'util-networks-misc.R'")

##
## Context
##

CF.DATA = file.path(".", "codeface-data")
CF.SELECTION.PROCESS = "testing"
CASESTUDY = "test"
ARTIFACT = "feature"

## use only when debugging this file independently
if (!dir.exists(CF.DATA)) CF.DATA = file.path(".", "tests", "codeface-data")

test_that("getting all authors of a list of networks, list length 0", {

    ## Act
    result = get.author.names.from.networks(networks = list(), globally = TRUE)
    
    ## Assert
    expected = list(c())
    
    expect_equal(expected, result)
})

test_that("getting all authors of a list of networks, list length 1", {

    ## Arrange
    vertices = data.frame(
        name = c("Heinz", "Dieter", "Klaus"),
        kind = TYPE.AUTHOR,
        type = TYPE.AUTHOR
        )
    edges = data.frame(
        from = "Heinz",
        to = "Dieter"
        )
    network = igraph::graph.data.frame(edges, directed = FALSE, vertices = vertices)

    ## Act
    result = get.author.names.from.networks(networks = list(network))

    ## Assert
    expected = list(c("Dieter", "Heinz", "Klaus"))
    
    expect_equal(expected, result)

})

test_that("getting all authors of a list of networks, list length 1, not global", {

    ## Arrange
    vertices = data.frame(
        name = c("Heinz", "Dieter", "Klaus"),
        kind = TYPE.AUTHOR,
        type = TYPE.AUTHOR
        )
    edges = data.frame(
        from = "Heinz",
        to = "Dieter"
        )
    network = igraph::graph.data.frame(edges, directed = FALSE, vertices = vertices)

    ## Act
    result = get.author.names.from.networks(networks = list(network), globally = FALSE)

    ## Assert
    expected = list(c("Dieter", "Heinz", "Klaus"))
    
    expect_equal(expected, result)

})

test_that("getting all authors of a list of networks, list length 2", {

    ## Arrange
    vertices = data.frame(
        name = c("Heinz", "Dieter", "Klaus"),
        kind = TYPE.AUTHOR,
        type = TYPE.AUTHOR
        )
    edges = data.frame(
        from = "Heinz",
        to = "Dieter"
        )
    first.network = igraph::graph.data.frame(edges, directed = FALSE, vertices = vertices)

    second.vertices = data.frame(
        name = c("Detlef", "Dieter"),
        kind = TYPE.AUTHOR,
        type = TYPE.AUTHOR
        )
    second.edges = data.frame(
        from = "Detlef",
        to = "Dieter"
        )
    second.network = igraph::graph.data.frame(second.edges, directed = FALSE, vertices = second.vertices)
    ## Act
    result = get.author.names.from.networks(networks = list(first.network, second.network))

    ## Assert
    expected = list(c("Detlef", "Dieter", "Heinz", "Klaus"))
    
    expect_equal(expected, result)
})

test_that("getting all authors of a list of networks, list length 2, not global", {

    ## Arrange
    vertices = data.frame(
        name = c("Heinz", "Dieter", "Klaus"),
        kind = TYPE.AUTHOR,
        type = TYPE.AUTHOR
        )
    edges = data.frame(
        from = "Heinz",
        to = "Dieter"
        )
    first.network = igraph::graph.data.frame(edges, directed = FALSE, vertices = vertices)

    second.vertices = data.frame(
        name = c("Detlef", "Dieter"),
        kind = TYPE.AUTHOR,
        type = TYPE.AUTHOR
        )
    second.edges = data.frame(
        from = "Detlef",
        to = "Dieter"
        )
    second.network = igraph::graph.data.frame(second.edges, directed = FALSE, vertices = second.vertices)
    ## Act
    result = get.author.names.from.networks(networks = list(first.network, second.network), globally = FALSE)

    ## Assert
    expected = list(c("Dieter", "Heinz", "Klaus"), c("Detlef", "Dieter"))
    
    expect_equal(expected, result)
})

test_that("getting all authors of a list of data ranges, list length 0", {

    ## Act
    result = get.author.names.from.data(data.ranges = list())
    
    ## Assert
    expected = list(c())
    
    expect_equal(expected, result)
})

test_that("getting all authors of a list of data ranges, list length 1", {

    ## Arrange
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.data.base = ProjectData$new(project.conf = proj.conf)
    range.data = proj.data.base$get.data.cut.to.same.date("mails")

    ## Act
    result = get.author.names.from.data(data.ranges = list(range.data))
    
    ## Assert
    expected = list(c("Björn", "Fritz fritz@example.org","georg", "Hans", 
                    "Karl", "Olaf", "Thomas", "udo"))
    
    expect_equal(expected, result)
})

test_that("getting all authors of a list of data ranges, list length 1, not global", {

    ## Arrange
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.data.base = ProjectData$new(project.conf = proj.conf)
    range.data = proj.data.base$get.data.cut.to.same.date("mails")

    ## Act
    result = get.author.names.from.data(data.ranges = list(range.data), globally = FALSE)
    
    ## Assert
    expected = list(c("Björn", "Fritz fritz@example.org","georg", "Hans", 
                    "Karl", "Olaf", "Thomas", "udo"))
    
    expect_equal(expected, result)
})

test_that("getting all authors of a list of data ranges, list length 2", {

    ## Arrange
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.data.base = ProjectData$new(project.conf = proj.conf)
    range.data.one = proj.data.base$get.data.cut.to.same.date("mails")
    range.data.two = proj.data.base$get.data.cut.to.same.date("issues")

    ## Act
    result = get.author.names.from.data(data.ranges = list(range.data.one, range.data.two))
    
    ## Assert
    expected = list(c("Björn", "Fritz fritz@example.org","georg", "Hans", 
                    "Karl", "Max", "Olaf", "Thomas", "udo"))
    
    expect_equal(expected, result)
})

test_that("getting all authors of a list of data ranges, list length 2, not global", {

    ## Arrange
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.data.base = ProjectData$new(project.conf = proj.conf)
    range.data.one = proj.data.base$get.data.cut.to.same.date("mails")
    range.data.two = proj.data.base$get.data.cut.to.same.date("issues")

    ## Act
    result = get.author.names.from.data(data.ranges = list(range.data.one, range.data.two), globally = FALSE)
    
    ## Assert
    expected = list(c("Björn", "Fritz fritz@example.org","georg", "Hans", "Karl", "Olaf",
                        "Thomas", "udo"), c("Björn", "Karl", "Max", "Olaf", "Thomas"))
    
    expect_equal(expected, result)
})

test_that("getting all authors of a list of data ranges by data source 'mails', list length 2, not global", {

    ## Arrange
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.data.base = ProjectData$new(project.conf = proj.conf)
    range.data.one = proj.data.base$get.data.cut.to.same.date("mails")
    range.data.two = proj.data.base$get.data.cut.to.same.date("issues")

    ## Act
    result = get.author.names.from.data(data.ranges = list(range.data.one, range.data.two), 
                                                            data.sources = "mails", globally = FALSE)
    
    ## Assert
    
    expected = list(c("Björn", "Fritz fritz@example.org","georg", "Hans", "Olaf",
                        "Thomas", "udo"), c("Björn", "Olaf", "Thomas"))
    
    expect_equal(expected, result)
})

test_that("getting all authors of a list of data ranges by data source 'issues', list length 2, not global", {

    ## Arrange
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.data.base = ProjectData$new(project.conf = proj.conf)
    range.data.one = proj.data.base$get.data.cut.to.same.date("mails")
    range.data.two = proj.data.base$get.data.cut.to.same.date("issues")

    ## Act
    result = get.author.names.from.data(data.ranges = list(range.data.one, range.data.two), 
                                                            data.sources = "issues", globally = FALSE)
    
    ## Assert
    expected = list(c("Björn", "Karl", "Olaf", "Thomas"), c("Björn","Karl", "Max", "Olaf", "Thomas"))
    
    expect_equal(expected, result)
})

test_that("getting all authors of a list of data ranges by data source 'commits', list length 2, not global", {

    ## Arrange
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.data.base = ProjectData$new(project.conf = proj.conf)
    range.data.one = proj.data.base$get.data.cut.to.same.date("mails")
    range.data.two = proj.data.base$get.data.cut.to.same.date("issues")

    ## Act
    result = get.author.names.from.data(data.ranges = list(range.data.one, range.data.two), 
                                                            data.sources = "commits", globally = FALSE)
    
    ## Assert
    
    expected = list(c("Björn", "Olaf"), c("Björn", "Olaf", "Thomas"))
    
    expect_equal(expected, result)
})