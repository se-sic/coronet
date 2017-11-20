## (c) Claus Hunsen, 2017
## hunsen@fim.uni-passau.de
## (c) Christian Hechtl, 2017
## hechtl@fim.uni-passau.de


context("Network-building functionality.")

##
## Context
##

CF.DATA = file.path(".", "codeface-data")
CF.SELECTION.PROCESS = "testing"
CASESTUDY = "test"
ARTIFACT = "feature" # function, feature, file, featureexpression

## use only when debugging this file independently
if (!dir.exists(CF.DATA)) CF.DATA = file.path(".", "tests", "codeface-data")


test_that("Network construction of the undirected artifact-cochange network", {

    ## configurations
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("artifact.filter.base", FALSE)
    net.conf = NetworkConf$new()
    net.conf$update.values(updated.values = list(artifact.relation = "cochange"))

    ## construct objects
    proj.data = ProjectData$new(project.conf = proj.conf)
    network.builder = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf)

    ## build network
    network.built = network.builder$get.artifact.network()

    ## vertex attributes
    vertices = c("Base_Feature", "A")

    ## build expected network
    network.expected = igraph::graph.empty(n = 0, directed = FALSE) + igraph::vertices(vertices)
    network.expected = igraph::set.vertex.attribute(network.expected, "id", value = igraph::get.vertex.attribute(network.expected, "name"))
    network.expected = igraph::set.edge.attribute(network.expected, "weight", value = 1)
    igraph::V(network.expected)$type = TYPE.ARTIFACT
    igraph::E(network.expected)$type = TYPE.EDGES.INTRA

    expect_true(igraph::identical_graphs(network.built, network.expected))
})
