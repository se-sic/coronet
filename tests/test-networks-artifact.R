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
    vertices = c("Base_Feature", "foo", "A")

    data = data.frame(from = c("Base_Feature", "Base_Feature"),
                      to = c("foo", "foo"),
                      date = get.date.from.string(c("2016-07-12 16:06:32", "2016-07-12 16:06:32")),
                      hash = c("0a1a5c523d835459c42f33e863623138555e2526", "0a1a5c523d835459c42f33e863623138555e2526"),
                      file = c("test2.c", "test2.c"),
                      artifact.type = c("Feature", "Feature"),
                      artifact = c("Base_Feature", "foo"),
                      weight = c(1, 1),
                      type = TYPE.EDGES.INTRA)

    ## build expected network
    network.expected = igraph::graph.data.frame(data, directed = FALSE, vertices = vertices)
    network.expected = igraph::set.vertex.attribute(network.expected, "id", value = igraph::get.vertex.attribute(network.expected, "name"))
    igraph::V(network.expected)$type = TYPE.ARTIFACT

    expect_true(igraph::identical_graphs(network.built, network.expected))
})
