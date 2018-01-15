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

test_that("Construction of the bipartite network for the feature artifact with author.reltion = 'cochange' and artifact.
          relation = 'cochange'.",{

    ## configurations
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("artifact.filter.base", FALSE)
    net.conf = NetworkConf$new()
    net.conf$update.values(updated.values = list(author.relation = "cochange", artifact.relation = "cochange"))

    ## construct objects
    proj.data = ProjectData$new(project.conf = proj.conf)
    network.builder = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf)

    ## build network
    network.built = network.builder$get.bipartite.network()

    authors = c("Claus Hunsen", "Karl", "Olaf", "Thomas")
    author.net = create.empty.network(directed = FALSE) +
        igraph::vertex(authors, name  = authors, type = TYPE.AUTHOR)

    artifact.vertices = c("A", "Base_Feature", "foo")
    artifact.net = create.empty.network(directed = FALSE) +
        igraph::vertices(artifact.vertices, name = artifact.vertices, type = TYPE.ARTIFACT, artifact.type = "feature")

    network.expected = igraph::disjoint_union(author.net, artifact.net)

    vertex.sequence.for.edges = c(1,5,2,6,3,5,3,6,4,6,4,7)
    names(vertex.sequence.for.edges) = c("Claus Hunsen.Claus Hunsen", "Claus Hunsen.A", "Karl.Karl", "Karl.Base_Feature",
                                         "Olaf.Olaf", "Olaf.A", "Olaf.Olaf", "Olaf.Base_Feature", "Thomas.Thomas",
                                         "Thomas.Base_Feature", "Thomas.Thomas", "Thomas.foo")

    extra.edge.attributes = list(date = as.POSIXct(c("2016-07-12 15:58:59", "2016-07-12 16:06:10", "2016-07-12 16:00:45",
                                                     "2016-07-12 16:05:41", "2016-07-12 16:06:32", "2016-07-12 16:06:32")),
                                 hash = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0", "1143db502761379c2bfcecc2007fc34282e7ee61",
                                          "5a5ec9675e98187e1e92561e1888aa6f04faa338", "3a0ed78458b3976243db6829f63eba3eead26774",
                                          "0a1a5c523d835459c42f33e863623138555e2526", "0a1a5c523d835459c42f33e863623138555e2526"),
                                 file = c("test.c", "test3.c", "test.c", "test2.c", "test2.c", "test2.c"),
                                 artifact.type = c("Feature", "Feature", "Feature", "Feature", "Feature", "Feature"),
                                 artifact = c("A", "Base_Feature", "A", "Base_Feature", "Base_Feature", "foo"),
                                 weight = c(1, 1, 1, 1, 1, 1),
                                 type = c(4))

    network.expected = igraph::add_edges(network.expected, vertex.sequence.for.edges, attr = extra.edge.attributes)

    expect_true(igraph::identical_graphs(network.built, network.expected))
})


test_that("Construction of the bipartite network for the file artifact with author.reltion = 'cochange' and artifact.
          relation = 'cochange'.",{

              ## configurations
              proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, "file")
              proj.conf$update.value("artifact.filter.base", FALSE)
              net.conf = NetworkConf$new()
              net.conf$update.values(updated.values = list(author.relation = "cochange", artifact.relation = "cochange"))

              ## construct objects
              proj.data = ProjectData$new(project.conf = proj.conf)
              network.builder = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf)

              ## build network
              network.built = network.builder$get.bipartite.network()

              authors = c("Claus Hunsen", "Olaf", "Thomas")
              author.net = create.empty.network(directed = FALSE) +
                  igraph::vertex(authors, name  = authors, type = TYPE.AUTHOR)

              artifact.vertices = c("test.c", "test2.c")
              artifact.net = create.empty.network(directed = FALSE) +
                  igraph::vertices(artifact.vertices, name = artifact.vertices, type = TYPE.ARTIFACT, artifact.type = "file")

              network.expected = igraph::disjoint_union(author.net, artifact.net)

              vertex.sequence.for.edges = c(1,4,2,4,2,5,3,5)
              names(vertex.sequence.for.edges) = c("Claus Hunsen.Claus Hunsen", "Claus Hunsen.test.c", "Olaf.Olaf", "Olaf.test.c",
                                                   "Olaf.Olaf", "Olaf.test2.c", "Thomas.Thomas", "Thomas.test2.c")

              extra.edge.attributes = list(date = as.POSIXct(c("2016-07-12 15:58:59", "2016-07-12 16:00:45",
                                                               "2016-07-12 16:05:41", "2016-07-12 16:06:32")),
                                           hash = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0", "5a5ec9675e98187e1e92561e1888aa6f04faa338",
                                                    "3a0ed78458b3976243db6829f63eba3eead26774", "0a1a5c523d835459c42f33e863623138555e2526"),
                                           file = c("test.c", "test.c", "test2.c", "test2.c"),
                                           artifact.type = c("File", "File", "File", "File"),
                                           artifact = c("test.c", "test.c", "test2.c", "test2.c"),
                                           weight = c(1, 1, 1, 1),
                                           type = c(4))

              network.expected = igraph::add_edges(network.expected, vertex.sequence.for.edges, attr = extra.edge.attributes)

              expect_true(igraph::identical_graphs(network.built, network.expected))
          })


test_that("Construction of the bipartite network for the function artifact with author.reltion = 'cochange' and artifact.
          relation = 'cochange'.",{

              ## configurations
              proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, "function")
              proj.conf$update.value("artifact.filter.base", FALSE)
              net.conf = NetworkConf$new()
              net.conf$update.values(updated.values = list(author.relation = "cochange", artifact.relation = "cochange"))

              ## construct objects
              proj.data = ProjectData$new(project.conf = proj.conf)
              network.builder = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf)

              ## build network
              network.built = network.builder$get.bipartite.network()

              authors = c("Claus Hunsen", "Olaf", "Thomas")
              author.net = create.empty.network(directed = FALSE) +
                  igraph::vertex(authors, name  = authors, type = TYPE.AUTHOR)

              artifact.vertices = c("File_Level")
              artifact.net = create.empty.network(directed = FALSE) +
                  igraph::vertices(artifact.vertices, name = artifact.vertices, type = TYPE.ARTIFACT, artifact.type = "function")

              network.expected = igraph::disjoint_union(author.net, artifact.net)

              vertex.sequence.for.edges = c(1,4,2,4,2,4,3,4)
              names(vertex.sequence.for.edges) = c("Claus Hunsen.Claus Hunsen", "Claus Hunsen.File_Level", "Olaf.Olaf", "Olaf.File_Level",
                                                   "Olaf.Olaf", "Olaf.File_Level", "Thomas.Thomas", "Thomas.File_Level")

              extra.edge.attributes = list(date = as.POSIXct(c("2016-07-12 15:58:59", "2016-07-12 16:00:45",
                                                               "2016-07-12 16:05:41", "2016-07-12 16:06:32")),
                                           hash = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0", "5a5ec9675e98187e1e92561e1888aa6f04faa338",
                                                    "3a0ed78458b3976243db6829f63eba3eead26774", "0a1a5c523d835459c42f33e863623138555e2526"),
                                           file = c("test.c", "test.c", "test2.c", "test2.c"),
                                           artifact.type = c("Function", "Function", "Function", "Function"),
                                           artifact = c("File_Level", "File_Level", "File_Level", "File_Level"),
                                           weight = c(1, 1, 1, 1),
                                           type = c(4))

              network.expected = igraph::add_edges(network.expected, vertex.sequence.for.edges, attr = extra.edge.attributes)

              expect_true(igraph::identical_graphs(network.built, network.expected))
          })

test_that("Construction of the bipartite network for the featureexpression artifact with author.reltion = 'cochange' and artifact.
          relation = 'cochange'.",{

              ## configurations
              proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, "featureexpression")
              proj.conf$update.value("artifact.filter.base", FALSE)
              net.conf = NetworkConf$new()
              net.conf$update.values(updated.values = list(author.relation = "cochange", artifact.relation = "cochange"))

              ## construct objects
              proj.data = ProjectData$new(project.conf = proj.conf)
              network.builder = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf)

              ## build network
              network.built = network.builder$get.bipartite.network()

              authors = c("Claus Hunsen", "Olaf")
              author.net = create.empty.network(directed = FALSE) +
                  igraph::vertex(authors, name  = authors, type = TYPE.AUTHOR)

              artifact.vertices = c("defined(A)", "Base_Feature")
              artifact.net = create.empty.network(directed = FALSE) +
                  igraph::vertices(artifact.vertices, name = artifact.vertices, type = TYPE.ARTIFACT, artifact.type = "featureexpression")

              network.expected = igraph::disjoint_union(author.net, artifact.net)

              vertex.sequence.for.edges = c(1,3,2,3,2,4)
              names(vertex.sequence.for.edges) = c("Claus Hunsen.Claus Hunsen", "Claus Hunsen.defined(A)", "Olaf.Olaf", "Olaf.defined(A)",
                                                   "Olaf.Olaf", "Olaf.Base_Feature")

              extra.edge.attributes = list(date = as.POSIXct(c("2016-07-12 15:58:59", "2016-07-12 16:00:45",
                                                               "2016-07-12 16:05:41")),
                                           hash = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0", "5a5ec9675e98187e1e92561e1888aa6f04faa338",
                                                    "3a0ed78458b3976243db6829f63eba3eead26774"),
                                           file = c("test.c", "test.c", "test2.c"),
                                           artifact.type = c("FeatureExpression", "FeatureExpression", "FeatureExpression"),
                                           artifact = c("defined(A)", "defined(A)", "Base_Feature"),
                                           weight = c(1, 1, 1),
                                           type = c(4))

              network.expected = igraph::add_edges(network.expected, vertex.sequence.for.edges, attr = extra.edge.attributes)

              expect_true(igraph::identical_graphs(network.built, network.expected))
          })

test_that("Construction of the directed bipartite network for the feature artifact with author.reltion = 'cochange' and artifact.
          relation = 'cochange'.",{

              ## configurations
              proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
              proj.conf$update.value("artifact.filter.base", FALSE)
              net.conf = NetworkConf$new()
              net.conf$update.values(updated.values = list(author.relation = "cochange", artifact.relation = "cochange",
                                                           author.directed = TRUE))

              ## construct objects
              proj.data = ProjectData$new(project.conf = proj.conf)
              network.builder = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf)

              ## build network
              network.built = network.builder$get.bipartite.network()

              authors = c("Claus Hunsen", "Karl", "Olaf", "Thomas")
              author.net = create.empty.network(directed = TRUE) +
                  igraph::vertex(authors, name  = authors, type = TYPE.AUTHOR)

              artifact.vertices = c("A", "Base_Feature", "foo")
              artifact.net = create.empty.network(directed = TRUE) +
                  igraph::vertices(artifact.vertices, name = artifact.vertices, type = TYPE.ARTIFACT, artifact.type = "feature")

              network.expected = igraph::disjoint_union(author.net, artifact.net)

              vertex.sequence.for.edges = c(1,5,2,6,3,5,3,6,4,6,4,7)
              names(vertex.sequence.for.edges) = c("Claus Hunsen.Claus Hunsen", "Claus Hunsen.A", "Karl.Karl", "Karl.Base_Feature",
                                                   "Olaf.Olaf", "Olaf.A", "Olaf.Olaf", "Olaf.Base_Feature", "Thomas.Thomas",
                                                   "Thomas.Base_Feature", "Thomas.Thomas", "Thomas.foo")

              extra.edge.attributes = list(date = as.POSIXct(c("2016-07-12 15:58:59", "2016-07-12 16:06:10", "2016-07-12 16:00:45",
                                                               "2016-07-12 16:05:41", "2016-07-12 16:06:32", "2016-07-12 16:06:32")),
                                           hash = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0", "1143db502761379c2bfcecc2007fc34282e7ee61",
                                                    "5a5ec9675e98187e1e92561e1888aa6f04faa338", "3a0ed78458b3976243db6829f63eba3eead26774",
                                                    "0a1a5c523d835459c42f33e863623138555e2526", "0a1a5c523d835459c42f33e863623138555e2526"),
                                           file = c("test.c", "test3.c", "test.c", "test2.c", "test2.c", "test2.c"),
                                           artifact.type = c("Feature", "Feature", "Feature", "Feature", "Feature", "Feature"),
                                           artifact = c("A", "Base_Feature", "A", "Base_Feature", "Base_Feature", "foo"),
                                           weight = c(1, 1, 1, 1, 1, 1),
                                           type = c(4))

              network.expected = igraph::add_edges(network.expected, vertex.sequence.for.edges, attr = extra.edge.attributes)

              expect_true(igraph::identical_graphs(network.built, network.expected))
          })

test_that("Construction of the directed bipartite network for the file artifact with author.reltion = 'cochange' and artifact.
          relation = 'cochange'.",{

              ## configurations
              proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, "file")
              proj.conf$update.value("artifact.filter.base", FALSE)
              net.conf = NetworkConf$new()
              net.conf$update.values(updated.values = list(author.relation = "cochange", artifact.relation = "cochange",
                                                           author.directed = TRUE))

              ## construct objects
              proj.data = ProjectData$new(project.conf = proj.conf)
              network.builder = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf)

              ## build network
              network.built = network.builder$get.bipartite.network()

              authors = c("Claus Hunsen", "Olaf", "Thomas")
              author.net = create.empty.network(directed = TRUE) +
                  igraph::vertex(authors, name  = authors, type = TYPE.AUTHOR)

              artifact.vertices = c("test.c", "test2.c")
              artifact.net = create.empty.network(directed = TRUE) +
                  igraph::vertices(artifact.vertices, name = artifact.vertices, type = TYPE.ARTIFACT, artifact.type = "file")

              network.expected = igraph::disjoint_union(author.net, artifact.net)

              vertex.sequence.for.edges = c(1,4,2,4,2,5,3,5)
              names(vertex.sequence.for.edges) = c("Claus Hunsen.Claus Hunsen", "Claus Hunsen.test.c", "Olaf.Olaf", "Olaf.test.c",
                                                   "Olaf.Olaf", "Olaf.test2.c", "Thomas.Thomas", "Thomas.test2.c")

              extra.edge.attributes = list(date = as.POSIXct(c("2016-07-12 15:58:59", "2016-07-12 16:00:45",
                                                               "2016-07-12 16:05:41", "2016-07-12 16:06:32")),
                                           hash = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0", "5a5ec9675e98187e1e92561e1888aa6f04faa338",
                                                    "3a0ed78458b3976243db6829f63eba3eead26774", "0a1a5c523d835459c42f33e863623138555e2526"),
                                           file = c("test.c", "test.c", "test2.c", "test2.c"),
                                           artifact.type = c("File", "File", "File", "File"),
                                           artifact = c("test.c", "test.c", "test2.c", "test2.c"),
                                           weight = c(1, 1, 1, 1),
                                           type = c(4))

              network.expected = igraph::add_edges(network.expected, vertex.sequence.for.edges, attr = extra.edge.attributes)

              expect_true(igraph::identical_graphs(network.built, network.expected))
          })


test_that("Construction of the directed bipartite network for the function artifact with author.reltion = 'cochange' and artifact.
          relation = 'cochange'.",{

              ## configurations
              proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, "function")
              proj.conf$update.value("artifact.filter.base", FALSE)
              net.conf = NetworkConf$new()
              net.conf$update.values(updated.values = list(author.relation = "cochange", artifact.relation = "cochange",
                                                           author.directed = TRUE))

              ## construct objects
              proj.data = ProjectData$new(project.conf = proj.conf)
              network.builder = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf)

              ## build network
              network.built = network.builder$get.bipartite.network()

              authors = c("Claus Hunsen", "Olaf", "Thomas")
              author.net = create.empty.network(directed = TRUE) +
                  igraph::vertex(authors, name  = authors, type = TYPE.AUTHOR)

              artifact.vertices = c("File_Level")
              artifact.net = create.empty.network(directed = TRUE) +
                  igraph::vertices(artifact.vertices, name = artifact.vertices, type = TYPE.ARTIFACT, artifact.type = "function")

              network.expected = igraph::disjoint_union(author.net, artifact.net)

              vertex.sequence.for.edges = c(1,4,2,4,2,4,3,4)
              names(vertex.sequence.for.edges) = c("Claus Hunsen.Claus Hunsen", "Claus Hunsen.File_Level", "Olaf.Olaf", "Olaf.File_Level",
                                                   "Olaf.Olaf", "Olaf.File_Level", "Thomas.Thomas", "Thomas.File_Level")

              extra.edge.attributes = list(date = as.POSIXct(c("2016-07-12 15:58:59", "2016-07-12 16:00:45",
                                                               "2016-07-12 16:05:41", "2016-07-12 16:06:32")),
                                           hash = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0", "5a5ec9675e98187e1e92561e1888aa6f04faa338",
                                                    "3a0ed78458b3976243db6829f63eba3eead26774", "0a1a5c523d835459c42f33e863623138555e2526"),
                                           file = c("test.c", "test.c", "test2.c", "test2.c"),
                                           artifact.type = c("Function", "Function", "Function", "Function"),
                                           artifact = c("File_Level", "File_Level", "File_Level", "File_Level"),
                                           weight = c(1, 1, 1, 1),
                                           type = c(4))

              network.expected = igraph::add_edges(network.expected, vertex.sequence.for.edges, attr = extra.edge.attributes)

              expect_true(igraph::identical_graphs(network.built, network.expected))
              })

test_that("Construction of the directed bipartite network for the featureexpression artifact with author.reltion = 'cochange' and artifact.
          relation = 'cochange'.",{

              ## configurations
              proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, "featureexpression")
              proj.conf$update.value("artifact.filter.base", FALSE)
              net.conf = NetworkConf$new()
              net.conf$update.values(updated.values = list(author.relation = "cochange", artifact.relation = "cochange",
                                                           author.directed = TRUE))

              ## construct objects
              proj.data = ProjectData$new(project.conf = proj.conf)
              network.builder = NetworkBuilder$new(project.data = proj.data, network.conf = net.conf)

              ## build network
              network.built = network.builder$get.bipartite.network()

              authors = c("Claus Hunsen", "Olaf")
              author.net = create.empty.network(directed = TRUE) +
                  igraph::vertex(authors, name  = authors, type = TYPE.AUTHOR)

              artifact.vertices = c("defined(A)", "Base_Feature")
              artifact.net = create.empty.network(directed = TRUE) +
                  igraph::vertices(artifact.vertices, name = artifact.vertices, type = TYPE.ARTIFACT, artifact.type = "featureexpression")

              network.expected = igraph::disjoint_union(author.net, artifact.net)

              vertex.sequence.for.edges = c(1,3,2,3,2,4)
              names(vertex.sequence.for.edges) = c("Claus Hunsen.Claus Hunsen", "Claus Hunsen.defined(A)", "Olaf.Olaf", "Olaf.defined(A)",
                                                   "Olaf.Olaf", "Olaf.Base_Feature")

              extra.edge.attributes = list(date = as.POSIXct(c("2016-07-12 15:58:59", "2016-07-12 16:00:45",
                                                               "2016-07-12 16:05:41")),
                                           hash = c("72c8dd25d3dd6d18f46e2b26a5f5b1e2e8dc28d0", "5a5ec9675e98187e1e92561e1888aa6f04faa338",
                                                    "3a0ed78458b3976243db6829f63eba3eead26774"),
                                           file = c("test.c", "test.c", "test2.c"),
                                           artifact.type = c("FeatureExpression", "FeatureExpression", "FeatureExpression"),
                                           artifact = c("defined(A)", "defined(A)", "Base_Feature"),
                                           weight = c(1, 1, 1),
                                           type = c(4))

              network.expected = igraph::add_edges(network.expected, vertex.sequence.for.edges, attr = extra.edge.attributes)

              expect_true(igraph::identical_graphs(network.built, network.expected))
              })
