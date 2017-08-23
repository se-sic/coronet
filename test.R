## (c) Claus Hunsen, 2016, 2017
## hunsen@fim.uni-passau.de
## (c) Raphael Nömmer, 2017
## noemmer@fim.uni-passau.de
## (c) Christian Hechtl, 2017
## hechtl@fim.uni-passau.de

source("util-init.R")

requireNamespace("logging")
logging::basicConfig(level = "DEBUG")
if (file.exists("test.log")) file.remove("test.log")
logging::addHandler(logging::writeToFile, file = "test.log", level = "DEBUG")
assign("last.warning", NULL, envir = baseenv())
options(mc.cores = 6L)

CF.DATA = "/path/to/codeface-data" # path to codeface data

CF.SELECTION.PROCESS = "threemonth" # releases, threemonth(, testing)

CASESTUDY = "busybox"
ARTIFACT = "feature" # function, feature, file, featureexpression

AUTHOR.RELATION = "mail" # mail, cochange
ARTIFACT.RELATION = "cochange" # cochange, callgraph


## CONFIGURATION

## initialize project configuration
proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
proj.conf$update.value("artifact.filter.base", FALSE)
# proj.conf$print()

## initialize network configuration
net.conf = NetworkConf$new()
net.conf$update.values(updated.values = list(author.relation = AUTHOR.RELATION, artifact.relation = ARTIFACT.RELATION))
# net.conf$print()

## get ranges
ranges = proj.conf$get.value("ranges")
revisions.callgraph = proj.conf$get.value("revisions.callgraph")


## PROJECT-LEVEL DATA

x.data = ProjectData$new(project.conf = proj.conf)
x = NetworkBuilder$new(project.data = x.data, network.conf = net.conf)

# x$get.commits.raw()
# x$get.synchronicity()
# x$get.author2artifact()
# x$get.commits.filtered()
# x$get.commits.filtered.empty()
# x$get.mails()
# x$get.authors()
# x$get.data.path()
# x$get.author2artifact()
# x$get.author2file()
# x$get.commit2artifact()
# x$get.commit2file()
# x$get.thread2author()
# x$update.network.conf(updated.values = list(author.directed = TRUE))
# x$get.author.network()
# x$update.network.conf(updated.values = list(author.directed = FALSE))
# x$get.author.network()
# x$get.artifact.network()
# x$reset.environment()
# x$get.networks()
# x$update.network.conf(updated.values = list(author.only.committers = FALSE, author.directed = FALSE))
# h = x$get.bipartite.network()
# plot.network(h)
# g = x$get.multi.network()
# plot.network(g)

# ## save binary objects
# net = x$get.author.network()
# save(net, file = sprintf("busybox_%s.network", x$get.network.conf.variable(var.name = "author.relation")))

# ## example for extensive configuration

# net = x$get.bipartite.network()

# save(net, file = sprintf("busybox_%s_%s_%s.network",  x$get.network.conf.variable(var.name = "author.relation"),
#                   ARTIFACT,  x$get.network.conf.variable(var.name = "artifact.relation")))


## RANGE-LEVEL DATA

y.data = RangeData$new(project.conf = proj.conf, range = ranges[[2]])
y = NetworkBuilder$new(project.data = y.data, network.conf = net.conf)

# y$get.commits.raw()
# y$get.commits.filtered()
# y$get.commits.filtered.empty()
# y$get.mails()
# y$get.authors()
# y$get.data.path()
# y$get.data.path.callgraph()
# y$get.author2artifact()
# y$get.author2file()
# y$update.network.conf(updated.values = list(edge.attributes = list("file")))
# y$get.commit2artifact()
# y$get.commit2file()
# y$update.network.conf(updated.values = list(edge.attributes = list("date")))
# y$get.thread2author()
# y$get.author.network()
# y$update.network.conf(updated.values = list(edge.attributes = list("hash")))
# y$get.artifact.network()
# y$get.networks()
# y$update.network.conf(updated.values = list(author.only.committers = FALSE, author.directed = TRUE))
# h = y$get.bipartite.network()
# plot.network(h)
# g = y$get.multi.network()
# plot.network(g)


## BULK METHODS to construct Codeface ranges
# net.conf = NetworkConf$new()

# ## author networks
#
# auth = collect.author.networks(proj.conf, net.conf)
#
# for (net in auth) {
#     plot.network(net)
# }

# ## artifact networks
#
# art = collect.artifact.networks(proj.conf, net.conf)
#
# for (net in art) {
#     plot.network(net)
# }

# ## bipartite networks
#
# bp = collect.bipartite.networks(proj.conf, net.conf)
#
# for (net in bp) {
#     plot.network(net)
# }

# ## multi networks
#
# multi = collect.multi.networks(proj.conf, net.conf)
#
# for (net in multi) {
#     plot.network(net)
# }

# ## lapply on data objects
# data = construct.data(proj.conf, callgraphs = TRUE)
# run.lapply(data, "get.data.path.callgraph")


## SPLITTING DATA AND NETWORKS

# cf.data = split.data.time.based(x, time.period = "18 months", split.basis = "commits", sliding.window = TRUE)
# for (range in names(cf.data)) {
#     y = cf.data[[range]]
#     plot.network(y$get.bipartite.network())
# }
# print(run.lapply(cf.data, "get.class.name"))

# mybins = as.POSIXct(c("2012-07-10 15:58:00", "2012-07-15 16:02:00", "2012-07-20 16:04:00", "2012-07-25 16:06:30"))
# cf.data = split.data.time.based(x, bins = mybins)
# for (range in names(cf.data)) {
#     y = cf.data[[range]]
#     plot.network(y$get.bipartite.network())
# }
# print(run.lapply(cf.data, "get.class.name"))

# cf.data = split.data.activity.based(x, activity.amount = 10000, activity.type = "mails")
# for (range in names(cf.data)) {
#     y = cf.data[[range]]
#     plot.network(y$get.bipartite.network())
# }
# print(run.lapply(cf.data, "get.class.name"))

# g = y$get.bipartite.network()
# nets = split.network.time.based(g, time.period = "1 month", sliding.window = TRUE)
# for (net in nets) {
#     plot.network(net)
# }

# g = y$get.bipartite.network()
# mybins = as.POSIXct(c("2013-05-23", "2013-06-11", "2013-06-27"))
# nets = split.network.time.based(g, bins = mybins)
# for (net in nets) {
#     plot.network(net)
# }

# g = y$get.bipartite.network()
# nets = split.network.activity.based(g, number.windows = 2)
# for (net in nets) {
#     plot.network(net)
# }

# g = y$get.bipartite.network()
# nets = split.network.activity.based(g, number.edges = 500, sliding.window = TRUE)
# for (net in nets) {
#     plot.network(net)
# }


## MOTIF IDENTIFICATION

# g = save.and.load("g", "y.multi.dat", skip = FALSE, if.not.found = function() {
#     g = y$get.multi.network()
#     return(g)
# })
# g.motifs = motifs.count(network = g, remove.duplicates = TRUE,
#                         motif.collaborating = MOTIFS.TRIANGLE.NEGATIVE,
#                         motif.communicating = MOTIFS.LINE,
#                         motif.collaborating.and.communicating = MOTIFS.TRIANGLE.POSITIVE)


## PLOTS

# ## construct sample network for plotting
# g = get.sample.network()
# g = igraph::as.directed(g, mode = "arbitrary")
# g = g + igraph::edges("A6", "A5", type = TYPE.EDGES.INTRA, weight = 2)
# g = simplify.network(g)

# ## print directly
# plot.print.network(g, labels = TRUE, grayscale = FALSE)

# ## set a layout and print directly
# lay = matrix(c(  20, 179, 552, 693, 956, 1091, 124, 317, 516, 615, 803, 1038,
#                 245, 175, 185, 255, 253, 225,   73,   8,  75,   0,  96,   86),
#              nrow = 12, byrow = FALSE) # for sample graph
# g = igraph::set.graph.attribute(g, "layout", lay)
# plot.print.network(g, labels = TRUE, grayscale = FALSE)

# ## get the plot object and modify it before plotting
# p = plot.get.plot.for.network(g, labels = FALSE, grayscale = TRUE)
# p = p +
#     ggplot2::theme(
#         panel.border = ggplot2::element_blank(),
#         legend.position = "none"
#     ) +
#     ggraph::facet_edges( ~ edge.type.char)
# # ggraph::facet_edges( ~ weight)
# # ggraph::facet_nodes( ~ vertex.type.char)
# # ggraph::facet_graph(edge.type.char ~ vertex.type.char)
# print(p)
