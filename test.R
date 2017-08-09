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

proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
net.conf = NetworkConf$new()
#initialize with AUTHOR.RELATION and ARTIFACT.RELATION
net.conf$update.values(updated.values = list(author.relation = AUTHOR.RELATION, artifact.relation = ARTIFACT.RELATION))

## get ranges
ranges = proj.conf$get.entry(entry.name = "ranges")
revisions.callgraph = proj.conf$get.entry("revisions.callgraph")


## PROJECT-LEVEL DATA

x = CodefaceProjectData$new(proj.conf, net.conf)

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
# x$update.network.conf(updated.values = list(artifact.filter.base = FALSE, author.only.committers = TRUE))
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

y = CodefaceRangeData$new(project.conf = proj.conf, network.conf = net.conf, range = ranges[[2]])
# y = CodefaceRangeData$new(project.conf = proj.conf, network.conf = net.conf, range = ranges[[2]], revision.callgraph = revisions.callgraph[[3]])
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
# y$update.network.conf(updated.values = list(artifact.filter.base = FALSE, author.only.committers = TRUE, author.directed = TRUE))
# h = y$get.bipartite.network()
# plot.network(h)
# g = y$get.multi.network()
# plot.network(g)


## BULK METHODS
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


## SPLITTING DATA

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

## SAMPLE PLOTS (e.g., for papers)

# pdf(file = "sample-network.pdf", width = 9, height = 5)
# g = get.sample.network()
# plot.network(g, grayscale = FALSE)
# dev.off()

# pdf(file = "sample-network-bw.pdf", width = 9, height = 5)
# g = get.sample.network()
# plot.network(g, grayscale = TRUE)
# dev.off()


## MOTIF IDENTIFICATION

# g = save.and.load("g", "y.multi.dat", skip = FALSE, if.not.found = function() {
#     g = y$get.multi.network()
#     return(g)
# })
# g.motifs = motifs.count(network = g, remove.duplicates = TRUE,
#                         motif.collaborating = MOTIFS.TRIANGLE.NEGATIVE,
#                         motif.communicating = MOTIFS.LINE,
#                         motif.collaborating.and.communicating = MOTIFS.TRIANGLE.POSITIVE)


## CORE/PERIPHERAL CLASSIFICATION

rangeData = CodefaceRangeData$new(project.conf = proj.conf, network.conf = net.conf, range = ranges[[10]])
rangeData2 = CodefaceRangeData$new(project.conf = proj.conf, network.conf = net.conf, range = ranges[[11]])
emptyRangeData = CodefaceRangeData$new(project.conf = proj.conf, network.conf = net.conf, range = ranges[[1]])

graph = rangeData$get.author.network()
emptyGraph = emptyRangeData$get.author.network()

graphList = list(emptyGraph, graph, rangeData2$get.author.network())
rangeList = list(emptyRangeData, rangeData, rangeData2)

# test functions for single range
developerClass = get.developer.class.by.type(graph = graph, type = "networkDegree")
get.developer.class.by.type(graph = graph, type = "networkEigen")
get.developer.class.by.type(data = rangeData, type = "commitCount")
get.developer.class.by.type(data = rangeData, type = "locCount")

# test functions for single range with "empty" range data (graph without edges)
developerClassEmptyRange = get.developer.class.by.type(graph = emptyGraph, type = "networkDegree")
get.developer.class.by.type(graph = emptyGraph, type = "networkEigen")
get.developer.class.by.type(data = emptyRangeData, type = "commitCount")
get.developer.class.by.type(data = emptyRangeData, type = "locCount")

# test function for mutliple ranges (evolution)
developerClassOverview = get.developer.class.overview(graphList = graphList, type = "networkDegree")
get.developer.class.overview(graphList = graphList, type = "networkEigen")
get.developer.class.overview(codefaceRangeDataList = rangeList, type = "commitCount")
developerClassOverviewLOC = get.developer.class.overview(codefaceRangeDataList = rangeList, type = "locCount")

recurringAuthors = get.recurring.authors(developerClassOverview = developerClassOverview, class = "both")
longtermCore = get.recurring.authors(developerClassOverview = developerClassOverview, class = "core")

roleStability = get.role.stability(developerClassOverview = developerClassOverview)

developerClassActivity = get.developer.class.activity(codefaceRangeData = rangeData, developer.class = developerClass,
                                                       activityMeasure = "commit.count")
developerClassActivityEmpty = get.developer.class.activity(codefaceRangeData = emptyRangeData,
                                                       developer.class = developerClassEmptyRange, activityMeasure = "loc.count")

developerClassActivityOverview = get.developer.class.activity.overview(codefaceRangeDataList = rangeList,
                                                                        developer.class.overview = developerClassOverview,
                                                                        activityMeasure = "commit.count")
get.developer.class.activity.overview(codefaceRangeDataList = rangeList,
                                      developer.class.overview = developerClassOverview,
                                      activityMeasure = "commit.count", longterm.cores = "Erik Andersen")

calculate.cohens.kappa(developerClassificationList = developerClassOverview,
                       comparingDeveloperClassificationList = developerClassOverviewLOC)

get.class.turnover.overview(developerClassOverview = developerClassOverview)
get.unstable.developers.overview(developerClassOverview = developerClassOverview, saturation = 2)
