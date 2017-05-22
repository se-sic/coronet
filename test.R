## (c) Claus Hunsen, 2016, 2017
## hunsen@fim.uni-passau.de

source("util-misc.R")
source("util-conf.R")
source("util-data.R")
source("util-split.R")
source("util-bulk.R")
source("util-plot.R")

requireNamespace("logging")
logging::basicConfig(level = "DEBUG")
if (file.exists("test.log")) file.remove("test.log")
logging::addHandler(logging::writeToFile, file = "test.log", level = "DEBUG")
assign("last.warning", NULL, envir = baseenv())
options(mc.cores = 6L)


CF.DATA = "C:/Users/RNoem/Codeface/codeface-data"
CF.SELECTION.PROCESS = "testing" # releases, threemonth(, testing)

CASESTUDY = "test"
ARTIFACT = "feature" # function, feature, file, featureexpression


## CONFIGURATION

conf = CodefaceConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)

## get ranges
ranges = conf$get.ranges()
revisions.callgraph = conf$get.revisions.callgraph()


## PROJECT-LEVEL DATA

network.conf = NetworkConf$new()
x = CodefaceProjectData$new(conf)
x$get.network.conf.variable(var.name = "synchronicity.time.window")
x$get.commits.raw()
x$get.synchronicity()
x$get.author2artifact()
x$get.commits()
x$get.mails()
x$get.authors()
x$get.data.path()
x$get.author2artifact()
x$get.author2file()
x$update.network.conf(updated.values = list(artifact.edge.attributes = list("file")))
x$get.commit2artifact()
x$get.commit2file()
x$get.thread2author()
x$get.author.network()
x$get.author.network()
x$get.artifact.network()
x$get.networks()
g = x$get.bipartite.network()
plot.bipartite.network(g)

# ## save binary objects
# net = x$get.author.network()
# save(net, file = sprintf("busybox_%s.network", x$get.network.conf.variable(var.name = "author.relation")))

# ## example for extensive configuration
# net = x$get.bipartite.network()
# save(net, file = sprintf("busybox_%s_%s_%s.network",  x$get.network.conf.variable(var.name = "author.relation"),
#                   ARTIFACT,  x$get.network.conf.variable(var.name = "artifact.relation")))


## RANGE-LEVEL DATA

y <- CodefaceRangeData$new(conf = conf, range = ranges[[2]])
# y <- CodefaceRangeData$new(conf = conf, range = ranges[[2]], revision.callgraph = revisions.callgraph[[3]])
# y$get.commits.raw()
# y$get.commits()
# y$get.mails()
# y$get.authors()
# y$get.data.path()
# y$get.data.path.callgraph()
# y$get.author2artifact()
# y$get.author2file()
# y$get.commit2artifact()
# y$get.commit2file()
# y$get.thread2author()
# y$get.author.network()
# y$get.artifact.network()
# y$get.networks()
# g = y$get.bipartite.network()
# plot.bipartite.network(g, labels = FALSE, grayscale = FALSE)


## BULK METHODS

# ## author networks
# auth = collect.author.networks(conf)
# for (net in auth) {
#     plot.author.network(net)
# }

# ## artifact networks
# art = collect.artifact.networks(conf)
# for (net in art) {
#     plot.artifact.network(net)
# }

# ## bipartite networks
# bp = collect.bipartite.networks(conf)
# for (net in bp) {
#     plot.bipartite.network(net)
# }

# ## lapply on data objects
# data = construct.data(conf, callgraphs = TRUE)
# run.lapply(data, "get.data.path.callgraph")


## SPLITTING DATA

# cf.data = split.data.time.based(x, time.period = "18 months", split.basis = "commits")
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
# nets = split.network.time.based(g, time.period = "1 month")
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
# nets = split.network.activity.based(g, number.edges = 500)
# for (net in nets) {
#     plot.network(net)
# }

## SAMPLE PLOTS (e.g., for papers)

# pdf(file = "sample-network.pdf", width = 9, height = 5)
# g = get.sample.network()
# plot.bipartite.network(g, grayscale = FALSE)
# dev.off()

# pdf(file = "sample-network-bw.pdf", width = 9, height = 5)
# g = get.sample.network()
# plot.bipartite.network(g, grayscale = TRUE)
# dev.off()
