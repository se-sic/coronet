source("util-misc.R")
source("util-conf.R")
source("util-data.R")
source("util-bulk.R")
source("util-plot.R")

library(logging)
logging::basicConfig(level = "DEBUG")
if (file.exists("test.log")) file.remove("test.log")
logging::addHandler(logging::writeToFile, file = "test.log", level = "DEBUG")
assign("last.warning", NULL, envir = baseenv())
options(mc.cores = 6L)


CF.DATA = "C:/Users/chris/Documents/Arbeit/codeface-data"
CF.SELECTION.PROCESS = "testing" # releases, threemonth(, testing)

CASESTUDY = "test"
ARTIFACT = "feature" # function, feature, file, featureexpression
## FIXME test:
## - artifact = featureexpression

#AUTHOR.RELATION = "mail" # mail, cochange
#ARTIFACT.RELATION = "cochange" # cochange, callgraph


## CONFIGURATION

conf = CodefaceConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)

## get ranges
ranges = conf$get.ranges()
revisions.callgraph = conf$get.revisions.callgraph()


## PROJECT-LEVEL DATA

x = CodefaceProjectData$new(conf)
# x$get.commits.raw()
# x$get.synchronicity(time.window = c(5))
# x$get.author2artifact()
# x$get.commits.raw()
# x$get.commits()
# x$get.mails()
# x$get.authors()
# x$get.data.path()
# x$get.author2artifact()
# x$get.author2file()
# x$update.network.conf(updated.values = list(artifact.edge.attributes = list("file")))
# x$get.commit2artifact()
# x$get.commit2file()
# x$get.thread2author()
# x$get.author.network() #Error in FUN(X[[i]], ...) : attempt to apply non-function ???
# x$get.author.network()  #Error in FUN(X[[i]], ...) : attempt to apply non-function ???
# x$get.artifact.network()
# x$get.networks()
# g = x$get.bipartite.network()
# plot.bipartite.network(g)

# ## save binary objects
# net = x$get.author.network()
# save(net, file = sprintf("busybox_%s.network", x$get.network.conf.variable(var.name = "author.relation")))

# ## example for extensive configuration
# net = x$get.bipartite.network(network.conf)
# save(net, file = sprintf("busybox_%s_%s_%s.network",  x$get.network.conf.variable(var.name = "author.relation"),
#                   ARTIFACT,  x$get.network.conf.variable(var.name = "artifact.relation")))


## RANGE-LEVEL DATA

y <- CodefaceRangeData$new(conf = conf, range = ranges[[1]])
# y <- CodefaceRangeData$new(conf = conf, range = ranges[[2]], revision.callgraph = revisions.callgraph[[3]])
# y$get.commits()
# y$get.mails()
# y$get.authors()
# y$get.data.path()
# y$get.data.path.range.callgraph()
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
# network.conf = NetworkConf$new()
# ## author networks
# auth = collect.author.networks(conf, network.conf)
# for (net in auth) {
#     plot.author.network(net)
# }
#
# ## artifact networks
# art = collect.artifact.networks(conf, network.conf)
# for (net in art) {
#     plot.artifact.network(net)
# }
#
# ## bipartite networks
# bp = collect.bipartite.networks(conf, network.conf)
# for (net in bp) {
#     plot.bipartite.network(net)
# }
#
# ## lapply on data objects
# data = construct.data(conf, callgraphs = TRUE)
# run.lapply(data, "get.data.path.callgraph")


## SAMPLE PLOTS (e.g., for papers)

# pdf(file = "sample-network.pdf", width = 9, height = 5)
# g = get.sample.network()
# plot.bipartite.network(g, grayscale = FALSE)
# dev.off()
#
# pdf(file = "sample-network-bw.pdf", width = 9, height = 5)
# g = get.sample.network()
# plot.bipartite.network(g, grayscale = TRUE)
# dev.off()
# network.conf$update.values(list(author.relation = "mail", author.directed = TRUE))
#network.conf$print.object()
# print(network.conf$get.variable("author.relation"))
