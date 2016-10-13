source("util-misc.R")
source("util-conf.R")
source("util-data.R")
source("util-bulk.R")
source("util-plot.R")

library(logging)
logging::basicConfig()
assign("last.warning", NULL, envir = baseenv())


CF.DATA = "/local/hunsen/projects/codeface-data"
CF.SELECTION.PROCESS = "testing" # releases, threemonth(, testing)

CASESTUDY = "busybox"
ARTIFACT = "feature" # function, feature, file, featureexpression
## FIXME test:
## - author network = cochange
## - artifact = featureexpression

AUTHOR.RELATION = "mail" # mail, cochange
ARTIFACT.RELATION = "cochange" # cochange, callgraph


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
# x$get.commits.raw(synchronicity = TRUE, synchronicity.window = 5)
# x$get.commits()
# x$get.mails()
# x$get.authors()
# x$get.data.path()
# x$get.author2artifact()
# x$get.author2file()
# x$get.commit2artifact(extra.data = c("file"))
# x$get.commit2file()
# x$get.thread2author(extra.data = "date")
# x$get.author.network(relation = AUTHOR.RELATION, directed = TRUE, simple.network = FALSE)
# x$get.author.network(relation = AUTHOR.RELATION, directed = FALSE)
# x$get.artifact.network(relation = ARTIFACT.RELATION, extra.edge.attr = c("hash"))
# x$get.networks(author.relation = AUTHOR.RELATION, artifact.relation = ARTIFACT.RELATION)
# g = x$get.bipartite.network(author.relation = AUTHOR.RELATION, artifact.relation = ARTIFACT.RELATION, artifact.filter.base = FALSE,
#                             author.only.committers = TRUE, author.directed = FALSE)
# plot.bipartite.network(g)

# ## save binary objects
# net = x$get.author.network(relation = "mail", directed = TRUE, simple.network = FALSE)
# save(net, file = sprintf("busybox_%s.network", AUTHOR.RELATION))

# ## example for extensive configuration
# net = x$get.bipartite.network(author.relation = AUTHOR.RELATION, artifact.relation = ARTIFACT.RELATION,
#                               author.directed = TRUE, author.only.committers = FALSE,
#                               artifact.extra.edge.attr = c("date", "hash"), artifact.filter.empty = TRUE,
#                               artifact.filter = TRUE, artifact.filter.base = TRUE,
#                               simple.network = FALSE, contract.edges = FALSE)
# save(net, file = sprintf("busybox_%s_%s_%s.network", AUTHOR.RELATION, ARTIFACT, ARTIFACT.RELATION))


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
# y$get.commit2artifact(extra.data = c("file"))
# y$get.commit2file()
# y$get.thread2author(extra.data = "date")
# y$get.author.network(relation = AUTHOR.RELATION, directed = FALSE, simple.network = FALSE)
# y$get.artifact.network(relation = ARTIFACT.RELATION, extra.edge.attr = c("hash"))
# y$get.networks(author.relation = AUTHOR.RELATION, artifact.relation = ARTIFACT.RELATION)
# g = y$get.bipartite.network(author.relation = AUTHOR.RELATION, artifact.relation = ARTIFACT.RELATION, artifact.filter.base = FALSE,
#                             author.only.committers = TRUE, author.directed = TRUE)
# plot.bipartite.network(g)


## BULK METHODS

# ## author networks
# auth = collect.author.networks(conf, author.relation = AUTHOR.RELATION, author.directed = TRUE, simple.network = FALSE)
# for (net in auth) {
#     plot.author.network(net)
# }
#
# ## artifact networks
# art = collect.artifact.networks(conf, artifact.relation = c("cochange", "callgraph"),
#                               filter.artifact = TRUE, filter.base.artifact = TRUE, extra.edge.attr = c("hash"))
# for (net in art) {
#     plot.artifact.network(net)
# }
#
# ## bipartite networks
# bp = collect.bipartite.networks(conf, author.relation = AUTHOR.RELATION, artifact.relation = ARTIFACT.RELATION,
#                              simple.network = TRUE, author.directed = TRUE, artifact.extra.edge.attr = c("hash"),
#                              artifact.filter = TRUE, artifact.filter.base = FALSE)
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
