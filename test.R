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
## - artifact = file and author network = cochange
## - artifact = featureexpression

AUTHOR.RELATION = "mail" # mail, cochange
ARTIFACT.RELATION = "cochange" # cochange, callgraph


conf = CodefaceConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)

ranges = conf$get.ranges()
revisions.callgraph = conf$get.revisions.callgraph()

# ## project level
# x = CodefaceProjectData$new(conf)
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
# g = x$get.bipartite.network(author.relation = AUTHOR.RELATION, artifact.relation = ARTIFACT.RELATION, artifact.filter.base = FALSE, author.only.committers = TRUE,
# author.directed = FALSE)
# plot.bipartite.network(g)

## range level
y <- CodefaceRangeData$new(conf = conf, range = ranges[[1]], revision.callgraph = revisions.callgraph[[2]])
# y <- CodefaceRangeData$new(conf = conf, range = ranges[[2]], revision.callgraph = revisions.callgraph[[3]])
# print(y$get.commits())
# y$get.mails()
# y$get.author.network(relation = AUTHOR.RELATION, directed = FALSE)
# y$get.authors()
# y$get.data.path()
# y$get.data.path.range.callgraph()
y$get.author2artifact()
# y$get.author2file()
# y$get.commit2artifact(extra.data = c("file"))
# y$get.commit2file()
# y$get.thread2author(extra.data = "date")
# y$get.author.network(relation = AUTHOR.RELATION, directed = TRUE, simple.network = FALSE)
# y$get.author.network(relation = AUTHOR.RELATION, directed = FALSE)
# y$get.artifact.network(relation = ARTIFACT.RELATION, extra.edge.attr = c("hash"))
# y$get.networks(author.relation = AUTHOR.RELATION, artifact.relation = ARTIFACT.RELATION)
# g = y$get.bipartite.network(author.relation = AUTHOR.RELATION, artifact.relation = ARTIFACT.RELATION, artifact.filter.base = FALSE, author.only.committers = TRUE,
#                             author.directed = TRUE)
# plot.bipartite.network(g)

# ## bulk
# auth = collect.author.networks(conf, author.relation = AUTHOR.RELATION, author.directed = TRUE, simple.network = FALSE)
# for (net in auth) {
#     plot.author.network(net)
# }
#
# art = collect.artifact.networks(conf, artifact.relation = c("cochange", "callgraph"),
#                               filter.artifact = TRUE, filter.base.artifact = TRUE, extra.edge.attr = c("hash"))
# for (net in art) {
#     plot.artifact.network(net)
# }
#
# bp = collect.bipartite.networks(conf, author.relation = AUTHOR.RELATION, artifact.relation = ARTIFACT.RELATION,
#                              simple.network = TRUE, author.directed = TRUE, artifact.extra.edge.attr = c("hash"),
#                              artifact.filter = TRUE, artifact.filter.base = FALSE)
# for (net in bp) {
#     plot.bipartite.network(net)
# }

# plot.bipartite.network(get.sample.network())

# y$save.to.disk("y.save")

data = construct.data(conf, callgraphs = TRUE)
run.lapply(data, "get.data.path.callgraph")
