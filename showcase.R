## This file is part of codeface-extraction-r, which is free software: you
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
## Copyright 2016-2018 by Claus Hunsen <hunsen@fim.uni-passau.de>
## Copyright 2017 by Raphael Nömmer <noemmer@fim.uni-passau.de>
## Copyright 2017 by Christian Hechtl <hechtl@fim.uni-passau.de>
## Copyright 2017 by Felix Prasse <prassefe@fim.uni-passau.de>
## Copyright 2017 by Thomas Bock <bockthom@fim.uni-passau.de>
## All Rights Reserved.


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Initialization ----------------------------------------------------------

source("util-init.R")


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Logging -----------------------------------------------------------------

requireNamespace("logging")
logging::basicConfig(level = "DEBUG")
if (file.exists("showcase.log")) file.remove("showcase.log")
logging::addHandler(logging::writeToFile, file = "showcase.log", level = "DEBUG")


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## System variables and R settings -----------------------------------------

assign("last.warning", NULL, envir = baseenv()) # reset warnings
options(mc.cores = 6L) # configure parallelism


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Case-study configuration ------------------------------------------------

CF.DATA = "/path/to/codeface-data" # path to codeface data

CF.SELECTION.PROCESS = "threemonth" # releases, threemonth(, testing)

CASESTUDY = "busybox"
ARTIFACT = "feature" # function, feature, file, featureexpression (only relevant for cochange)

AUTHOR.RELATION = "mail" # mail, cochange, issue
ARTIFACT.RELATION = "cochange" # cochange, callgraph, mail, issue


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Project and network configuration ---------------------------------------

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


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Project-level data ------------------------------------------------------

x.data = ProjectData$new(project.conf = proj.conf)
x = NetworkBuilder$new(project.data = x.data, network.conf = net.conf)

## * Data retrieval --------------------------------------------------------

# x.data$get.commits()
# x.data$get.synchronicity()
# x.data$get.author2artifact()
# x.data$get.commits.filtered()
# x.data$get.commits.filtered.empty()
# x.data$get.mails()
# x.data$get.authors()
# x.data$get.data.path()
# x.data$get.author2artifact()
# x.data$get.author2file()
# x.data$get.commit2artifact()
# x.data$get.commit2file()
# x.data$get.thread2author()

## * Network construction --------------------------------------------------

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

## * Backups ---------------------------------------------------------------

# ## save binary objects
# net = x$get.author.network()
# save(net, file = sprintf("busybox_%s.network", x$get.network.conf.variable(var.name = "author.relation")))


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Range-level data --------------------------------------------------------

y.data = RangeData$new(project.conf = proj.conf, range = ranges[[22]])
y = NetworkBuilder$new(project.data = y.data, network.conf = net.conf)

## * Data retrieval --------------------------------------------------------

# y.data$get.commits()
# y.data$get.synchronicity()
# y.data$get.author2artifact()
# y.data$get.commits.filtered()
# y.data$get.commits.filtered.empty()
# y.data$get.mails()
# y.data$get.authors()
# y.data$get.data.path()
# y.data$get.author2artifact()
# y.data$get.author2file()
# y.data$get.commit2artifact()
# y.data$get.commit2file()
# y.data$get.thread2author()

## * Network construction --------------------------------------------------

# y$update.network.conf(updated.values = list(edge.attributes = list("date")))
# y$get.author.network()
# y$update.network.conf(updated.values = list(edge.attributes = list("hash")))
# y$get.artifact.network()
# y$get.networks()
# y$update.network.conf(updated.values = list(author.only.committers = FALSE, author.directed = TRUE))
# h = y$get.bipartite.network()
# plot.network(h)
# g = y$get.multi.network()
# plot.network(g)


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Vertex attributes -------------------------------------------------------

# ## define bins for network construction
# mybins = c("2012-07-10 15:58:00", "2012-07-15 16:02:00", "2012-07-20 16:04:00", "2012-07-25 16:06:30")
# ## split data into ranges
# cf.data = split.data.time.based(x.data, bins = mybins)
# ## construct (author) networks from range data
# my.networks = lapply(cf.data, function(range.data) {
#     y = NetworkBuilder$new(project.data = range.data, network.conf = net.conf)
#     return (y$get.author.network())
# })
# ## add commit-count vertex attributes
# sample = add.vertex.attribute.commit.count.author(my.networks, x.data, aggregation.level = "range")
# sample.cumulative = add.vertex.attribute.commit.count.author(my.networks, x.data, aggregation.level = "cumulative")
# ## add email-address vertex attribute
# sample.mail = add.vertex.attribute.author.email(my.networks, x.data, "author.email")

# ## add vertex attributes for the project-level network
# x.net.as.list = list("1970-01-01 00:00:00-2030-01-01 00:00:00" = x$get.author.network())
# sample.entire = add.vertex.attribute.commit.count.author(x.net.as.list, x.data, aggregation.level = "complete")


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Bulk methods for Codeface ranges ----------------------------------------

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


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Splitting data and networks ---------------------------------------------

## * Data-based splitting --------------------------------------------------

# cf.data = split.data.time.based(x.data, time.period = "18 months", split.basis = "commits", sliding.window = TRUE)
# for (range in names(cf.data)) {
#     y.data = cf.data[[range]]
#     y = NetworkBuilder$new(project.data = y.data, network.conf = net.conf)
#     plot.network(y$get.bipartite.network())
# }
# print(run.lapply(cf.data, "get.class.name"))

# mybins = c("2012-07-10 15:58:00", "2012-07-15 16:02:00", "2012-07-20 16:04:00", "2012-07-25 16:06:30")
# cf.data = split.data.time.based(x.data, bins = mybins)
# for (range in names(cf.data)) {
#     y.data = cf.data[[range]]
#     y = NetworkBuilder$new(project.data = y.data, network.conf = net.conf)
#     plot.network(y$get.bipartite.network())
# }
# print(run.lapply(cf.data, "get.class.name"))

# cf.data = split.data.activity.based(x.data, activity.amount = 10000, activity.type = "mails")
# for (range in names(cf.data)) {
#     y.data = cf.data[[range]]
#     y = NetworkBuilder$new(project.data = y.data, network.conf = net.conf)
#     plot.network(y$get.bipartite.network())
# }
# print(run.lapply(cf.data, "get.class.name"))

## * Network-based splitting -----------------------------------------------

# g = y$get.bipartite.network()
# nets = split.network.time.based(g, time.period = "1 month", sliding.window = TRUE)
# for (net in nets) {
#     plot.network(net)
# }

# g = y$get.bipartite.network()
# mybins = c("2004-10-23", "2004-11-11", "2004-12-27 00:12:23")
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


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Motif identification ----------------------------------------------------

# g = save.and.load("g", "y.multi.dat", skip = FALSE, if.not.found = function() {
#     g = y$get.multi.network()
#     return(g)
# })
# g.motifs = motifs.count(network = g, remove.duplicates = TRUE,
#                         motif.collaborating = MOTIFS.TRIANGLE.NEGATIVE,
#                         motif.communicating = MOTIFS.LINE,
#                         motif.collaborating.and.communicating = MOTIFS.TRIANGLE.POSITIVE)


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Plots -------------------------------------------------------------------

# ## construct sample network for plotting
# g = get.sample.network()
# g = igraph::as.directed(g, mode = "arbitrary")
# g = g + igraph::edges("A6", "A5", type = TYPE.EDGES.INTRA, weight = 2,
#                       relation = "callgraph", artifact.type = "Feature")
# g = simplify.network(g)

# ## print directly
# plot.print.network(g, labels = TRUE)

# ## set a layout and print directly
# lay = matrix(c(  20, 179, 693, 552, 956, 1091, 124, 317, 516, 615, 803, 1038,
#                 245, 175, 255, 185, 253, 225,   73,   8,  75,   0,  96,   86),
#              nrow = 12, byrow = FALSE) # for sample graph
# g = igraph::set.graph.attribute(g, "layout", lay)
# plot.print.network(g, labels = TRUE)

# ## get the plot object and modify it before plotting
# p = plot.get.plot.for.network(g, labels = FALSE)
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


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Core/peripheral classification ------------------------------------------

# range.data = RangeData$new(project.conf = proj.conf, range = ranges[[22]])
# range.data2 = RangeData$new(project.conf = proj.conf, range = ranges[[23]])
# empty.range.data = RangeData$new(project.conf = proj.conf, range = ranges[[1]])

# range.net = NetworkBuilder$new(project.data = range.data, network.conf = net.conf)
# range.net2 = NetworkBuilder$new(project.data = range.data2, network.conf = net.conf)
# empty.range.net = NetworkBuilder$new(project.data = empty.range.data, network.conf = net.conf)

# network = range.net$get.author.network()
# network2 = range.net2$get.author.network()
# empty.network = empty.range.net$get.author.network()

# network.list = list(empty.network, network, network2)
# range.list = list(empty.range.data, range.data, range.data2)

# ## test functions for single range
# author.class = get.author.class.by.type(network = network, type = "network.degree")
# get.author.class.by.type(network = network, type = "network.eigen")
# get.author.class.by.type(data = range.data, type = "commit.count")
# get.author.class.by.type(data = range.data, type = "loc.count")

# ## test functions for single range with "empty" range data (network without edges)
# author.class.empty.range = get.author.class.by.type(network = empty.network, type = "network.degree")
# get.author.class.by.type(network = empty.network, type = "network.eigen")
# get.author.class.by.type(data = empty.range.data, type = "commit.count")
# get.author.class.by.type(data = empty.range.data, type = "loc.count")

# ## test function for mutliple ranges (evolution)
# author.class.overview = get.author.class.overview(network.list = network.list, type = "network.degree")
# get.author.class.overview(network.list = network.list, type = "network.eigen")
# get.author.class.overview(range.data.list = range.list, type = "commit.count")
# author.class.overview.loc = get.author.class.overview(range.data.list = range.list, type = "loc.count")
#
# recurring.authors = get.recurring.authors(author.class.overview = author.class.overview, class = "both")
# longterm.core = get.recurring.authors(author.class.overview = author.class.overview, class = "core")
#
# role.stability = get.role.stability(author.class.overview = author.class.overview)
#
# author.class.activity = get.author.class.activity(range.data = range.data, author.class = author.class,
#                                                   activity.measure = "commit.count")
# author.class.activity.empty = get.author.class.activity(range.data = empty.range.data,
#                                                        author.class = author.class.empty.range, activity.measure = "loc.count")
#
# author.class.activity.overview = get.author.class.activity.overview(range.data.list = range.list,
#                                                                     author.class.overview = author.class.overview,
#                                                                     activity.measure = "commit.count")
# get.author.class.activity.overview(range.data.list = range.list,
#                                       author.class.overview = author.class.overview,
#                                       activity.measure = "commit.count", longterm.cores = "Erik Andersen")
#
# calculate.cohens.kappa(author.classification.list = author.class.overview,
#                        other.author.classification.list = author.class.overview.loc)
#
# get.class.turnover.overview(author.class.overview = author.class.overview)
# get.unstable.authors.overview(author.class.overview = author.class.overview, saturation = 2)
