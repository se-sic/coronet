## (c) Claus Hunsen, 2016, 2017
## hunsen@fim.uni-passau.de
## (c) Raphael Nömmer, 2017
## noemmer@fim.uni-passau.de
## (c) Christian Hechtl, 2017
## hechtl@fim.uni-passau.de


## libraries
requireNamespace("R6") # for R6 classes
requireNamespace("logging") # for logging
requireNamespace("parallel") # for parallel computation
requireNamespace("plyr") # for dlply function
requireNamespace("igraph") # networks




## NetworkBuilder ####
NetworkBuilder = R6::R6Class("NetworkBuilder",
  ## private members ####
  private = list(
    proj.data = NULL,
    network.conf = NULL,

    ## networks
    authors.network.mail = NULL, # igraph
    authors.network.cochange = NULL, # igraph
    artifacts.network.cochange = NULL, # igraph
    artifacts.network.callgraph = NULL, # igraph

    ## AUTHOR NETWORKS ####

    ## get the co-change-based developer relation as network
    get.author.network.cochange = function() {

      logging::logdebug("get.author.network.cochange: starting.")

      ## do not compute anything more than once
      if (!is.null(private$authors.network.cochange)) {
        logging::logdebug("get.author.network.cochange: finished. (already existing)")
        return(private$authors.network.cochange)
      }

      ## construct network based on artifact2author data
      author.net = construct.dependency.network.from.list(private$proj.data$get.artifact2author(), network.conf = private$network.conf,
                                                          directed = private$network.conf$get.variable("author.directed"))

      ## store network
      private$authors.network.cochange = author.net
      logging::logdebug("get.author.network.cochange: finished.")

      return(author.net)
    },

    ## get the thread-based developer relation as network
    get.author.network.mail = function() {

      logging::logdebug("get.author.network.mail: starting.")

      ## do not compute anything more than once
      if (!is.null(private$authors.network.mail)) {
        logging::logdebug("get.author.network.mail: finished. (already existing)")
        return(private$authors.network.mail)
      }

      edge.attributes = c("date", "message.id", "thread")

      if (length(thread2author) != 0) {
        dev.relation = construct.dependency.network.from.list(private$proj.data$get.thread2author(), network.conf = private$network.conf,
                                                 directed = private$network.conf$get.variable("author.directed"))
      } else {
        dev.relation = create.empty.network(private$network.conf$get.variable("author.directed"))
      }

      ## store network
      private$authors.network.mail = dev.relation
      logging::logdebug("get.author.network.mail: finished.")

      return(dev.relation)
    },


    ## ARTIFACT NETWORKS ####

    ## co-change-based artifact network
    get.artifact.network.cochange = function() {

      logging::logdebug("get.artifact.network.cochange: starting.")

      ## do not compute anything more than once
      if (!is.null(private$artifacts.network.cochange)) {
        logging::logdebug("get.artifact.network.cochange: finished. (already existing)")
        return(private$artifacts.network.cochange)
      }

      artifacts.net = construct.dependency.network.from.list(private$proj.data$get.commit2artifact(), network.conf = private$network.conf,
                                                             directed = FALSE)

      ## store network
      private$artifacts.network.cochange = artifacts.net
      logging::logdebug("get.artifact.network.cochange: finished.")

      return(artifacts.net)
    },


    ## call-graph-based artifact network
    ## IMPORTANT: This only works for range-level analyses! (errors otherwise)
    get.artifact.network.callgraph = function() {

      logging::logdebug("get.artifact.network.callgraph: starting.")

      ## do not compute anything more than once
      if (!is.null(private$artifacts.network.callgraph)) {
        logging::logdebug("get.artifact.network.callgraph: finished. (already existing)")
        return(private$artifacts.network.callgraph)
      }

      ## check if revision for call-graphs is set
      if (is.na(private$revision.callgraph)) {
        logging::logerror("The call-graph revision is not set. Aborting...")
        logging::logerror("This may be due to project-level analysis.
                                   The call-graph data is only available in range-level analysis.")
        stop("Trying to get call-graph data before setting a revision.")
      }

      ## construct path and file
      file.dir = private$proj.data$get.data.path.callgraph()
      file.name = paste0("cg_nw_", private$proj.data$get.project.conf$get.entry("artifact.short"), "_", private$proj.data$get.revision.callgraph, ".net")
      file = file.path(file.dir, file.name)

      ## read network from disk
      artifacts.net = read.network.from.file(file)
      ## post-process network
      artifacts.net = postprocess.artifact.names.callgraph(artifacts.net, private$proj.data$get.project.conf$get.entry("artifact"))

      ## store network
      private$artifacts.network.callgraph = artifacts.net
      logging::logdebug("get.artifact.network.callgraph: finished.")

      return(artifacts.net)
    }

  ),
  ## public members ####
  public = list(
    ## constructor
    initialize = function(project.conf, network.conf) {
      private$proj.data = ProjectData$new(project.conf, network.conf)

      if(!missing(network.conf) && "NetworkConf" %in% class(network.conf)) {
        private$network.conf = network.conf
      }

      if (class(self)[1] == "ProjectData")
        logging::loginfo("Initialized data object %s", self$get.class.name())
    },
    ## RESET ENVIRONMENT ##

    # Reset cached data
    reset.environment = function() {
      private$authors.network.mail = NULL
      private$authors.network.cochange = NULL
      private$artifacts.network.cochange = NULL
      private$artifacts.network.callgraph = NULL
      private$proj.data$reset.environment()
    },

    ## CONFIGURATION ####

    # Get the current network configuration
    get.network.conf = function() {
      return(private$network.conf)
    },

    # Set the current network configuration to the given one.
    set.network.conf = function(network.conf) {
      private$network.conf = network.conf
      private$proj.data$set.network.conf(network.conf = network.conf)
      self$reset.environment()
    },

    ## UPDATE CONFIGURATION ####
    update.network.conf = function(updated.values = list()) {
      private$network.conf$update.values(updated.values = updated.values)

      self$reset.environment()
    },


    ## get the developer relation as network (generic)
    get.author.network = function() {
      logging::loginfo("Constructing author network.")

      ## construct network
      relation = private$network.conf$get.variable("author.relation")
      net = switch(
        relation,
        cochange =
          private$get.author.network.cochange(),
        mail =
          private$get.author.network.mail(),
        stop(sprintf("The author relation '%s' does not exist.", relation))
      )

      ## set vertex and edge attributes for identifaction
      igraph::V(net)$type = TYPE.AUTHOR
      igraph::E(net)$type = TYPE.EDGES.INTRA

      return(net)
    },

    ## get artifact relation as network (generic)
    get.artifact.network = function() {
      logging::loginfo("Constructing artifact network.")

      ## construct network
      relation = private$network.conf$get.variable("artifact.relation")

      net = switch(
        relation,
        cochange =
          private$get.artifact.network.cochange(),
        callgraph =
          private$get.artifact.network.callgraph(),
        stop(sprintf("The artifact relation '%s' does not exist.", relation))
      )

      ## set vertex and edge attributes for identifaction
      igraph::V(net)$type = TYPE.ARTIFACT
      igraph::E(net)$type = TYPE.EDGES.INTRA

      return(net)
    },

    ## get the (real) bipartite network
    get.bipartite.network = function() {
      ## authors-artifact relation
      authors.to.artifacts = self$get.author2artifact()

      ## extract vertices
      authors = names(authors.to.artifacts)
      artifacts = self$get.artifacts()

      ## construct networks from vertices
      authors.net = create.empty.network(directed = FALSE) +
        igraph::vertices(authors, name = authors, type = TYPE.AUTHOR)
      artifacts.net = create.empty.network(directed = FALSE) +
        igraph::vertices(artifacts, name = artifacts, type = TYPE.ARTIFACT)

      ## combine the networks
      u = combine.networks(authors.net, artifacts.net, authors.to.artifacts,
                           network.conf = private$network.conf)
      return(u)
    },

    ## get all networks (build unification to avoid null-pointers)
    get.networks = function() {
      logging::loginfo("Constructing all networks.")

      ## authors-artifact relation
      authors.to.artifacts = self$get.author2artifact()

      ## bipartite network
      bipartite.net = self$get.bipartite.network()

      ## authors relation
      authors.net = self$get.author.network()

      ## unify vertices with developer-artifact relation
      authors.from.net = igraph::get.vertex.attribute(authors.net, "name")
      authors.from.artifacts = names(authors.to.artifacts)
      authors.net = authors.net + igraph::vertices(setdiff(authors.from.artifacts, authors.from.net), type = TYPE.AUTHOR)

      ## remove all authors from the corresponding network who do not have touched any artifact
      if (private$network.conf$get.variable("author.only.committers") & !is.null(authors.from.artifacts)) {
        authors.net = igraph::delete.vertices(authors.net, setdiff(authors.from.net, authors.from.artifacts))
      }

      ## artifact relation
      artifacts.net = self$get.artifact.network()
      # merge vertices on artifact network to avoid NULL references
      artifacts.net = unify.artifact.vertices(artifacts.net, authors.to.artifacts)
      # ## compute communities # TODO in the end, this needs to read Thomas' files!
      # artifacts.comm = get.communities(artifact.net)

      return(list(
        "authors.to.artifacts" = authors.to.artifacts,
        "bipartite.net" = bipartite.net,
        "authors.net" = authors.net,
        "artifacts.net" = artifacts.net
      ))
    },

    ## get the multi networks (get.networks combined in one network)
    get.multi.network = function() {
      logging::loginfo("Constructing multi network.")

      ## construct the network parts we need for the multi network
      networks = self$get.networks()
      authors.to.artifacts = networks[["authors.to.artifacts"]]
      authors.net = networks[["authors.net"]]
      artifacts.net = networks[["artifacts.net"]]

      ## check directedness and adapt artifact network if needed
      if (igraph::is.directed(authors.net) && !igraph::is.directed(artifacts.net)) {
        logging::logwarn("Author network is directed, but artifact network is not. Converting artifact network...")
        artifacts.net = igraph::as.directed(artifacts.net, mode = "mutual")
      } else if (!igraph::is.directed(authors.net) && igraph::is.directed(artifacts.net)) {
        logging::logwarn("Author network is undirected, but artifact network is not. Converting artifact network...")
        contraction.mode = ifelse(
          private$network.conf$get.variable("contract.edges"),
          "collapse",
          "each"
        )
        artifacts.net = igraph::as.undirected(artifacts.net, mode = contraction.mode, edge.attr.comb = EDGE.ATTR.HANDLING)
      }

      ## reduce memory consumption by removing temporary data
      rm(networks)
      gc()

      ## combine the networks
      u = combine.networks(authors.net, artifacts.net, authors.to.artifacts,
                           network.conf = private$network.conf)

      return(u)
    }

  )
)


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Union of networks
##

## combine networks to a bipartite network
combine.networks = function(authors.net, artifacts.net, authors.to.artifacts, network.conf) {

  authors = igraph::get.vertex.attribute(authors.net, "name")
  artifacts = igraph::get.vertex.attribute(artifacts.net, "name")

  ## check emptiness of networks
  if (length(authors) == 0) {
    logging::logwarn("Author network is empty.")
  }
  if (length(artifacts) == 0) {
    logging::logwarn("Artifact network is empty.")
  }

  ## combine networks
  u = igraph::disjoint_union(authors.net, artifacts.net)

  ## add edges for devs.to.arts relation
  u = add.edges.for.devart.relation(u, authors.to.artifacts, network.conf = network.conf)

  ## simplify network
  if (network.conf$get.variable("simplify"))
    u = simplify.network(u)

  return(u)
}


## helper function to add dependencies from dev--art mapping to the bipartite network
add.edges.for.devart.relation = function(net, auth.to.arts, network.conf) {

  # construct edges (i.e., a vertex sequence with c(source, target, source, target, ...))
  vertex.sequence.for.edges = parallel::mcmapply(function(d, a.df) {
    a = a.df[["artifact"]]
    new.edges = lapply(a, function(art) {
      igraph::V(net)[d, art] # get two vertices from source network:  c(developer, artifact)
    })
    return(new.edges)
  }, names(auth.to.arts), auth.to.arts)

  ## get extra edge attributes
  extra.edge.attributes.df = parallel::mclapply(auth.to.arts, function(a.df) {
    cols.which = network.conf$get.variable("edge.attributes") %in% colnames(a.df)
    return(a.df[, network.conf$get.variable("edge.attributes")[cols.which], drop = FALSE])
  })
  extra.edge.attributes.df = plyr::rbind.fill(extra.edge.attributes.df)
  extra.edge.attributes.df["weight"] = 1 # add weight

  extra.edge.attributes = as.list(extra.edge.attributes.df)

  ## set edge type
  extra.edge.attributes = c(extra.edge.attributes, list(type = TYPE.EDGES.INTER))

  ## add the vertex sequences as edges to the network
  new.net = igraph::add_edges(net, unlist(vertex.sequence.for.edges), attr = extra.edge.attributes)

  return(new.net)

}
