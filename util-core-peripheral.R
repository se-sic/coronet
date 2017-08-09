## (c) Ferdinand Frank, 2017
## frankfer@fim.uni-passau.de
## (c) Claus Hunsen, 2017
## hunsen@fim.uni-passau.de
## (c) Mitchell Joblins, 2017
## mitchell.joblin@uni-passau.de
## (c) Sofie Kemper, 2017
## kemperso@fim.uni-passau.de

## This file is derived from following Codeface script:
## https://github.com/siemens/codeface/blob/master/codeface/R/developer_classification.r


### LIBRARIES

library(sqldf)
library(igraph)
library(markovchain)


### THRESHOLDS


## Defines at which percentage of the work load developers will
## be classified as core
CORE.THRESHOLD <- 0.8

## Defines the percentage of version development ranges in which
## a developer has to be classified as core to be stated as a
## longterm core developer
LONGTERM.CORE.THRESHOLD <- 0.5


### FUNCTIONS

## Classify the developers of the specified version range into core and peripheral
## based on the classification metric indicated by "type".
##
## For count-based network metrics, the raw data has to be given. For network-based metrics
## the graph is preferred to the raw data in case both are given.
get.developer.class.by.type = function(graph = NULL, data = NULL,
                                       type = c("networkDegree", "networkEigen", "commitCount", "locCount")){
    type = match.arg(type, c("networkDegree", "networkEigen", "commitCount", "locCount"))

    if(is.null(graph) && is.null(data)){
        stop("Either graph or raw data needs to be given.")
    }

    return(switch(type,
                  "networkDegree" = get.developer.class.network.degree(graph = graph, codefaceRangeData = data),
                  "networkEigen" = get.developer.class.network.eigen(graph = graph, codefaceRangeData = data),
                  "commitCount" = get.developer.class.commit.count(codefaceRangeData = data),
                  "locCount"= get.developer.class.loc.count(codefaceRangeData = data)))
}

### network-metric-based classification

## Classify the developers of the specified version range into core and peripheral
## based on the degree centrality.
##
## This function takes either a network OR the raw range data. In case both are given, the network is used.
get.developer.class.network.degree <- function(graph=NULL, codefaceRangeData=NULL, resultLimit=NULL) {

if(is.null(graph) && is.null(codefaceRangeData)){
    stop("Either the raw data or a network has to be given.")
}else if(is.null(graph)){
  ## Get co-change developer network
  graph <- codefaceRangeData$get.author.network()
}

  ## Get node degrees for all developers
  centrality.vec <- sort(igraph::degree(graph), decreasing=T)
  centrality.df <- data.frame(author.name=names(centrality.vec),
                              centrality=as.vector(centrality.vec))

  ## Get the developer classification based on the centrality
  res <- get.developer.class(centrality.df, 'centrality', resultLimit=resultLimit)

  return(res)
}

## Classify the developers of the specified version range into core and peripheral
## based on the eigenvector centrality.
##
## This function takes either a network OR the raw range data. In case both are given, the network is used.
get.developer.class.network.eigen <- function(graph=NULL, codefaceRangeData=NULL, resultLimit=NULL) {

    if(is.null(graph) && is.null(codefaceRangeData)){
        stop("Either the raw data or a network has to be given.")
    }else if(is.null(graph)){
        ## Get co-change developer network
        graph <- codefaceRangeData$get.author.network()
    }

  ## Get eigenvectors for all developers
  centrality.vec <- sort(igraph::eigen_centrality(graph)$vector, decreasing=T)

  ## In case no collaboration occured, all centrality values are set to 0
  if(igraph::ecount(graph) == 0){
      centrality.vec[1:length(centrality.vec)] <- rep(0, length(centrality.vec))
  }
  centrality.df <- data.frame(author.name=names(centrality.vec),
                              centrality=as.vector(centrality.vec))

  ## Get the developer classification based on the centrality
  res <- get.developer.class(centrality.df, 'centrality', resultLimit=resultLimit)

  return(res)
}

### count-based classification

## Classify the developers of the specified version range into core and peripheral
## based on the number of commits made withing a version range.
get.developer.class.commit.count <- function(codefaceRangeData, resultLimit=NULL, minValue=0) {

  ## Get the commit counts per developer
  author.commit.count <- get.author.commit.count(codefaceRangeData)

  ## Get the developer classification based on the commit counts
  res <- get.developer.class(author.commit.count, 'freq', resultLimit=resultLimit, minValue=minValue)

  return(res)
}

## Classify the developers of the specified version range into core and peripheral
## based on the sum of added and deleted lines of code a developer has committed within a version range.
get.developer.class.loc.count <- function(codefaceRangeData, resultLimit=NULL, minValue=0) {

  ## Get the changed lines (loc counts) per developer
  author.loc.count <- get.author.loc.count(codefaceRangeData)

  ## Get the developer classification based on the loc counts
  res <- get.developer.class(author.loc.count, 'loc', resultLimit=resultLimit, minValue=minValue)

  return(res)
}

## Get the changed lines per developer of the specified version range
## as a data frame ordered by the changed lines.
get.author.loc.count <- function(codefaceRangeData) {

  ## Get commit data
  commits.df <- get.commit.data(codefaceRangeData,
                                columns=c("author.name", "author.email", "added.lines", "deleted.lines"))[[1]]

  ## Execute a query to get the changed lines per developer
  res <- sqldf("select `author.name`, `author.email`, SUM(`added.lines`) + SUM(`deleted.lines`) as `loc`
               from `commits.df` group by `author.name` order by `loc` desc")

  return(res)
}

## Get the commit count per developer of the specified version range
## as a data frame ordered by the commit count.
get.author.commit.count <- function(codefaceRangeData) {

  ## Get commit data
  commits.df <- get.commit.data(codefaceRangeData)[[1]]

  ## Execute a query to get the commit count per developer
  res <- sqldf("select *, COUNT(*) as `freq` from `commits.df` group by `author.name` order by `freq` desc")

  return(res)
}

## Get the loc count threshold of the specified version range
## on which a developer can be classified as core.
get.loc.count.threshold <- function(codefaceRangeData) {

  ## Get the loc per developer
  author.loc.count <- get.author.loc.count(codefaceRangeData)

  return(get.threshold(author.loc.count$loc))
}

## Get the commit count threshold  of the specified version range
## on which a developer can be classified as core.
get.commit.count.threshold <- function(codefaceRangeData) {

  ## Get the commit counts per developer
  author.commit.count <- get.author.commit.count(codefaceRangeData)

  return(get.threshold(author.commit.count$freq))
}

## Get the commit data with the specified columns for the specified version range as a data frame
## for each specified split range.
## A split interval can be set by defining the number of weeks for each requested range as a vector.
get.commit.data <- function(codefaceRangeData, columns=c("author.name", "author.email"), split=c()) {

  ## Get commit data
  commits.df <- codefaceRangeData$get.commits.raw()

  ## Make sure the hash is included in the cut columns vector for grouping
  cut.columns = columns
  if (!("hash" %in% cut.columns)) {
    cut.columns <- c(cut.columns, "hash")
  }

  ## Make sure the date is included in the cut columns vector for splitting
  if (!("date" %in% cut.columns)) {
    cut.columns <- c(cut.columns, "date")
  }

  ## Cut down data to needed minimum
  commits.df <- commits.df[cut.columns]

  ## Group by hash to get a line per commit
  commits.df <- sqldf("select * from `commits.df` group by `hash`")

  ## Remove hash column if not wanted as it now contains nonsensical data
  if (!("hash" %in% columns)) {
    commits.df["hash"] <- NULL
  }

  ## Order commits by date column
  commits.df <- commits.df[order(commits.df$date),]

  ## Fetch the date range info
  date.first <- commits.df$date[1] - 1 # -1 because the first date in split ranges is exclusive
  date.last <- commits.df$date[nrow(commits.df)]

  ## Calc the split dates depending on the specified intervals
  date.split <- c(date.last)
  if (!is.null(split)) {
    for (i in 1:length(split)){
      date.calc <- date.split[i] - (split[i] * 7)

      ## Check if calculated date is still after the first commit date of the range
      if (date.calc > date.first) {
        date.split <- c(date.split, date.calc)
      } else {
        date.split <- c(date.split, date.first)
        break
      }
    }
  } else {
    date.split <- c(date.split, date.first)
  }

  date.split <- rev(date.split)

  ## Only keep the commits which were made within the specified split ranges
  commits.df <- commits.df[commits.df$date > date.split[1],]

  ## Calc group numbers for the commits by the split dates
  split.groups <- c()
  for(i in 1:nrow(commits.df)){
    commits.df.row.date <- commits.df[i,]$date
    for(j in 2:length(date.split)){
      if (commits.df.row.date <= date.split[j]) {
        split.groups <- c(split.groups, paste(date.split[j - 1] + 1, date.split[j], sep=" - "))
        break
      }
    }
  }

  ## Remove date column if not wanted as it now contains nonsensical data
  if (!("date" %in% columns)) {
    commits.df["date"] <- NULL
  }

  ## Split the commits by the calculated groups
  res <- split(commits.df, split.groups)

  return(res)
}

### utility functions

## Classify the specified set of developers/authors into core and peripheral based on the data
## of the specified data frame column name (calcBaseName). Only authors which have a higher
## value than specified (minValue) are included in the result.
## Core developers are those which are responsible for a given
## percentage of the work load with a default threshold set at 80% according
## to Ref: Terceiro A, Rios LR, Chavez C (2010) An empirical study on
##         the structural complexity introduced by core and peripheral
##         developers in free software projects.
get.developer.class <- function(authorDataFrame, calcBaseName, resultLimit=NULL, minValue=0) {

  ## Make sure the provided data is ordered correctly by the calculation base
  author.data <- authorDataFrame[rev(order(authorDataFrame[[calcBaseName]])),]

  ## Remove rows with invalid calculation base values
  author.data <- author.data[!is.na(author.data[calcBaseName]),]

  ## Get the threshold depending on all calculation base values
  developer.class.threshold <- get.threshold(author.data[[calcBaseName]])

  ## Only include authors with the specified minimum of the calculation base value
  author.data <- author.data[author.data[[calcBaseName]] >= minValue, ]

  ## Check if the result shall be limited
  if (!is.null(resultLimit)) {
    author.data <- head(author.data, resultLimit)
  }

  ## Check which developers can be treated as core based on the calculation base values
  core.test <- cumsum(author.data[[calcBaseName]]) < developer.class.threshold

  ## If we have not found a core developer, the author with the highest calculation base value
  ## will be treated as core, to return at least one core developer. The only exception is the
  ## case that no activity/collaboration occured. Then, all developers are classified as peripheral.
  if(developer.class.threshold==0){
      core.test <- rep(FALSE, length(core.test))
  } else if (!any(core.test)) {
    core.test <- c(TRUE, rep(FALSE, length(core.test) - 1))
  }

  ## Cut core and peripheral developers from base data and construct return value
  core.developers <- author.data[core.test,]
  peripheral.developers <- author.data[!core.test,]
  res <- list(core=core.developers, peripheral=peripheral.developers)

  return(res)
}

## Get the threshold based on the specified integer data list
## on which a developer can be classified as core.
get.threshold <- function(dataList) {

  ## Calculate the sum of the provided data as base for the threshold calculation
  data.threshold.base <- sum(dataList)

  ## Check which developers can be treated as core based on the data
  data.threshold <- round(CORE.THRESHOLD * data.threshold.base)
  core.test <- cumsum(dataList) < data.threshold

  # If we have not found a core dev, the author with the highest data value
  # marks the threshold
  if (!any(core.test)) {
    data.threshold <- max(dataList)
  }

  return(data.threshold)
}

## Get a data frame with the authors and their occurence count in the specified class for
## the specified developer classification list.
get.recurring.authors <- function(developerClassOverview, class = c("both", "core", "peripheral")) {
    class <- match.arg(class, c("both", "core", "peripheral"))

    authors <- c()
    freq <- c()

    ## Iterate over each version development range
    for(i in 1:length(developerClassOverview)){

        ## skip range in case no classification is available
        if(all(is.na(developerClassOverview[[i]]))){
            next
        }

        if (class == "both") {

            ## skip range in case no classification is available
            if(nrow(developerClassOverview[[i]]$core) == 0
               && nrow(developerClassOverview[[i]]$peripheral) == 0){
                next
            }

            developer.class.authors <- c(developerClassOverview[[i]]$core$author.name,
                                         developerClassOverview[[i]]$peripheral$author.name)
        } else {

            ## skip range in case no classification for the given class is available
            if(nrow(developerClassOverview[[i]][[class]])==0){
                next
            }

            developer.class.authors <- developerClassOverview[[i]][[class]]$author.name
        }

        ## Iterate over each author in the specified class and increase his occurence count
        for(j in 1:length(developer.class.authors)){
            developer.class.author.name <- developer.class.authors[j]

            ## Check if the author already exists in previous ranges
            developer.class.author.index <- which(authors == developer.class.author.name)
            if (length(developer.class.author.index) > 0) {

                ## Increase the occurence count as the author already exists in previous ranges
                freq[developer.class.author.index] <- freq[developer.class.author.index] + 1
            } else {

                ## Save the author and its first occurence
                authors <- c(authors, developer.class.author.name)
                freq <- c(freq, 1)
            }
        }
    }

    data <- data.frame(
        author.name = authors,
        freq = freq
    )

    ## Sort the authors by occurence count
    data <- data[order(data$freq, decreasing = TRUE),]

    return(data)
}


### Non-classification-type-specific functionality


## Classify the developers of all specified version ranges into core and peripheral
## based on the specified classification function.
##
## The data can either be given as list of raw range data or as list of networks (only
## for the network-based metrics). In case both are given, the raw data is used.
get.developer.class.overview <- function(graphList = NULL, codefaceRangeDataList = NULL, type =
                                             c("networkDegree", "networkEigen", "commitCount", "locCount")) {

    type <- match.arg(type, c("networkDegree", "networkEigen", "commitCount", "locCount"))

    if(is.null(graphList) && is.null(codefaceRangeDataList)){
        stop("Either graph or raw data has to be given.")

    }else if(is.null(codefaceRangeDataList) && (type == "commitCount" || type == "locCount")){
        stop("For the count-based metrics, the raw data has to be given.")
    }

    if(!is.null(codefaceRangeDataList)){## raw data is preferred
  res <- list()
  for (i in 1:length(codefaceRangeDataList)) {
    range.data <- codefaceRangeDataList[[i]]

    ## Get classification data of the current range
    range.class <- get.developer.class.by.type(data = range.data, type = type)
    res <- c(res, list(range.class))
  }
    }else{## use graph list as data
        res <- list()
        for (i in 1:length(graphList)) {
            range.graph <- graphList[[i]]

            ## Get classification data of the current range
            range.class <- get.developer.class.by.type(graph = range.graph, type = type)
            res <- c(res, list(range.class))
        }
    }

  return(res)
}

## Retrieves all developers which will be classified as core by the specified
## classification function "type" in more than a certain number of version ranges
## -> see: 'LONGTERM.CORE.THRESHOLD'.
##
## The data can either be given as list of raw range data or as list of networks (only
## for the network-based metrics). In case both are given, the raw data is used.
get.longterm.core.developers <- function(graphList = NULL, codefaceRangeDataList = NULL, type =
                                             c("networkDegree", "networkEigen", "commitCount", "locCount")) {

    type <- match.arg(type, c("networkDegree", "networkEigen", "commitCount", "locCount"))

    if(is.null(graphList) && is.null(codefaceRangeDataList)){
        stop("Either graph or raw data has to be given.")

    }else if(is.null(codefaceRangeDataList) && (type == "commitCount" || type == "locCount")){
        stop("For the count-based metrics, the raw data has to be given.")
    }

  ## Get the classifications for all ranges
  developer.class <- get.developer.class.overview(graphList = graphList,
                                                  codefaceRangeDataList = codefaceRangeDataList,
                                                  type = type)

  ## Get a list with the occurence freq for each core developer
  recurring.developers <- get.recurring.authors(developer.class, class = "core")

  ## Calculate the num of occurences at which a dev gets stated as longterm core
  longterm.threshold <- length(codefaceRangeDataList) * LONGTERM.CORE.THRESHOLD

  ## Get the longterm core developers
  return(recurring.developers[recurring.developers$freq >= longterm.threshold,]$author.name)
}

## Get a markov chain object representing the role stability of the
## specified classification overview.
get.role.stability <- function(developerClassOverview) {
  core.core <- 0 # core in prev version and core in current version
  core.peripheral <- 0 # core in prev version and peripheral in current version
  core.absent <- 0 # core in prev version and absent in current version
  peripheral.core <- 0 # peripheral in prev version and core in current version
  peripheral.peripheral <-0 # peripheral in prev version and peripheral in current version
  peripheral.absent <- 0 # peripheral in prev version and absent in current version
  absent.core <- 0 # absent in prev version and core in current version
  absent.peripheral <- 0 # absent in prev version and peripheral in current version
  absent.absent <- 0 # absent in prev version and absent in current version

  dev.current.absent <- c()

  ## Run through each version range developer classification
  for (i in 2:length(developerClassOverview)) {

    ## Get core and peripheral devs from previous version
    class.prev <- developerClassOverview[[i-1]]
    dev.prev.core <- class.prev$core$author.name
    dev.prev.peripheral <- class.prev$peripheral$author.name
    dev.prev <- c(dev.prev.core, dev.prev.peripheral)
    dev.prev.absent <- dev.current.absent

    ## Get core and peripheral devs from current version
    class.current <- developerClassOverview[[i]]
    dev.current.core <- class.current$core$author.name
    dev.current.peripheral <- class.current$peripheral$author.name
    dev.current <- c(dev.current.core, dev.current.peripheral)
    dev.current.absent <- unique(c(dev.prev.absent[!(dev.prev.absent %in% dev.current)], dev.prev[!(dev.prev %in% dev.current)]))

    ## Add the transition numbers of core developers
    core.core <- core.core + sum(dev.prev.core %in% dev.current.core)
    core.peripheral <- core.peripheral + sum(dev.prev.core %in% dev.current.peripheral)
    core.absent <- core.absent + sum(dev.prev.core %in% dev.current.absent)

    ## Add the transition numbers of peripheral developers
    peripheral.core <- peripheral.core + sum(dev.prev.peripheral %in% dev.current.core)
    peripheral.peripheral <- peripheral.peripheral + sum(dev.prev.peripheral %in% dev.current.peripheral)
    peripheral.absent <- peripheral.absent + sum(dev.prev.peripheral %in% dev.current.absent)

    ## Add the transition numbers of absent developers
    absent.core <- absent.core + sum(dev.prev.absent %in% dev.current.core)
    absent.peripheral <- absent.peripheral + sum(dev.prev.absent %in% dev.current.peripheral)
    absent.absent <- absent.absent + sum(dev.prev.absent %in% dev.current.absent)
  }

  ## Calculate the percentage of the core developer transitions
  core.transition.sum <- sum(core.core, core.peripheral, core.absent)
  core.core.rel <- core.core / core.transition.sum
  core.peripheral.rel <- core.peripheral / core.transition.sum
  core.absent.rel <- core.absent / core.transition.sum

  ## Calculate the percentage of the peripheral developer transitions
  peripheral.transition.sum <- sum(peripheral.core, peripheral.peripheral, peripheral.absent)
  peripheral.core.rel <- peripheral.core / peripheral.transition.sum
  peripheral.peripheral.rel <- peripheral.peripheral / peripheral.transition.sum
  peripheral.absent.rel <- peripheral.absent / peripheral.transition.sum

  ## Calculate the percentage of the absent developer transitions
  absent.transition.sum <- sum(absent.core, absent.peripheral, absent.absent)
  absent.core.rel <- ifelse(absent.transition.sum > 0, absent.core / absent.transition.sum, 0)
  absent.peripheral.rel <- ifelse(absent.transition.sum > 0, absent.peripheral / absent.transition.sum, 0)
  ## set to 1 because if all absent authors remain absent if there are never transition from absent-state
  absent.absent.rel <- ifelse(absent.transition.sum > 0, absent.absent / absent.transition.sum, 1)

  ## Build the markov chain
  roles <- c("Core", "Peripheral", "Absent")
  roles.matrix <- matrix(data = c(core.core.rel, core.peripheral.rel, core.absent.rel,
                                  peripheral.core.rel, peripheral.peripheral.rel, peripheral.absent.rel,
                                  absent.core.rel, absent.peripheral.rel, absent.absent.rel),
                         byrow = TRUE, nrow = 3, dimnames = list(roles, roles))

  roles.stability <- new("markovchain", states = roles, byrow = TRUE,
                         transitionMatrix = roles.matrix, name = "Role Stability")

  return(roles.stability)
}

## Get the number/activity of core and peripheral developers based on the specified classification function
## for each specified split range and for each version development range.
## An individual split range can be set for each version as a list of vectors with the version name as the key.
## An integer value can be set as 'slidingWindowCore' to specify, that the core developers of the last
## version ranges (according to the value) shall be included in the core developer set of the current range.
get.developer.class.activity.overview <- function(codefaceRangeDataList = NULL,
                                                  developer.class.overview = NULL,
                                                  split=c(),
                                                  slidingWindowCore=NULL,
                                                  longterm.cores = c(),
                                                  activityMeasure = c("commit.count", "loc.count")) {

activityMeasure <- match.arg(activityMeasure, c("commit.count", "loc.count"))

if(is.null(codefaceRangeDataList)) {
    stop("Raw data is needed for the activity analysis.")
}

if(is.null(developer.class.overview)) {
    stop("Developer classification has to be given.")
}

if(length(codefaceRangeDataList) != length(developer.class.overview)) {
    stop("Raw data and developer classification have to match.")
}

  res <- list()
  for (i in 1:length(codefaceRangeDataList)){

    ## Check if an individual split for each version range is set
    if (class(split) == 'list') {
      range.split <- split[[i]]
    } else {
      range.split <- split
    }

    additional.core <- longterm.cores

    ## Add the additional core developers according to the specified sliding window length,
    ## if one is specified
    if (!is.null(slidingWindowCore) && i > 1) {
      sliding.window.start <- ifelse(i - slidingWindowCore > 1, i - slidingWindowCore, 1)

      for (j in sliding.window.start:(i-1)) {
        additional.core <- c(additional.core, developer.class.overview[[j]]$core$author.name)
      }
    }

    res[[i]] <- get.developer.class.activity(codefaceRangeDataList[[i]],
                                                         developer.class = developer.class.overview[[i]],
                                                         activityMeasure=activityMeasure,
                                                         split=range.split,
                                                         additionalCores=additional.core)
  }

  return(res)
}

## Get the number/activity of core and peripheral developers based on the specified classification function
## for each split range of the specified version development range.
## A split interval can be set by defining the number of weeks for each requested range as a vector.
## A vector of addition core developers can be specified which will always be set as core in each range.
get.developer.class.activity <- function(codefaceRangeData = NULL,
                                         developer.class = NULL,
                                         activityMeasure = c("commit.count", "loc.count"),
                                         split=c(),
                                         additionalCores=NULL) {

    activityMeasure <- match.arg(activityMeasure, c("commit.count", "loc.count"))

    if(is.null(codefaceRangeData)){
        stop("Raw data is needed for the activity analysis.")
    }
    if(is.null(developer.class)){
        stop("Developer classification has to be given by the user")
    }


  ## Return NA in case no classification information is available
  if(all(is.na(developer.class))
     || (nrow(developer.class$core) + nrow(developer.class$peripheral) == 0)) {
      return(NA)
  }

  developer.core <- unique(c(developer.class$core$author.name, additionalCores))# TODO works with 0 rows?

  ## Get the splitted commit data with all necessary columns
  commits.data <- get.commit.data(codefaceRangeData,
                                  columns=c("author.name", "added.lines", "deleted.lines"),
                                  split=split)

  ## Build the query string to group commits by the author name
  commits.query <- "select `author.name`, SUM(`added.lines`) + SUM(`deleted.lines`) as `loc.count`,
  COUNT(*) as `commit.count` from `commits.df` group by `author.name`"

  ## Get the developers with their commit count and corresponding class for each splitted range
  commits.dev.list = list()
  for (i in 1:length(commits.data)){
    commits.df <- commits.data[[i]]
    commits.dev.list[[names(commits.data)[i]]] <- sqldf(commits.query)

    ## Classify developers in splitted range according to the overall classification
    core.test <- commits.dev.list[[names(commits.data)[i]]]$author.name %in% developer.core
    commits.dev.core <- commits.dev.list[[names(commits.data)[i]]][core.test,]
    commits.dev.peripheral <- commits.dev.list[[names(commits.data)[i]]][!core.test,]

    commits.dev.list[[names(commits.data)[i]]] <- list(core=commits.dev.core, peripheral=commits.dev.peripheral)
  }

  res.range <- c()
  res.devs <- c()
  res.core <- c()
  res.peripheral <- c()

  res.activity.count <- c()
  res.activity.count.core <- c()
  res.activity.count.peripheral <- c()
  res.activity.count.avg.core <- c()
  res.activity.count.avg.peripheral <- c()
  res.activity.count.med.core <- c()
  res.activity.count.med.peripheral <- c()
  res.activity.count.norm.weeks.core <- c()
  res.activity.count.norm.weeks.peripheral <- c()

  ## Count the number of core and peripheral developers for each splitted range
  for (i in 1:length(commits.dev.list)){
    commits.dev <- commits.dev.list[[i]]
    res.range[i] <- names(commits.dev.list)[i]

    res.core[i] <- nrow(commits.dev$core)
    res.peripheral[i] <- nrow(commits.dev$peripheral)
    res.devs[i] <- res.core[i] + res.peripheral[i]

    res.activity.count.core[i] <- sum(commits.dev$core[[activityMeasure]])
    res.activity.count.peripheral[i] <- sum(commits.dev$peripheral[[activityMeasure]])
    res.activity.count[i] <- res.activity.count.core[i] + res.activity.count.peripheral[i]

    ## Get average activity count
    num.core.dev <- ifelse(res.core[i] > 0, res.core[i], 1)
    num.peripheral.dev <- ifelse(res.peripheral[i] > 0, res.peripheral[i], 1)
    res.activity.count.avg.core[i] <- res.activity.count.core[i] / num.core.dev
    res.activity.count.avg.peripheral[i] <- res.activity.count.peripheral[i] / num.peripheral.dev

    ## Get median activity count
    activity.count.core.ordered <- commits.dev$core[order(commits.dev$core[[activityMeasure]]),][[activityMeasure]]
    activity.count.peripheral.ordered <- commits.dev$peripheral[order(commits.dev$peripheral[[activityMeasure]]),][[activityMeasure]]
    res.activity.count.med.core[i] <- ifelse(length(activity.count.core.ordered) > 0, median(activity.count.core.ordered), 0)
    res.activity.count.med.peripheral[i] <- ifelse(length(activity.count.peripheral.ordered) > 0, median(activity.count.peripheral.ordered), 0)

    ## Normalize activity count by weeks
    res.range.splitted <- strsplit(res.range[i], " - ")[[1]]
    res.range.start <- as.Date(res.range.splitted[1])
    res.range.end <- as.Date(res.range.splitted[2])
    res.range.length.days <- as.numeric(res.range.end - res.range.start)
    res.range.length.weeks <- round(res.range.length.days / 7)

    res.activity.count.norm.weeks.core[i] <- res.activity.count.core[i] / res.range.length.weeks
    res.activity.count.norm.weeks.peripheral[i] <- res.activity.count.peripheral[i] / res.range.length.weeks
  }

  ## Build the data frame as the result
  res <- data.frame(
    range = res.range,
    devs = res.devs,
    devs.core = res.core,
    devs.peripheral = res.peripheral,
    activity.count = res.activity.count,
    activity.count.core = res.activity.count.core,
    activity.count.peripheral = res.activity.count.peripheral,
    activity.count.avg.core = res.activity.count.avg.core,
    activity.count.avg.peripheral = res.activity.count.avg.peripheral,
    activity.count.med.core = res.activity.count.med.core,
    activity.count.med.peripheral = res.activity.count.med.peripheral,
    activity.count.norm.weeks.core = res.activity.count.norm.weeks.core,
    activity.count.norm.weeks.peripheral = res.activity.count.norm.weeks.peripheral
  )

  return(res)
}

## Calculates the cohen's kappa to measure the agreement of the specified developer classifications.
calculate.cohens.kappa <- function(developerClassificationList, comparingDeveloperClassificationList) {
  num.core.core <- 0 # core in first, core in second
  num.core.peripheral <- 0 # core in first, peripheral in second
  num.peripheral.core <- 0 # peripheral in first, core in second
  num.peripheral.peripheral <- 0 # peripheral in first, peripheral in second

  ## Calculate the sums of equal classifications
  for(i in 1:length(developerClassificationList)){
    developer.class <- developerClassificationList[[i]]
    developer.class.compare <- comparingDeveloperClassificationList[[i]]

    num.core.core <- num.core.core +
      sum(developer.class$core$author.name %in% developer.class.compare$core$author.name == TRUE)

    num.core.peripheral <- num.core.peripheral +
      sum(developer.class$core$author.name %in% developer.class.compare$peripheral$author.name == TRUE)

    num.peripheral.core <- num.peripheral.core +
      sum(developer.class$peripheral$author.name %in% developer.class.compare$core$author.name == TRUE)

    num.peripheral.peripheral <- num.peripheral.peripheral +
      sum(developer.class$peripheral$author.name %in% developer.class.compare$peripheral$author.name == TRUE)
  }

  num.sum <- num.core.core + num.peripheral.peripheral + num.core.peripheral + num.peripheral.core

  po <- (num.core.core + num.peripheral.peripheral) / num.sum
  pe.core <- ((num.core.core + num.core.peripheral) / num.sum) * ((num.core.core + num.peripheral.core) / num.sum)
  pe.peripheral <- ((num.peripheral.peripheral + num.core.peripheral) / num.sum) * ((num.peripheral.peripheral + num.peripheral.core) / num.sum)
  pe <- pe.core + pe.peripheral

  kappa <- (po - pe) / (1 - pe)
  return(kappa)
}

## Get the developer turnover values measured as the proportion of developers in the
## specified version range classes which were not active, i.e. do not exist,
## in the previous version range classes (saturation).
get.class.turnover.overview = function(developerClassOverview, saturation = 1){


    if(!is.null(names(developerClassOverview))){
        versions <- names(developerClassOverview)
    }else{
        versions <- 1:length(developerClassOverview)
    }

  ## Set up the data.frame for the analysis results
  turnover.overview <- data.frame(
    versions = versions,
    row.names = 1,
    turnover = 0,
    turnover.core = 0,
    turnover.peripheral = 0,
    dev.count = 0,
    dev.count.core = 0,
    dev.count.peripheral = 0
  )

  ## Get all active developers for each version range in the different classes (and both)
  devs <- sapply(developerClassOverview, function(developer.class) {
    return(c(developer.class$core$author.name, developer.class$peripheral$author.name))
  })
  devs.core <- sapply(developerClassOverview, function(developer.class) {
    return(developer.class$core$author.name)
  })
  devs.peripheral <- sapply(developerClassOverview, function(developer.class) {
    return(developer.class$peripheral$author.name)
  })

  ## The developer turnover measured as the proportion of devs in the current version
  ## range which were not active in the previous version range
  devs.new <- devs[[1]]
  devs.core.new <- devs.core[[1]]
  devs.peripheral.new <- devs.peripheral[[1]]
  turnover.overview$dev.count[1] <- length(devs.new)
  turnover.overview$dev.count.core[1] <- length(devs.core.new)
  turnover.overview$dev.count.peripheral[1] <- length(devs.peripheral.new)
  for (i in 2:length(developerClassOverview)){
    devs.old <- devs.new
    devs.core.old <- devs.core.new
    devs.peripheral.old <- devs.peripheral.new

    j <- 1
    while (j <= saturation) {
      if ((i-j) > 0) {
        devs.old <- devs.old %u% devs[[i-j]]
        devs.core.old <- devs.core.old %u% devs.core[[i-j]]
        devs.peripheral.old <- devs.peripheral.old %u% devs.peripheral[[i-j]]
      }
      j <- j + 1
    }

    ## Find the developers which are active in the current period
    devs.new <- devs[[i]]
    devs.core.new <- devs.core[[i]]
    devs.peripheral.new <- devs.peripheral[[i]]

    ## Calculate the turnover values
    turnover.overview$turnover[i] <- sum(!(devs.new %in% devs.old)) / length(devs.new)
    turnover.overview$turnover.core[i] <- sum(!(devs.core.new %in% devs.core.old)) / length(devs.core.new)
    turnover.overview$turnover.peripheral[i] <- sum(!(devs.peripheral.new %in% devs.peripheral.old)) / length(devs.peripheral.new)

    turnover.overview$dev.count[i] <- length(devs.new)
    turnover.overview$dev.count.core[i] <- length(devs.core.new)
    turnover.overview$dev.count.peripheral[i] <- length(devs.peripheral.new)
  }

  return(turnover.overview)
}

## Gets a data frame to show the proportion of
## the developers which are either only active in the current version range but not in the previous ones (new) or
## which are only active in the previous ranges (as specified in saturation) but not in the current one (gone) in
## relation to all developers of the current and the previous ranges.
get.unstable.developers.overview = function(developerClassOverview, saturation = 1){

    if(!is.null(names(developerClassOverview))){
        versions <- names(developerClassOverview)
    }else{
        versions <- 1:length(developerClassOverview)
    }

  ## Set up the data.frame for the analysis results
  turnover.overview <- data.frame(
    versions = versions,
    row.names = 1,
    unstable = 0,
    unstable.core = 0,
    unstable.peripheral = 0
  )

  ## Get all active developers for each version range in the different classes (and both)
  devs <- sapply(developerClassOverview, function(developer.class) {
    return(c(developer.class$core$author.name, developer.class$peripheral$author.name))
  })
  devs.core <- sapply(developerClassOverview, function(developer.class) {
    return(developer.class$core$author.name)
  })
  devs.peripheral <- sapply(developerClassOverview, function(developer.class) {
    return(developer.class$peripheral$author.name)
  })

  devs.current <- devs[[1]]
  devs.core.current <- devs.core[[1]]
  devs.peripheral.current <- devs.peripheral[[1]]
  for (i in 2:length(developerClassOverview)){
    devs.prev <- devs.current
    devs.core.prev <- devs.core.current
    devs.peripheral.prev <- devs.peripheral.current

    j <- 1
    while (j <= saturation) {
      if ((i-j) > 0) {
        devs.prev <- devs.prev %u% devs[[i-j]]
        devs.core.prev <- devs.core.prev %u% devs.core[[i-j]]
        devs.peripheral.prev <- devs.peripheral.prev %u% devs.peripheral[[i-j]]
      }
      j <- j + 1
    }

    ## Find the developers which are active in the current period
    devs.current <- devs[[i]]
    devs.core.current <- devs.core[[i]]
    devs.peripheral.current <- devs.peripheral[[i]]

    ## Find the union of the devs which are active in the current period and in the prev periods
    devs.union <- devs.current %u% devs.prev
    devs.core.union <- devs.core.current %u% devs.core.prev
    devs.peripheral.union <- devs.peripheral.current %u% devs.peripheral.prev

    ## Find the devs which are only active in the current range but not in the previous ones
    devs.new <- sum(!(devs.current %in% devs.prev))
    devs.core.new <- sum(!(devs.core.current %in% devs.core.prev))
    devs.peripheral.new <- sum(!(devs.peripheral.current %in% devs.peripheral.prev))

    ## Find the devs which are only active in the previous ranges but not in the current one
    devs.gone <- sum(!(devs.prev %in% devs.current))
    devs.core.gone <- sum(!(devs.core.prev %in% devs.core.current))
    devs.peripheral.gone <- sum(!(devs.peripheral.prev %in% devs.peripheral.current))

    ## Calculate the ratio values
    turnover.overview$unstable[i] <- (devs.new + devs.gone) / length(devs.union)
    turnover.overview$unstable.core[i] <- (devs.core.new + devs.core.gone) / length(devs.core.union)
    turnover.overview$unstable.peripheral[i] <- (devs.peripheral.new + devs.peripheral.gone) / length(devs.peripheral.union)

  }

  return(turnover.overview)
}
