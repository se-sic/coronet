## (c) Raphael Nömmer, 2017
## noemmer@fim.uni-passau.de

## (c) Christian Hechtl 2017
## hechtl@fim.uni-passau.de

read.commits.raw = function(data.path, artifact) {

    logging::logdebug("read.commits.raw: starting.")

    commits.raw = NULL

    file = file.path(data.path, "commits.list")

    ## read data.frame from disk (as expected from save.list.to.file) [can be empty]
    commit.data <- try(read.table(file, header = FALSE, sep = ";", strip.white = TRUE,
                                  fileEncoding = "latin1", encoding = "utf8"), silent = TRUE)

    ## break if the list of commits is empty
    if (inherits(commit.data, 'try-error')) {
        logging::logwarn("There are no commits available for the current environment.")
        ##logging::logwarn("Class: %s", self$get.class.name())               ??????
        # logging::logwarn("Configuration: %s", private$project.conf$get.conf.as.string())
        commits.raw = data.frame()
        return()
    }

    ## set proper column names based on Codeface extraction:
    ##
    ## SELECT c.id, c.authorDate, a.name, a.email1, c.commitHash,
    ## c.ChangedFiles, c.AddedLines, c.DeletedLines, c.DiffSize,
    ## cd.file, cd.entityId, cd.entityType, cd.size
    colnames(commit.data) = c(
        "id", # id
        "date", "author.name", "author.email", # author information
        "hash", "changed.files", "added.lines", "deleted.lines", "diff.size", # commit information
        "file", "artifact", "artifact.type", "artifact.diff.size" ## commit-dependency information
    )

    ## rewrite data.frame when we want file-based data
    ## (we have proximity-based data as foundation)
    if (artifact == "file") {
        ## aggregate diff size by hash and file
        commit.data = sqldf::sqldf("select *, sum(`artifact.diff.size`) as diffsum from `commit.data`
                                   group by hash, file order by `date`, `author.name`, `id`, `file`, `artifact`")

        ## fix column class for diffsum
        commit.data["diffsum"] = as.numeric(commit.data[["diffsum"]])

        ## copy columns to match proper layout for further analyses
        commit.data["artifact"] = commit.data[["file"]]
        commit.data["artifact.type"] = "File"
        commit.data["artifact.diff.size"] = commit.data[["diffsum"]]
        commit.data["diffsum"] = NULL # remove
    }

    ## rewrite data.frame when we want function-based data
    ## (we have proximity-based data as foundation)
    if (artifact == "function") {
        ## artifact = file name + "::" . function name
        artifacts.new = paste(commit.data[["file"]], commit.data[["artifact"]], sep = "::")

        ## clean up empty artifacts and File_Level artifact
        artifacts.new = gsub("^::$", "", artifacts.new)
        artifacts.new = gsub("^(.*)::File_Level$", "File_Level", artifacts.new)

        ## insert new artifact names into commit table
        commit.data["artifact"] = artifacts.new
    }

    ## convert dates and sort by them
    commit.data[["date"]] = as.POSIXct(commit.data[["date"]])
    commit.data = commit.data[order(commit.data[["date"]], decreasing = FALSE), ] # sort!


    ## store the commit data
    commits.raw = commit.data
    logging::logdebug("read.commits.raw: finished.")
    return(commits.raw)
}

## read the synchronicity data of commits
read.synchronicity = function(data.path, artifact, time.window){
    logging::logdebug("read.synchronicity: starting.")

    synchronicity = NULL;

    ## check time.window
    allowed.time.windows = c(1, 5, 10)
    stopifnot((time.window) %in% allowed.time.windows)

    ## construct file
    file.name = sprintf("commit_sync_analysis_%ss_%s.dat", artifact, time.window)
    file = file.path(data.path, file.name)

    ## break if file does not exist
    if(!file.exists(file)) {
        logging::logwarn("There are no synchronicity data available for the current environment.")
        ##logging::logwarn("Class: %s", self$get.class.name()) ????
        synchronicity = data.frame()
        return(synchronicity)
    }

    ## load commit.ids object
    load(file = file)
    synchronous.commits = data.frame(hash = commit.hashes[["synchronous"]], synchronous = TRUE)
    nonsynchronous.commits = data.frame(hash = commit.hashes[["non.synchronous"]], synchronous = FALSE)

    ## construct data.frame
    synchronicity = plyr::rbind.fill(synchronous.commits, nonsynchronous.commits)

    ## store the synchronicity data
    logging::logdebug("read.synchronicity: finished.")
    return(synchronicity)
}

## read the mail data for the range
read.mails = function(data.path) {

    logging::logdebug("read.mails: starting.")

    mails = NULL

    ## get file name of commit data
    file = file.path(data.path, "emails.list")

    ## read data.frame from disk (as expected from save.list.to.file) [can be empty]
    mail.data <- try(read.table(file, header = FALSE, sep = ";", strip.white = TRUE,
                                fileEncoding = "latin1", encoding = "utf8"), silent = TRUE)

    ## break if the list of mails is empty
    if (inherits(mail.data, 'try-error')) {
        logging::logwarn("There are no mails available for the current environment.")
        ##logging::logwarn("Class: %s", self$get.class.name())  ??
        # logging::logwarn("Configuration: %s", private$project.conf$get.conf.as.string())
        mails = data.frame()
        return(mails)
    }

    ## set proper column names based on Codeface extraction:
    ##
    ## SELECT a.name AS authorName, a.messageId, a.email1, m.creationDate, m.subject, m.threadId
    colnames(mail.data) = c(
        "author.name", "author.email", # author information
        "message.id", "date", "date.offset", "subject", # meta information
        "thread" # thread ID
    )

    ## remove mails without a proper date as they mess up directed mail-based networks
    ## this basically only applies for project-level analysis
    empty.dates = which(mail.data[["date"]] == "" | is.na(mail.data[["date"]]))
    if (length(empty.dates) > 0)
        mail.data = mail.data[-empty.dates, ]

    ## convert dates and sort by them
    mail.data[["date"]] = as.POSIXct(mail.data[["date"]])
    mail.data = mail.data[order(mail.data[["date"]], decreasing = FALSE), ] # sort!

    ## store the mail data
    mails = mail.data
    logging::logdebug("read.mails: finished.")
    return(mails)
}

read.authors = function(data.path) {

    logging::logdebug("read.authors: starting.")

    authors = NULL

    ## get file name of commit data
    file = file.path(data.path, "authors.list")

    ## read data.frame from disk (as expected from save.list.to.file) [can be empty]
    authors.df <- try(read.table(file, header = FALSE, sep = ";", strip.white = TRUE,
                                 fileEncoding = "latin1", encoding = "utf8"), silent = TRUE)

    ## break if the list of authors is empty
    if (inherits(authors.df, 'try-error')) {
        logging::logerror("There are no authors available for the current environment.")
        ##logging::logerror("Class: %s", self$get.class.name()) ??
        ##logging::logerror("Configuration: %s", project.conf$get.conf.as.string())
        stop("Stopped due to missing authors.")
    }

    ## set proper column names based on Codeface extraction:
    ##
    ## SELECT a.name AS authorName, a.email1, m.creationDate, m.subject, m.threadId
    colnames(authors.df) = c(
        "ID", "author.name" # author information
    )

    ## store the ID--author mapping
    authors = authors.df
    logging::logdebug("read.authors: finished.")
    return(authors)
}

read.pasta = function(data.path) {
    result = list()
    filepath = paste(data.path, "similar-mailbox", sep = "/")
    lines = readLines(filepath)
    for(i in 1:length(lines)) {
        line = lines[i]
        if ( length(line) == 0 ) {
            break
        }
        if(!grepl(" => ", line)) {
            print("Faulty line!")
        }
        else if(grepl("> <", line)) {
            #finds all postions of the pattern to split the single messageIds
            val1 <- gregexpr(pattern = "> <", line)
            #finds the position of the pattern to seperate the commithash from the rest
            val2 <- val <- gregexpr(pattern = " => ", line)
            #the commithash
            value <- substr(line, as.numeric(val) + 4, nchar(line))
            listOfKeys <- c()
            #stores all positions of seperators
            for(i in 1:length(val1[[1]])) {
                listOfKeys <- c(listOfKeys, val1[[1]][i])
            }
            #splits the first messageId and stores it in result
            keyOne <- substr(line, 0, listOfKeys[[1]])
            if(is.null(result[[key]])) {
                result[[key]] <- value
            }
            else {
                result[[key]] <- c(result[[key]], value)
            }
            #splits messageId 2 till n-1 and stores it into result
            for(i in 1:length(listOfKeys)) {
                if(!i > length(listOfKeys)&length(listOfKeys) > 1) {
                    key <- substr(line, as.numeric(listOfKeys[[i]]) + 2,
                                  as.numeric(listOfKeys[[i+1]]))
                    if(is.null(result[[key]])) {
                        result[[key]] <- value
                    }
                    else {
                        result[[key]] <- c(result[[key]], value)
                    }
                    #splits the last messageId and stores it to result
                    if(i == length(listOfKeys)) {
                        key2 <- substr(line, as.numeric(listOfKeys[[i]]) + 2,
                                       as.numeric(val2) - 1)
                        if(is.null(result[[key]])) {
                            result[[key]] <- value
                        }
                        else {
                            result[[key]] <- c(result[[key]], value)
                        }
                    }
                }
            }
        }
        else {
            val <- gregexpr(pattern = " => ", line)
            key <- substr(line, 0, as.numeric(val) - 1)
            value <- substr(line, as.numeric(val) + 4, nchar(line))
            if(is.null(result[[key]])) {
                result[[key]] <- value
            }
            else {
                result[[key]] <- c(result[[key]], value)
            }
        }
    }
    return(result)
}
