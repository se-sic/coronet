## This file is part of coronet, which is free software: you
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
## Copyright 2019 by Klara Schlüter <schluete@fim.uni-passau.de>
## All Rights Reserved.

## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Libraries ---------------------------------------------------------------

requireNamespace("ggplot2") ## plotting


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Plot functions ----------------------------------------------------------

#' Produces a barplot showing for every editor the number of commits for which he is only author, only committer, and
#' both author and committer.
#'
#' @param data The project data.
#' @param percentage.per.author If true, the barplot shows the relative number of differently edited commits per author: each
#'                              bar in the barplot (representing the commits of one editor) is scaled to 100%. Otherwise, the
#'                              absolute number of commits per author is shown in the plot. [default: FALSE]
plot.commit.editor.types.by.author = function(data, percentage.per.author = FALSE) {

    ## get editor data
    and = get.committer.and.author.commit.count(data)
    or = get.committer.not.author.commit.count(data)

    ## build data frame as required for plotting
    both = data.frame(and[["author.name"]], and[["freq"]])
    colnames(both) = c("editor", "author and committer")

    author = aggregate(or$freq, by = list(or$author.name), FUN = sum)
    colnames(author) = c("editor", "only author")

    committer = aggregate(or$freq, by = list(or$committer.name), FUN = sum)
    colnames(committer) = c("editor", "only committer")

    plot.data = merge(merge(both, author, all = TRUE), committer, all = TRUE)
    plot.data[is.na(plot.data)] = 0

    ## if desired, calculate percentage of editor types per author
    if(plot.percentage) {
        plot.data = cbind(plot.data[1], t(apply(plot.data[2:4], 1, function(x) {x/sum(x)})))
    }

    ## compute order of bars from data: only author < author and committer < only committer
    ordered.editors = plot.data$editor[with(plot.data, order(`only committer`, `author and committer`, `only author`))]

    ## prepare data for a stacked barplot (prepare for stacking the editor types)
    plot.data = reshape2::melt(plot.data)
    names(plot.data) = c("editor", "editor type", "commit count")

    ## draw plot
    ggplot2::ggplot(data = plot.data, mapping = ggplot2::aes(x = factor(editor, levels = ordered.editors), y = `commit count`, fill = `editor type`)) +
        ## use data frame values instead of counting entries
        ggplot2::geom_bar(stat = 'identity') +
        ## rotate y-axis labels by 90 degree
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
}

#' Produces a barplot showing for how many commits committer and author are the same person and for how many commits committer
#' and author are different.
#'
#' @param data The project data.
#' @param relative.y.scale If true, the y axis shows the percentage of the number of commits of the special edit type with
#'                         respect to all commits. If false, the y axis shows the absolut number of commits.
plot.commit.edit.types.in.project = function(data, relative.y.scale = FALSE) {

    ## get commit count
    and = get.committer.and.author.commit.count(data)
    or = get.committer.not.author.commit.count(data)

    ## build data frame as required for plotting
    plot.data = data.frame(c("author /= committer", "author = committer"), c(sum(or$freq), sum(and$freq)))
    colnames(plot.data) = c("edit types", "commit count")

    ## if desired, calculate values for y axis labes showing percentage of all commits
    if(relative.y.scale) {
        plot.data = cbind(plot.data[1], plot.data[2]/sum(plot.data[2]))
    }

    ## draw plot
    ggplot2::ggplot(data = plot.data, mapping = ggplot2::aes(y = `commit count`, x = `edit types`)) +
        ## use data frame values instead of counting entries
        ggplot2::geom_bar(stat = 'identity')
}
