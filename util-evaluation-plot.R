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
plot.commit.editor.types.by.author = function(data) {

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
    ## prepare data for a stacked barplot (prepare for stacking the editor types)
    plot.data = reshape2::melt(plot.data)
    names(plot.data) = c("editor", "editor type", "commit count")


    ## draw plot
    ggplot2::ggplot(data = plot.data, mapping = ggplot2::aes(x = editor, y = `commit count`, fill = `editor type`)) +
        ## use data frame values instead of counting entries
        ggplot2::geom_bar(stat = 'identity') +
        ## rotate y-axis labels by 90 degree
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
}

#' Produces a barplot showing for how many commits committer and author are the same person and for how many commits committer
#' and author are different.
#'
#' @param data The project data.
plot.commit.edit.types.in.project = function(data) {

    ## get commit count
    and = get.committer.and.author.commit.count(data)
    or = get.committer.not.author.commit.count(data)

    ## build data frame as required for plotting
    plot.data = data.frame(c("author /= committer", "author = committer"), c(sum(or$freq), sum(and$freq)))
    colnames(plot.data) = c("edit types", "commit count")

    ## draw plot
    ggplot2::ggplot(data = plot.data, mapping = ggplot2::aes(y = `commit count`, x = `edit types`)) +
        ## use data frame values instead of counting entries
        ggplot2::geom_bar(stat = 'identity')
}
