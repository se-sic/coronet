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
## Copyright 2022 by Jonathan Baumann <joba00002@stud.uni-saarland.de>
## All Rights Reserved.


#' Construct the 'cross product' of two patrick::cases objects.
#' Each case of the left object is combined with each case of the right,
#' test names are joined with a comma.
#' The variable names in the two cases objects are assumed to be disjoint.
#'
#' @param cases.1 a patrick::cases object (or compatible tibble).
#' @param cases.2 a patrick::cases object (or compatible tibble).
#'
#' @returns the cross product of the two patrick::cases objects, as a tibble.
cases.cross.product = function (cases.1, cases.2) {
    ## patrick::cases are tibbles with a column named '.test_name' and columns for each variable.

    ## Creating an empty tibble with matching types is not trivial.
    ## Therefore, the tibble is created with the first row of data.
    result = NULL
    is.first = T

    for (id.1 in seq_along(cases.1)){
        row.1 = cases.1[id.1,]
        for (id.2 in seq_along(cases.2)){
            row.2 = cases.2[id.2,]

            test.name.1 = row.1[[".test_name"]]
            test.name.2 = row.2[[".test_name"]]
            ## The new test name should reference both previous names.
            ## Joining with a comma is an obvious option
            test.name.combined = paste(test.name.1, test.name.2, sep = ", ")

            ## Select everthing from row.1 and everything but '.test_name' from row.2.
            row.combined = cbind(row.1, subset(row.2, select = -.test_name))
            ## Set the new combined test name
            row.combined[[".test_name"]] = test.name.combined

            ## If this is the first row, we need to create the tibble.
            ## Otherwise, we add a row.
            if (is.first) {
                result = row.combined
                is.first = F
            } else {
                result = tibble::add_case(result, row.combined)
            }
        }
    }

    return(result)
}
