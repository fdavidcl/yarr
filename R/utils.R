# yarr, Yet Another ARFF Reader
# Copyright (C) 2019 David Charte & Francisco Charte
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

#' @title Dataset utilities
#'
#' @description Some tools to access the metadata in ARFF files. `attr.names`
#'   retrieves the names of the attributes, `attr.types` returns the ARFF type
#'   of each variable, `relation` shows the name/relation specified in the
#'   `@relation` section of the file.
#'
#' @param x A dataset read using [`read.arff`](read.arff.html)
#' @rdname utils
#' @export
attr.names <- function(x) {
  colnames(x)
}

#' @rdname utils
#' @export
attr.types <- function(x) {
  attr(x, "attributes")
}

#' @rdname utils
#' @export
relation <- function(x) {
  attr(x, "relation")
}

#' @title Display functions
#' @param x A data.frame read from an ARFF file
#' @param ... Extra parameters for the corresponding S3 method for class
#'   `data.frame`
#' @rdname display
#' @export
print.arff_data <- function(x, ...) {
  cat("An ARFF dataset:", relation(x), "\n")
  cat("Variables")
  print.data.frame(x, ...)
}

#' @param object A data.frame read from an ARFF file
#' @rdname display
#' @export
summary.arff_data <- function(object, max_attrs = 10, max_values = 5, ...) {
  typ <- attr.types(object)

  cat("An ARFF dataset:", relation(object), "\n")
  cat(length(typ), "attributes and", nrow(object), "instances\n")

  sep <- "  ...\n"
  if (length(typ) <= max_attrs) {
    sep <- ""
    max_attrs <- length(typ)
  }

  etc <- "..."
  if (nrow(object) <= max_values) {
    max_values <- nrow(object)
    etc <- ""
  }

  vals <- function(variable) {
    if (max_values < 1) "" else paste0(paste0(variable[1:max_values], collapse = ", "), etc)
  }

  range <- if (max_attrs > 1) 1:(max_attrs - 1) else 1
  for (i in range) {
    cat("  ", names(typ)[i], ": ", typ[i], " ", vals(object[[i]]), "\n", sep = "")
  }
  if (max_attrs > 2) {
    cat(sep, "  ", names(typ)[length(typ)], ": ", typ[length(typ)], " ", vals(object[[length(typ)]]), "\n", sep = "")
  }
}
