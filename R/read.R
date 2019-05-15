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

#
# Functions to read ARFF files in different formats (sparse and dense)
#

#'@title Read an ARFF file
#'@description Reads a dataset from an ARFF file, parsing each section and
#'  converting the data section into a `data.frame`.
#'@param file Name of the file to read the data from
#'@param stringsAsFactors Logical: should categorical attributes be converted to
#'  factors?
#'@return A `data.frame` with some attributes:
#'
#'  - attributes: a named vector indicating the type of each variable
#'  - relation: the original `@relation` of the dataset
#'
#'@examples
#'
#' library(yarr)
#'\donttest{
#' yeast <- read.arff("yeast.arff")
#'}
#'@export
read.arff <- function(file, stringsAsFactors = default.stringsAsFactors()) {
  # Get file contents
  relation <- NULL
  attrs <- NULL
  contents <-
    read_arff_internal(file, stringsAsFactors = stringsAsFactors)

  attr(contents, "relation") <- read_header(attr(contents, "relation"))

  # Adjust type of numeric attributes
  attrs <- which(attr(contents, "attributes") == "numeric")
  for (i in attrs) {
    contents[[i]] <- if (is.factor(contents[[i]])) {
      as.numeric(levels(contents[[i]])[contents[[i]]])
    } else {
      as.numeric(contents[[i]])
    }
  }

  structure(contents,
            class = c("arff_data", class(contents)))
}

# Extracts all useful data from an ARFF file in an
# R object
#
# @param arff_file Path to the file
# @return data.frame with "variables" and "relation" attributes
read_arff_internal <- function(arff_file, ...) {
  file_con <- file(arff_file, "rb")

  if (!isOpen(file_con)) {
    open(file_con, "rb")
    on.exit(close(file_con))
  }

  # Read whole file
  file_data <- strsplit(readChar(file_con, nchars = file.info(arff_file)$size, useBytes = TRUE),
                        "\\\r\\\n|\\\r|\\\n", fixed = FALSE, useBytes = TRUE)[[1]]

  close(file_con)

  # Split into relation, attributes and data
  relation_at <- grep("@relation", file_data, ignore.case = TRUE)
  data_start <- grep("@data", file_data, ignore.case = TRUE)

  if (is.na(relation_at)) stop("Missing @relation or not unique.")
  if (is.na(data_start)) stop("Missing @data mark or not unique.")

  relation <- file_data[relation_at]

  # Get attribute vector
  attributes <- parse_attributes(file_data[(relation_at + 1):(data_start - 1)])
  num_attrs <- length(attributes)

  # Ignore blank lines before data
  data_start <- data_start + 1
  while (grepl("^\\s*$", file_data[data_start]))
    data_start <- data_start + 1

  # Build data.frame with @data section
  rawdata <- file_data[data_start:length(file_data)]
  dataset <- if (detect_sparsity(rawdata)) {
    parse_sparse_data(rawdata, defaults = sparse_defaults(attributes), ...)
  } else {
    parse_nonsparse_data(rawdata, num_attrs, ...)
  }

  rm(rawdata)
  colnames(dataset) <- names(attributes)

  return(structure(dataset,
                   relation = relation,
                   attributes = attributes))
}

# Reads the attributes section of an ARFF file
#
# @param arff_attrs Lines containing the attributes
# @return A vector containing, for each
#  attribute, its name and its type
parse_attributes <- function(arff_attrs) {
  # Extract attribute definitions

  #-----------------------------------------------------------------------------------------------------
  # Finding adequate spaces to split the attribute definition into 3 parts:
  #    @attribute attr_name {0, 1}   -> c("@attribute", "attr_name", "{0, 1}")
  #    @attribute 'Attr. name' {0,1} -> c("@attribute", "'Attr. name'", "{0,1}")
  #    @attribute 'David\'s attribute' {0,1} -> c("@attribute", "'David\'s attribute'", "{0,1}")
  #-----------------------------------------------------------------------------------------------------
  # Using the technique described under "Perl/PCRE Variation" in this StackOverflow answer:
  #    (Regex Pattern to Match, Excluding when...) http://stackoverflow.com/a/23589204/5306389
  # We capture any spacing character ignoring those within braces or single quotes,
  # allowing the appearance of escaped single quotes (\').
  #-----------------------------------------------------------------------------------------------------
  # Regex tested in https://regex101.com/r/tE5mP1/20
  #-----------------------------------------------------------------------------------------------------

  rgx <- "(?:{(?:.*?)}\\s*$|(?<!\\\\)'[^'\\\\]*(?:.*?)(?<!\\\\)'|(?<!\\\\)\"(?:.*?)(?<!\\\\)\"|(\\s*?)@)(*SKIP)(*F)|\\s+"
  att_list <- strsplit(arff_attrs, rgx, perl = TRUE)

  # Structure by rows
  att_mat <- matrix(unlist(att_list[sapply(att_list, function(row){length(row) == 3})]),
                    ncol = 3, byrow = T)
  rm(att_list)
  # Filter any data that is not an attribute
  att_mat <- att_mat[grepl("\\s*@attribute", att_mat[, 1], ignore.case = TRUE), 2:3, drop = FALSE]
  att_mat <- gsub("^'(.*?)'$", "\\1", att_mat, perl = T)
  att_mat <- gsub('^"(.*?)"$', "\\1", att_mat, perl = T)
  att_mat[, 1] <- gsub("\\'", "'", att_mat[, 1], fixed = T)
  att_mat[, 1] <- gsub('\\"', '"', att_mat[, 1], fixed = T)

  # Create the named vector
  att_v <- att_mat[, 2, drop = FALSE]
  names(att_v) <- att_mat[, 1, drop = FALSE]

  rm(att_mat)
  return(att_v)
}

# Reads the name and Meka parameters in the header of an
# ARFF file
#
# @param arff_relation "relation" line of the ARFF file
# @return Number of labels in the dataset
read_header <- function(arff_relation) {
  rgx <- regexpr("[\\w\\-\\._]+\\s*:\\s*-[Cc]\\s*-?\\d+", arff_relation, perl = TRUE)
  hdr <- strsplit(regmatches(arff_relation, rgx), "\\s*:\\s*-[Cc]\\s*")

  if (length(hdr) > 0) {
    # Meka header
    return(structure(
      hdr[[1]][1],
      c = as.numeric(hdr[[1]][2])
    ))
  } else {
    # Mulan header
    nm <- regmatches(arff_relation, regexpr("(?<=\\s)'?[\\w\\-\\._]+'?", arff_relation, perl = TRUE))
    return(nm)
  }
}

# Detects whether an ARFF file is in sparse format
#
# @param arff_data Content of the data section
# @return Boolean, TRUE when the file is sparse
detect_sparsity <- function(arff_data) {
  grepl("^\\s*\\{", arff_data[1])
}

# Builds a data.frame out of non-sparse ARFF data
#
# @param arff_data Content of the data section
# @return data.frame containing data values
parse_nonsparse_data <- function(arff_data, num_attrs, stringsAsFactors = default.stringsAsFactors()) {
  data.frame(matrix(
    unlist(strsplit(arff_data, ",", fixed = T)),
    ncol = num_attrs,
    byrow = T
  ), stringsAsFactors = stringsAsFactors)
}

# Builds a data.frame out of sparse ARFF data
#
# @param arff_data Content of the data section
# @return data.frame containing data values
parse_sparse_data <- function(arff_data, defaults, stringsAsFactors = default.stringsAsFactors()) {
  # Extract data items
  empty <- grep("^\\s*$", arff_data)
  arff_data <- if (length(empty) > 0) arff_data[-empty] else arff_data
  arff_data <- strsplit(gsub("\\s*[\\{\\}]\\s*", "", arff_data), "\\s*,\\s*")

  dataset <- lapply(arff_data, function(item) {
    row <- unlist(strsplit(item, "\\s+"))

    # Build complete row with data
    complete <- defaults
    complete[as.integer(row[c(T, F)]) + 1] <- row[c(F, T)]
    complete
  })

  # Create and return data.frame
  t(data.frame(dataset, stringsAsFactors = stringsAsFactors))
}

# The default value for a sparse variable is:
#  - 0, if the attribute is numeric
#  - The first value of the factor, if the attribute is categorical
#  - ""? if the type is string (the dataset was probably badly exported)
sparse_defaults <- function(attributes) {
  defaults <- vector(mode = "character", length = length(attributes))

  # Detect factors, extract values
  factors <- which(grepl("^\\s*\\{", attributes))
  values <- strsplit(gsub("\\s*[\\{\\}]\\s*", "", attributes[factors]), "\\s*,\\s*")
  defaults[factors] <- sapply(values, function(v) v[1])

  strings <- which(attributes == "string")
  defaults[strings] <- ""

  rest <- setdiff(1:length(attributes), union(factors, strings))
  defaults[rest] <- "0" # will be converted to numeric later

  defaults
}
