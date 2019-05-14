#
# Contains necessary functions to read ARFF files in
# different formats (sparse and dense)
#

#' @title Read an ARFF file
#' @description Reads a multilabel dataset from an ARFF file in Mulan or MEKA
#' and retrieves instances distinguishing attributes corresponding to labels
#' @param filen Name of the file to read the data from
#' @param stringsAsFactors logical: should categorical attributes be converted to factors?
#' @return A `data.frame` with some attributes:
#'   - attributes: a named vector indicating the type of each variable
#'   - relation: the original "@relation" of the dataset
#'   - name: the extracted name (if the file is in MEKA format)
#' @examples
#'
#' library(yarr)
#'\dontrun{
#' yeast <- read.arff("yeast.arff")
#'}
#' @export
read.arff <- function(file, stringsAsFactors = default.stringsAsFactors()) {
  # Get file contents
  relation <- NULL
  attrs <- NULL
  contents <-
    read_arff_internal(file, stringsAsFactors = stringsAsFactors)

  attr(contents, "relation") <- read_header(attr(contents, "relation"))

  # Adjust type of numeric attributes
  attrs <- attr(contents, "attributes")
  contents[, which(attrs == "numeric")] <-
    lapply(contents[, which(attrs == "numeric")], function(column) {
      if (is.factor(column)) {
        as.numeric(levels(column)[column])
      } else {
        as.numeric(column)
      }
    })

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
  dataset <- if (detect_sparsity(rawdata))
    parse_sparse_data(rawdata, num_attrs, ...)
  else
    parse_nonsparse_data(rawdata, num_attrs, ...)

  rm(rawdata)
  names(dataset) <- names(attributes)

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
  att_mat <- att_mat[grepl("\\s*@attribute", att_mat[, 1], ignore.case = TRUE), 2:3]
  att_mat <- gsub("^'(.*?)'$", "\\1", att_mat, perl = T)
  att_mat <- gsub('^"(.*?)"$', "\\1", att_mat, perl = T)
  att_mat[, 1] <- gsub("\\'", "'", att_mat[, 1], fixed = T)
  att_mat[, 1] <- gsub('\\"', '"', att_mat[, 1], fixed = T)

  # Create the named vector
  att_v <- att_mat[, 2]
  names(att_v) <- att_mat[, 1]

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
parse_sparse_data <- function(arff_data, num_attrs, stringsAsFactors = default.stringsAsFactors()) {
  # Extract data items
  arff_data <- strsplit(gsub("[\\{\\}]", "", arff_data), ",")
  arff_data <- lapply(arff_data, function(item) {
    unlist(strsplit(gsub("^\\s+|\\s+$", "", item), " "))
  })

  # Build complete matrix with data
  dataset <- sapply(arff_data, function(row) {
    complete <- rep(0, num_attrs)
    complete[as.integer(row[c(T, F)]) + 1] <- row[c(F, T)]
    complete
  })

  # Create and return data.frame
  data.frame(t(dataset), stringsAsFactors = stringsAsFactors)
}
