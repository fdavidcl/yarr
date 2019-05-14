#
# Functions to export a dataset onto an ARFF file
#

#' @title Write a data.frame onto an ARFF file
#' @description Takes a data frame and records it in ARFF (Attribute-Relation File Format).
#' @param x A data.frame
#' @param relation Name of the dataset
#' @param types A character vector indicating the type of each variable (optional)
#' @param file Name of the file to read the data from
#' @param sparse Logical: write in sparse format?
#' @param append Logical: append to an existing file?
#' @param ... Extra parameters for internal functions
#' @return A `data.frame` with some attributes:
#'   - attributes: a named vector indicating the type of each variable
#'   - relation: the original "@relation" of the dataset
#'   - name: the extracted name (if the file is in MEKA format)
#' @examples
#'
#' library(yarr)
#'\dontrun{
#' write.arff(iris, "iris", file = "iris.arff")
#'}
#' @export
write.arff <- function(x, relation = NULL, types = NULL, file = "", sparse = FALSE, append = FALSE, ...) {
  if (is.null(relation)) {
    relattr <- attr(x, "relation")
    relation <- if (is.null(relattr))
      substitute(x)
    else
      relattr
  }

  if (is.null(types)) {
    typattr <- attr(x, "attributes")
    types <- if (is.null(typattr))
      compute_types(x)
    else
      typattr
  }

  arffConnection <- base::file(file, open = if (append) "a" else "w")
  on.exit(close(arffConnection))
  export.arff(x, relation, types, sparse, arffConnection, ...)
}

compute_types <- function(x) {
  # TODO: Convert R classes to ARFF types
  cl <- sapply(x, class)

  types <- vector("character", length(cl))
  for (i in 1:length(types)) {
    types[i] <- if (cl[i] %in% c("factor", "character")) {
      paste0("{", paste(unique(x[, i]), collapse = ","), "}")
    } else if (cl[i] == "numeric") {
      "numeric"
    }
  }

  names(types) <- colnames(x)
  types
}

export.arff <- function(x, relation, types, sparse, con, ...) {
  writeLines(export.header(relation), con)
  writeLines(export.arff.attributes(types), con)
  export.arff.data(x, sparse, con = con, ...)
}

export.header <- function(relation) {
  paste0("@relation ", relation)
}

export.arff.attributes <- function(types) {
  attr_names <- names(types)
  attr_names <- ifelse(grepl("(\\s|\"|\')", attr_names),
                      paste0("'", gsub(
                        "'", "\\'", attr_names, fixed = T
                      ), "'"),
                      attr_names)
  paste("@attribute",
        attr_names,
        types)
}

export.arff.data <- function(x, sparse, con, header = "@data\n", ...) {
  x[is.na(x)] <- '?'

  cat(header, file = con)
  export.arff.chunks(x, con = con, sparse = sparse, ...)
}


export.dense.arff.data <- function(data) {
  do.call(paste, c(unname(data), list(sep = ',')))
}

export.sparse.arff.data <- function(data) {
  # skip type check since some datasets have factors read as character
  # TODO improve read.arff to read binary factors as 0-1
  #ischar <- sapply(data, is.character)
  nonzero <- data != 0

  sapply(1:nrow(data), function(i) {
    select <- nonzero[i, ]
    paste0(
      "{",
      paste(
        which(select) - 1,
        data[i, select],
        sep = " ",
        collapse = ","
      ),
      "}"
    )
  })
}

export.arff.chunks <-
  function(data,
           con,
           chunk_size = floor(1e6 / ncol(data)),
           sparse = F,
           fun = if (sparse)
             export.sparse.arff.data
           else
             export.dense.arff.data) {
    num_instances <- dim(data)[1]
    chunks <- floor((num_instances - 1) / chunk_size)

    finished <- FALSE
    ch <- 0

    while (!finished) {
      start <- 1 + ch * chunk_size
      end <- (ch + 1) * chunk_size
      end <- if (end < num_instances) {
        end
      } else {
        finished <- TRUE
        num_instances
      }
      chunk <- data[start:end, ]

      writeLines(fun(chunk), con)
      ch <- ch + 1
    }
  }

export.csv <- function(mld, sparse, con, ...) export.arff.data(mld, sparse = sparse, header = "", con = con, ...)

export.csv.labels <- function(mld) {
  paste(rownames(mld$labels), mld$labels$index, sep = ", ")
}

