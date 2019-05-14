#
# Functions to export a dataset onto an ARFF file
#

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

  arffConnection <- base::file(name, open = if (append) "a" else "w")
  on.exit(close(arffConnection))
  export.arff(x, relation, types, sparse, arffConnection, ...)
}

compute_types <- function(x) {

}

export.arff <- function(x, relation, types, sparse, con, ...) {
  writeLines(export.header(relation), con)
  writeLines(export.arff.attributes(types), con)
  export.arff.data(mld, sparse, con = con, ...)
}

export.header <- function(relation) {
  paste0("@relation ", relation)
}

export.arff.attributes <- function(mld) {
  attrNames <- ifelse(grepl("(\\s|\"|\')", names(mld$attributes)),
                      paste0("'", gsub(
                        "'", "\\'", names(mld$attributes), fixed = T
                      ), "'"),
                      names(mld$attributes))
  paste("@attribute",
        attrNames,
        mld$attributes)
}

export.arff.data <- function(mld, sparse, con, header = "@data\n", ...) {
  data <- mld$dataset[, 1:mld$measures$num.attributes]
  data[is.na(data)] <- '?'

  cat(header, file = con)
  export.arff.chunks(data, con = con, sparse = sparse, ...)
}


export.dense.arff.data <- function(data) {
  do.call("paste", c(unname(data), list(sep = ',')))
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

