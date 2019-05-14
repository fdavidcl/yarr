attributes <- function(x) {
  colnames(x)
}

types <- function(x) {
  attr(x, "attributes")
}

relation <- function(x) {
  attr(x, "relation")
}

name <- function(x) {
  attr(x, "name")
}
