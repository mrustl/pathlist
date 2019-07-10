# S4 class pathlist ------------------------------------------------------------

#' Class to Work with File Paths
#'
#' This class simplifies the work with file paths. Paths are internally stored
#' as a matrix of subfolders with one row per file and one column per path depth
#' level. You can use the Dollar operator to filter for paths belonging to a
#' certain top folder.
#'
#' @importFrom methods new
#' @export pathlist
#'
pathlist <- setClass("pathlist", slots = c(
  folders = "matrix",
  depths = "integer",
  root = "character"
))

# S4 method initialize ---------------------------------------------------------

#' @importFrom methods initialize
#' @importFrom kwb.file split_paths remove_common_root to_subdir_matrix
#' @importFrom kwb.utils getAttribute
#'
setMethod("initialize", "pathlist", function(
  .Object, paths
)
{
  stopifnot(is.character(paths))

  all_segments <- kwb.file::split_paths(paths)

  segments <- kwb.file::remove_common_root(all_segments)

  .Object@depths <- lengths(segments)
  .Object@folders <- kwb.file::to_subdir_matrix(segments)
  .Object@root <- kwb.utils::getAttribute(segments, "root")

  .Object
})

# S4 method as.character -------------------------------------------------------

#' Character Representation of a pathlist Object
#'
#' @param x a pathlist object
#' @param relative if \code{TRUE} (the default is \code{FALSE}) the root is
#'   omitted from the paths
#'
setMethod("as.character", "pathlist", function(x, relative = FALSE)
{
  paths <- character(nrow(x@folders))

  for (depth in unique(x@depths)) {

    indices <- which(x@depths == depth)

    args <- kwb.utils::asColumnList(
      (x@folders)[indices, seq_len(depth), drop = FALSE]
    )

    if (! relative) {
      args <- c(list(x@root), args)
    }

    paths[indices] <- do.call(paste, c(args, sep = "/"))
  }

  paths
})

# S4 method show ---------------------------------------------------------------

#' Print a pathlist Object
#'
#' @param object a pathlist object
#' @importFrom methods show
#' @export
#'
setMethod("show", "pathlist", function(object) {
  print(as.character(object))
})

# S4 method summary ------------------------------------------------------------

#' Print Path Summary
#'
#' This function prints a summary of a vector of file paths giving the common
#' root of all paths, the number of paths, the maximum path depth, the number
#' of paths within each depth and the names of the ten top level folders that
#' are biggest in terms of number of contained files
#'
#' @param object object of class pathlist
#' @export
#'
setMethod("summary", "pathlist", function(object)
{
  cat(sprintf("# Root: %s\n", object@root))
  cat(sprintf("# Number of paths: %d\n", nrow(object@folders)))
  cat(sprintf("# Max. depth: %d\n", ncol(object@folders)))

  cat("# Paths per depth:")
  print(table(object@depths))

  cat("# Top-level folders:\n")
  print(sort(table(object@folders[, 1]), decreasing = TRUE))
})


# S4 method [ ------------------------------------------------------------------

#' Filter Paths by Index
#'
#' @param x object of class pathlist
#' @param i indices in the vector of paths
#'
setMethod("[", "pathlist", function(x, i)
{
  x@depths <- (x@depths)[i]

  j <- seq_len(max(x@depths))

  x@folders <- (x@folders)[i, j, drop = FALSE]

  x
})

# S4 method $ ------------------------------------------------------------------

#' Filter Paths by Top Level Folder
#'
#' @param x object of class pathlist
#' @param name name of top level folder
#' @export
setMethod("$", "pathlist", function(x, name)
{
  top_levels <- (x@folders)[, 1]
  matches <- top_levels == name

  if (! any(matches)) stop(call. = FALSE, sprintf(
    "No such top-level folder: '%s'. Available folders: %s",
    name, kwb.utils::stringList(unique(top_levels))
  ))

  x <- x[matches]
  x@depths <- x@depths - 1L
  valid_depth <- x@depths > 0L
  x@folders <- (x@folders)[valid_depth, -1, drop = FALSE]
  x@depths <- (x@depths)[valid_depth]
  x@root <- paste(x@root, name, sep = "/")
  x
})

# S4 method length -------------------------------------------------------------

#' Number of Paths in pathlist
#'
#' @param x object of class pathlist
#' @export
#'
setMethod("length", "pathlist", function(x) {
  nrow(x@folders)
})

# S4 method head ---------------------------------------------------------------

#' First Paths in pathlist
#'
#' @param x object of class pathlist
#' @param n number of paths
#' @export
#'
setMethod("head", "pathlist", function(x, n = 6) {
  to <- min(n, nrow(x@folders))
  x[1:to]
})

# S4 method tail ---------------------------------------------------------------
#' Last Paths in pathlist
#'
#' @param x object of class pathlist
#' @param n number of paths
#' @export
#'
setMethod("tail", "pathlist", function(x, n = 6) {
  to <- nrow(x@folders)
  from <- max(1, to - n + 1)
  x[from:to]
})

# S3 method .DollarNames -------------------------------------------------------

#' Provide Names for Autocomplete after Dollar
#'
#' @param x object
#' @param pattern patterm matching possible completions
#' @importFrom utils .DollarNames
#' @export
.DollarNames.pathlist <- function(x, pattern)
{
  folders <- x@folders
  grep(pattern, unique(folders[, 1]), value = TRUE)
}
