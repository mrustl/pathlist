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
  root = "character",
  basename = "function",
  extension = "function",
  data = "data.frame"
))

# S4 method initialize ---------------------------------------------------------

#' @importFrom methods initialize
#' @importFrom kwb.file split_paths remove_common_root to_subdir_matrix
#' @importFrom kwb.utils getAttribute
#'
setMethod("initialize", "pathlist", function(
  .Object, paths = NULL, segments = NULL, data = NULL, dbg = TRUE
)
{
  if (is.null(paths) && is.null(segments)) stop(
    "Either 'paths' or 'segments' must be given to the pathlist().",
    call. = FALSE
  )

  if (! is.null(paths) && ! is.null(segments)) warning(
    "Both 'paths' and 'segments' are given to pathlist(). Using only ",
    "'segments'.", call. = FALSE
  )

  n_paths <- length(if (is.null(paths)) segments else paths)

  if (! is.null(data) && ! is.data.frame(data)) {
    stop(call. = FALSE, sprintf(
      "A data frame must be given in 'data'. You gave something of class '%s'.",
      class(data)[1]
    ))
  }

  if (! is.null(data) && nrow(data) != n_paths) stop(call. = FALSE, sprintf(
    paste0(
      "The number of rows in 'data' (%d) must be equal to the length of ",
      "'%s' (%d)."
    ), nrow(data), ifelse(is.null(paths), "segments", "paths"), n_paths
  ))

  # If paths are given, check if they are a vector of character, check for
  # duplicates and split the paths into segments. Otherwise check if segments is
  # a list of character vectors.
  segments <- if (is.null(segments)) {
    stopifnot(is.character(paths))
    stopifnot(! any(duplicated(paths)))
    kwb.file::split_paths(paths)
  } else {
    stopifnot(is.list(segments), all(sapply(segments), is.character))
    segments
  }

  # Remove the common root of all paths
  segments <- kwb.file::remove_common_root(segments, dbg = dbg)

  # Fill the slots of the object
  .Object@depths <- lengths(segments)
  .Object@folders <- kwb.file::to_subdir_matrix(segments, dbg = dbg)
  .Object@root <- kwb.utils::getAttribute(segments, "root")
  .Object@data <- if (is.null(data)) data.frame() else data

  # Define function basename()
  .Object@basename <- function() {
    sapply(seq_along(.Object@depths), function(i) {
      .Object@folders[i, .Object@depths[i]]
    })
  }

  # Define function extension()
  .Object@extension <- function() {
    kwb.utils::fileExtension(.Object@basename())
  }

  # Return the object
  .Object
})

# S4 method as.character -------------------------------------------------------

#' Character Representation of a pathlist Object
#'
#' @param x a pathlist object
#' @param relative if \code{TRUE} (the default is \code{FALSE}) the root is
#'   omitted from the paths
#' @export
#'
setMethod("as.character", "pathlist", function(x, relative = FALSE)
{
  paste_segments(
    folders = x@folders,
    depths = x@depths,
    depth_to_colnum = seq_len,
    root = if (! relative) x@root
  )
})

# paste_segments ---------------------------------------------------------------
paste_segments <- function(folders, depths, depth_to_colnum, root = NULL)
{
  paths <- character(nrow(folders))

  for (depth in unique(depths)) {

    indices <- which(depths == depth)

    args <- kwb.utils::asColumnList(
      folders[indices, depth_to_colnum(depth), drop = FALSE]
    )

    if (! is.null(root)) {
      args <- c(list(root), args)
    }

    paths[indices] <- do.call(paste, c(args, sep = "/"))
  }

  paths
}

# S4 method as.list ------------------------------------------------------------

#' List Representation of a pathlist Object
#'
#' @param x a pathlist object
#' @param relative if \code{TRUE} (the default is \code{FALSE}) the root is
#'   omitted from the paths
#' @export
#'
setMethod("as.list", "pathlist", function(x, relative = FALSE)
{
  root_segments <- if (! relative) {
    kwb.file::split_paths(x@root, dbg = FALSE)[[1]]
  }

  lapply(seq_len(nrow(x@folders)), function(i) {
    c(root_segments, x@folders[i, seq_len(x@depths[i])])
  })
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

  if (! is.null(object@data)) {
    cat("\n# Additional data with columns:\n")
    cat(paste(names(object@data), collapse = "|"))
  }
})

# Define Generics --------------------------------------------------------------

#' Get the File Name Only
#'
#' @param object object of class pathlist
#'
setGeneric("folder", function(object) standardGeneric("folder"))

#' Get the Top-Level Folder
#'
#' @param object object of class pathlist
setGeneric("toplevel", function(object) standardGeneric("toplevel"))

#' Get the File Name Only
#'
#' @param object object of class pathlist
setGeneric("filename", function(object) standardGeneric("filename"))

# S4 method folder -------------------------------------------------------------

#' Get the Folder Path of all Folders Below the Top-Level
#'
#' @param object object of class pathlist
#' @export
setMethod("folder", "pathlist", function(object) {
  result <- character(length(object@depths))
  has_folder <- object@depths > 2
  object_folders <- object[has_folder]
  object_folders@folders <- object_folders@folders[, -1, drop = FALSE]
  object_folders@depths <- object_folders@depths - 2L
  result[has_folder] <- as.character(object_folders, relative = TRUE)
  result
})

# S4 method toplevel -----------------------------------------------------------

#' Get the Top-Level Folder
#'
#' @param object object of class pathlist
#' @export
setMethod("toplevel", "pathlist", function(object) {
  result <- character(length(object@depths))
  has_top_level <- object@depths > 1
  result[has_top_level] <- object@folders[has_top_level, 1]
  result
})

# S4 method filename -----------------------------------------------------------

#' Get the File Name Only
#'
#' @param object object of class pathlist
#' @export
setMethod("filename", "pathlist", function(object) {
  object@folders[cbind(seq_along(object@depths), object@depths)]
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
  x@data <- x@data[i, , drop = FALSE]

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
  x@data <- x@data[valid_depth, , drop = FALSE]
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
setMethod("tail", "pathlist", function(x, n = 6)
{
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
#'
.DollarNames.pathlist <- function(x, pattern)
{
  folders <- x@folders
  grep(pattern, unique(folders[, 1]), value = TRUE)
}
