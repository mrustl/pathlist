# Define Reference Class PathList ----------------------------------------------
PathList <- setRefClass("PathList", fields = list(
  folders = "matrix",
  depths = "integer",
  root = "character"
))

# Create accessors -------------------------------------------------------------
PathList$accessors(c("folders", "depths", "root"))

# Method init ------------------------------------------------------------------
PathList$methods(init = function(paths = NULL, segments = NULL, dbg = TRUE) {

  if (is.null(paths) && is.null(segments)) stop(
    "Either 'paths' or 'segments' must be given to the pathlist().",
    call. = FALSE
  )

  if (! is.null(paths) && ! is.null(segments)) warning(
    "Both 'paths' and 'segments' are given to pathlist(). Using only ",
    "'segments'.", call. = FALSE
  )

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
  # depths <<- lengths(segments)
  # folders <<- kwb.file::to_subdir_matrix(segments, dbg = dbg)
  # root <<- kwb.utils::getAttribute(segments, "root")
  .self$setDepths(lengths(segments))
  .self$setFolders(kwb.file::to_subdir_matrix(segments, dbg = dbg))
  .self$setRoot(kwb.utils::getAttribute(segments, "root"))
})

# Method basename --------------------------------------------------------------
PathList$methods(basename = function() {

  .depths <- .self$getDepths()

  sapply(seq_along(.depths), function(i) {
    .self$getFolders()[i, depths[i]]
  })
})

# Method extension -------------------------------------------------------------
PathList$methods(extension = function() {

  kwb.utils::fileExtension(.self$basename())
})

# Method as_character ----------------------------------------------------------
PathList$methods(as_character = function(relative = FALSE) {

  paths <- character(nrow(.self$folders))

  for (depth in unique(.self$depths)) {

    indices <- which(.self$depths == depth)

    args <- kwb.utils::asColumnList(
      .self$folders[indices, seq_len(depth), drop = FALSE]
    )

    if (! relative) {
      args <- c(list(.self$root), args)
    }

    paths[indices] <- do.call(paste, c(args, sep = "/"))
  }

  paths
})

# Method as_list ---------------------------------------------------------------
PathList$methods(as_list = function(relative = FALSE) {

  root_segments <- if (! relative) {
    kwb.file::split_paths(.self$root, dbg = FALSE)[[1]]
  }

  lapply(seq_len(nrow(.self$folders)), function(i) {
    c(root_segments, .self$folders[i, seq_len(.self$depths[i])])
  })
})

# Method as_pathlist -----------------------------------------------------------
PathList$methods(as_pathlist = function() {

  pathlist::pathlist(paths = .self$as_character())
})

# Method show ------------------------------------------------------------------
PathList$methods(show = function() {

  print(.self$as_character())
})

PathList$methods(summary = function() {

  cat(sprintf("# Root: %s\n", .self$root))
  cat(sprintf("# Number of paths: %d\n", nrow(.self$folders)))
  cat(sprintf("# Max. depth: %d\n", ncol(.self$folders)))

  cat("# Paths per depth:")
  print(table(.self$depths))

  cat("# Top-level folders:\n")
  print(sort(table(.self$folders[, 1]), decreasing = TRUE))
})

# Method getAtIndices ----------------------------------------------------------
PathList$methods(getAtIndices = function(i) {

  # Make a copy of yourself, modify it and return it
  result <- .self$copy()

  result$depths <- result$depths[i]

  j <- seq_len(max(result$depths))

  result$folders <- result$folders[i, j, drop = FALSE]

  result
})

# Method digInto ---------------------------------------------------------------
PathList$methods(digInto = function(name) {

  top_levels <- (.self$folders)[, 1]
  matches <- top_levels == name

  if (! any(matches)) stop(call. = FALSE, sprintf(
    "No such top-level folder: '%s'. Available folders: %s",
    name, kwb.utils::stringList(unique(top_levels))
  ))

  result <- .self$getAtIndices(matches)
  result$depths <- result$depths - 1L
  valid_depth <- result$depths > 0L
  result$folders <- result$folders[valid_depth, -1, drop = FALSE]
  result$depths <- result$depths[valid_depth]
  result$root <- paste(result$root, name, sep = "/")
  result
})

# Method length ----------------------------------------------------------------
PathList$methods(length = function(x) {
  nrow(.self$folders)
})

# Method head ------------------------------------------------------------------
PathList$methods(head = function(x, n = 6) {
  to <- min(n, nrow(.self$folders))
  .self$getAtIndices(1:to)
})

# Method tail ------------------------------------------------------------------
PathList$methods(tail = function(x, n = 6) {
  to <- nrow(.self$folders)
  from <- max(1, to - n + 1)
  .self$getAtIndies(from:to)
})
