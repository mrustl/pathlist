# guess_and_set_declared_encodings ----------------------------------------------

#' Guess and Set the Declared Encodings
#'
#' @param x vector of character
#' @export
#' @examples
#' Encoding("\xe4")
#' Encoding(guess_and_set_declared_encodings("\xe4"))
#'
guess_and_set_declared_encodings <- function(x)
{
  stopifnot(is.character(x))

  if (length(x) == 0) {
    return(x)
  }

  is_ascii <- kwb.utils::isASCII(x)

  if (all(is_ascii)) {
    return(x)
  }

  x_non_ascii <- x[! is_ascii]

  guessed_encodings <- rep("unknown", length(x_non_ascii))

  for (encoding in c("latin1", "UTF-8")) {

    is_valid <- ! is_invalid_in_locale(`Encoding<-`(x_non_ascii, encoding))

    guessed_encodings[is_valid] <- encoding
  }

  Encoding(x[! is_ascii]) <- guessed_encodings

  x
}

# is_invalid_in_locale ---------------------------------------------------------
is_invalid_in_locale <- function(x)
{
  gives_warning <- function(expr) {
    result <- tryCatch(eval(expr, envir = -1), warning = function(w) w)
    inherits(result, "warning")
  }

  grep_warns <- gives_warning(grep("", x))
  strsplit_warns <- gives_warning(strsplit(x, ""))

  warns <- c(grep = grep_warns, strsplit = strsplit_warns)

  structure(any(warns), is_warning = warns)
}
