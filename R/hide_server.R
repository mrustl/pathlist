# hide_server_ -----------------------------------------------------------------
hide_server_ <- function(root, remove_dollar = TRUE)
{
  if (! nzchar(root)) {
    return("")
  }

  replacements <- c(
    list(
      # Replace real server name with "server"
      "^//[^/]+" = "//server"
    ), if (remove_dollar) list(
      # Remove dollar character
      "\\$" = ""
    )
  )

  kwb.utils::multiSubstitute(root, replacements)
}
