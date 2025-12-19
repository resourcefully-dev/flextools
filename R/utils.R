.message_cache <- new.env(parent = emptyenv())

message_once <- function(msg) {
  if (!exists(msg, envir = .message_cache)) {
    message(msg)
    assign(msg, TRUE, envir = .message_cache)
  }
}

reset_message_once <- function() {
  rm(
    list = ls(envir = .message_cache, all.names = TRUE),
    envir = .message_cache
  )
  invisible(NULL)
}
