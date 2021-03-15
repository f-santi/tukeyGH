
vmessage <- function(verbose, pri = 1, datetime = TRUE, ...) {
  if (verbose %in% c('v', 'vv', 'vvv')[pri:3]) {
    if (datetime) {
      message('[', format(Sys.time(), '%H:%M:%S'), '] ', ...)
    } else {
      message(...) 
    }
    flush.console()
  }
}

