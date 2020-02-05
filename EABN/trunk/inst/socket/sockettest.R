## Test of a socket transfer mechanism.

EABNPORT <- 5885

listenerloop <- function (port) {
  sock <- make.socket("localhost",port)
  on.exit(close.socket(sock))
  repeat {
    ## Input a hunk of stuff up to a blank line.
    output <- character()
    repeat {
      ss <- read.socket(sock)
      if (ss == "") break
      output <- paste(output,ss)
    }
    if (tolower(output) == "quit") break

    if (nchar(output[1]) > 0L) {
      ## Now output
      cat("Message: ",substring(output,1,100),"\n")
      write.socket(sock,paste("Message: ",substring(output,1,100)))
    }
  }
  close.socket(sock)
}

listenerloop(EABNPORT)
