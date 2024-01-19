## Set a flag if mongo is not available (so we can run the skip test just once)
if (identical(Sys.getenv("NOT_CRAN"),"true")) {
  con <-  try (mongolite::mongo())
  if (is(con, "try-error"))
    MongoAvailable <- FALSE
  else
    MongoAvailable <- TRUE
} else {
  ## On CRAN, skip
  warning("Mongo Tests skipped on CRAN")
  MongoAvailable <- FALSE
}
