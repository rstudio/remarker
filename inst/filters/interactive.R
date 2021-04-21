#!/usr/bin/env Rscript
#
# To use in an Rmd document, put this in the YAML header:
# output:
#   html_document:
#     pandoc_args: !expr 'list(paste0("--filter=", system.file("filters/interactive.R", package="remarker")))'
#
# When rmarkdown::run() is called on the document, it will invoke Pandoc, and
# Pandoc will run this script, piping the JSON AST to it. This script writes
# the JSON to a file called ast_in.json, and it will wait for ast_out.json
# to be created. It will read in ast_out.json and send the contents back to
# Pandoc.
#
# The utility functions remarker::json_ast and remarker::ast_json should be used
# for parsing and writing out of the new AST. The make sure that the JSON is
# valid for pandoc's ast format by avoiding class dispatch for native R objects
# So scripts that use this filter should take the form:
#
#   # Make sure you're in correct working directory
#   x <- json_ast(readLines(file("ast_in.json", open = "rb")))
#   # Do some manipulation to AST
#   ...
#   # Write out ast_out so filter knows to proceed
#   writeLines(ast_json(x), con = "ast_out.json")
#

intxt <- readLines(file("stdin", open = "rb"))
writeLines(intxt, "ast_in.json")

message(tempdir())
message("Pandoc AST written to ", file.path(getwd(), "ast_in.json"))
message("Waiting for ast_out.json")

if (file.exists("ast_out.json")) {
  invisible(file.remove("ast_out.json"))
}

while(!file.exists("ast_out.json")) {
  Sys.sleep(0.5)
}

outtxt <- readLines("ast_out.json")

cat(outtxt, sep = "\n", file = stdout())
