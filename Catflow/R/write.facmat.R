#' Write Factor Matrix to File
#'
#' This function writes a factor matrix to a specified output file for a model application.
#'
#' @param output.file Character. Path to the output file. If missing, the matrix is not written to a file.
#' @param eta Numeric vector. Row identifiers for the matrix.
#' @param xsi Numeric vector. Column identifiers for the matrix.
#' @param header Character. Header string for the file. If missing, a default header is generated.
#' @param numh Numeric. A number used in the default header generation. Default is 1.
#' @param fac Numeric. The value to fill the matrix with. Default is 1.
#' @return A matrix of dimensions `length(eta)` x `length(xsi)` filled with `fac`. If `output.file` is provided, the matrix is also written to the file.
#' @examples
#' write.facmat("output.txt", eta = 1:5, xsi = 1:3)
write.facmat <- function(output.file, eta, xsi, header = NULL, numh = 1, fac = 1) {
  # Validate inputs
  if (!is.numeric(eta) || length(eta) == 0) stop("'eta' must be a non-empty numeric vector.")
  if (!is.numeric(xsi) || length(xsi) == 0) stop("'xsi' must be a non-empty numeric vector.")
  if (!is.numeric(fac)) stop("'fac' must be numeric.")
  if (!is.numeric(numh) || length(numh) != 1) stop("'numh' must be a single numeric value.")
  if (!is.null(output.file) && !is.character(output.file)) stop("'output.file' must be a character string.")

  # Generate default header if missing
  if (is.null(header)) {
    header <- paste(-1000 - numh, length(eta), length(xsi))
  }

  # Create the matrix
  factor_matrix <- matrix(fac, nrow = length(eta), ncol = length(xsi))

  # Write to file if output.file is provided
  if (!missing(output.file)) {
    con <- file(output.file, open = "w")
    on.exit(close(con), add = TRUE)  # Ensure the file is closed on exit
    write(header, con)
    write.table(format(factor_matrix), con, row.names = FALSE, col.names = FALSE, quote = FALSE)
    message(sprintf("Generated file: %s", basename(output.file)))
  }

  # Return the matrix invisibly
  return(invisible(factor_matrix))
}