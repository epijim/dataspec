#' Add together two numbers
#'
#' @param data The dataframe
#' @param output_type Output type required. For now, if not kable you get a dataframe
#' @return Summarise a dataframe in a table
#' @importFrom utils packageVersion sessionInfo capture.output packageDescription
#' @export
#' @examples
#' d_data <- data.frame(
#'   time = as.Date('2009-01-01') + 0:9,
#'   X = rnorm(10, 0, 1),
#'   Y = rnorm(10, 0, 2),
#'   Z = rnorm(10, 0, 4)
#' )
#' summaryspecs(d_data)

summaryspecs <- function(
  data,
  output_type = "kable"
  ) {
  # data checks
    # is it a dataframe?
    if (!is.data.frame(data) ) {
      ## tibble is automatically a data frame
      if (is.matrix(data)) {
        data <- as.data.frame(data)
      } else stop("clean requires a data.frame, tibble or matrix as input")
    }

  # Name of dataframe
    # Extract the dataframe name
    dfname <- deparse(substitute(data))

  ## Summarise
    data_type <- class(data)
    data_rows <- nrow(data)
    col_names <- names(data)
    col_type <- unlist(as.data.frame(sapply(data, class))[1,])
    col_missing <- sapply(data, function(x) sum(is.na(x)))
    col_min <- apply(data,2,min,na.rm=TRUE)
    col_max <- apply(data,2,max,na.rm=TRUE)

    output <- data.frame(
      Variable = col_names,
      Type = col_type,
      Missing = paste0(col_missing," ",round(100*col_missing/data_rows),"%"),
      Min = col_min,
      Max = col_max
    )

    if (output_type == "kable") {
    output <- knitr::kable(
      output,
      caption = paste0("Summary of the ",data_type," of ",dfname,". It has ",data_rows," rows"),
      row.names = FALSE
    )
    }
    return(output)
}

