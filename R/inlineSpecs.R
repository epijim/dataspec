#' Make a table explaining data
#'
#' @param data The dataframe
#' @param output_type Output type required. For now, if not kable you get a dataframe
#' @return Summarise a dataframe in a table
#' @importFrom utils packageVersion sessionInfo capture.output packageDescription
#' @export
#' @examples
#' d_data <- data.frame(
#'   Time = as.Date('2009-01-01') + 0:9,
#'   Colours = rep(c("red", "blue"),5),
#'   Numbers = rnorm(10, 0, 10)
#' )
#' d_data$Potatoes <- rep(c("sweet",NA,"russet","french-fried","roasted"),2)
#' inlineSpecs(d_data)

inlineSpecs <- function(
  data,
  output_type = "kable"
  ) {
  # data checks
    # is it a dataframe?
    if (!is.data.frame(data) ) {
      ## tibble is automatically a data frame
      if (is.matrix(data)) {
        data <- as.data.frame(data)
      } else stop("inlineSpecs requires a data.frame, tibble or matrix as input")
    }

  # Name of dataframe
    # Extract the dataframe name
    dfname <- deparse(substitute(data))

  ## Summarise
    data_type <- class(data)
    data_rows <- nrow(data)
    col_names <- names(data)
    col_type <- sapply(data, class)
    col_missing <- sapply(data, function(x) sum(is.na(x)))
    col_minmax <- paste(apply(data,2,min,na.rm=TRUE),"to",apply(data,2,max,na.rm=TRUE))
    col_unique <- paste(apply(data,2,dplyr::n_distinct,na.rm=TRUE),"levels")

    output <- data.frame(
      Variable = col_names,
      Type = col_type,
      Missing = paste0(col_missing," ",round(100*col_missing/data_rows),"%"),
      MinMax = col_minmax,
      Levels = col_unique,
      stringsAsFactors = FALSE
    )
    output  <- dplyr::mutate(
      output,
      Details = ifelse(
        col_type %in% c("factor", "character"),
        Levels, MinMax
        )
      )

    output  <- dplyr::select(
      output,
      Variable,Type,Details
    )

    if (output_type == "kable") {
    output <- knitr::kable(
      output,
      caption = paste0("Summary of the ",data_type," ",dfname,". It has ",data_rows," rows"),
      row.names = FALSE
    )
    }
    return(output)
}

