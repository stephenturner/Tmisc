#' Convert string values to true \code{NA} values.
#'
#' Converts instances of user-specified strings into \code{NA}.  Can operate on either a single vector or an entire data.frame.
#'
#' @param x vector or data.frame to operate on.
#' @param strings character vector of strings to convert.
#' 
#' @return Returns a cleaned object.  Can be a vector, data.frame, or \code{tibble::tbl_df} depending on the provided input.
#' 
#' @examples
#' convert_to_NA(mtcars, "4") # a silly example;
#' # mtcars has no string NA values, but this will convert 4s to NA
#'
#' # a more typical call would be (not run):
#' # convert_to_NA(my_df, c("NA", "#N/A", "N/A", "n/a", "#NAME?"))
#' # catches common strings that should be NA
#' 
#' convert_to_NA(letters, c("b", "d"))
#' 
#' @export
convert_to_NA <- function(x, strings=c("NA", "#N/A", "N/A", "n/a", "#NAME?", "", " ", ".", "-")){
  
  if(!class(strings) %in% c("character", "numeric", "factor", "integer")){ stop("'strings' parameter should be a vector of class character, numeric, factor, or integer") }

    # helper function: converts instances of user-specified strings into NA
  clean_NA_vec <- function(vec, na_vals) {
    vec[vec %in% na_vals] <- NA
    vec
  }

  if(is.vector(x) & !is.list(x)){ # handle vector/list case, otherwise proceed for df
    result <- clean_NA_vec(x, strings)
    if(identical(x, result)){ warning("no replacements made") }
    result
  } else if(is.data.frame(x)){
    # replace character values with NA - use loop instead of apply to retain df class (data.frame or tbl_df)
    result <- x
    for(i in seq_along(result)){
      result[[i]] <- clean_NA_vec(result[[i]], na_vals = strings)
    }
    if(identical(x, result)){ warning("no replacements made") }
    result
  } else{ stop("argument 'x' must be a vector or data.frame") }
}
