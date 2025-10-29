## Utilities ----


#' str_wrap_factor Function
#'
#' This function takes a factor and applies str_wrap,
#' @return the factor with the labels text wrapped
#' @export
str_wrap_factor <- function(x, width.cur=10, ...) {
  levels(x) <- stringr::str_wrap(levels(x), width=width.cur, ...)
  x
}



#' list_with_flat_elems_to_DT
#' This function convert a list to a data.table with two columns
#' @param list.cur The list to convert
#' @param dt.names A vector with the names of the returned DT
#' @return a DT
#' #example
#'   TODO Provide an example of usage of this function
#' @export
list_with_flat_elems_to_DT = function(list.cur,dt.names) {

  tmp1 = data.table::data.table(utils::stack(tmp1))

  data.table::setnames(tmp1,c("values","ind"),dt.names)

  return(tmp1)
}


#' Build a full path to a file in the database directory
#'
#' This helper function constructs an absolute file path by joining a
#' database directory with a relative file path. By default, it uses the
#' directory defined in `CONFIG$database_dir`, but a different base
#' directory can be provided via the `db` argument.
#'
#' @param relative_filepath Character string giving the path of the file
#'   relative to the database directory.
#' @param db Character string giving the base database directory.
#'   Defaults to `CONFIG$database_dir`.
#'
#' @return A character string containing the full file path.
#'
#' @export
get_db_path <- function(relative_filepath,db=CONFIG$database_dir) {
  paste0(db, "/", relative_filepath)
}




#' list_with_named_elems_to_DT
#' This function convert a list that contains named vectors to a data.table with columns the names of the vectors
#' @param list.cur The list to convert
#' @param id.col.cur How to name the names of the list
#' @return a DT
#' example
#'    TODO: Provide an example of the usage of this function
#' @export
list_with_named_elems_to_DT = function(list.cur,id.col.cur) {

 return(
   data.table::rbindlist(lapply(list.cur, as.list), idcol = id.col.cur)
 )
}
