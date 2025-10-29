## Utilities / IO ----

#TODO Add here the PLOTS.list functionality


#' Save an EXCEL.list into an excel file
#'
#' This function takes a numeric vector as input and computes various statistical measures, including mean, median, mode, and standard deviation.
#'
#'  TODO explain what is an EXCEL.list
#'
#' @param EXCEL.list.cur A list of elements to save in the excel file with the following structure
#' @param save_xl_path A string with the path of the Excel file
#' @param split.dist How many cells to leave between the data.frames to be saved
#' @param overwrite.cur T for overwrite the file if it exists
#' @return NULL
#' #example
#'     Provide an example of its usage
#' @export

save_to_excel = function(EXCEL.list.cur,save_xl_path,split.dist = 4,overwrite.cur = T) {

  wb = openxlsx::createWorkbook("IFM-CAP team")

  names(EXCEL.list.cur) <- substr(names(EXCEL.list.cur), 1, 25)


  for(n in names(EXCEL.list.cur)) {

    openxlsx::addWorksheet(wb,n)


    cat("Saving to Excel -->", n,"\n")


    if(data.table::is.data.table(EXCEL.list.cur[[n]])) {

      cat("\t", n," [",nrow(EXCEL.list.cur[[n]]),"x",ncol(EXCEL.list.cur[[n]]),"]\n")

      openxlsx::writeData(wb,n,EXCEL.list.cur[[n]])

    }
    else if (is.list(EXCEL.list.cur[[n]])) {

      r=0;c=1;

      for(n2 in 1:length(EXCEL.list.cur[[n]])) {

        cat("\t", names(EXCEL.list.cur[[n]])[n2]," [",nrow(EXCEL.list.cur[[n]][[n2]]),"x",ncol(EXCEL.list.cur[[n]][[n2]]),"]\n")

        r=r+1
        openxlsx::writeData(wb,n,
                  names(EXCEL.list.cur[[n]])[n2],
                  startRow = r,startCol=c)


        r=r+1

        openxlsx::writeData(wb,n,
                  EXCEL.list.cur[[n]][[n2]],
                  startRow = r,startCol=c)

        r=r+1+split.dist+nrow(EXCEL.list.cur[[n]][[n2]])
      }

    }

  }

  cat("\nSaving to ",save_xl_path)
  openxlsx::saveWorkbook(wb,save_xl_path,overwrite = overwrite.cur)
}



#' copy.to.clipboard Function
#' @param d vector or data.table or data.frame or vector
#' @param getRownames If you want to display also the rownames, set to T
#' @param ... all other parameters are passed to the write.table function
#' @return Copies to memory
#' @export
copy.to.clipboard = function (d, getRownames = F, ...)
{
  utils::write.table(d, "clipboard-65000", sep = "\t",
              row.names = getRownames, ...)
}



#' kable_print Function
#' @param big.mark.cur What character to use in order to separate thousands in digits (default ",")
#' @param digits.cur How many decimal digits to show (default 0)
#' @param caption The title to display at the top of the table
#' @return Nothing
#' @export
kable_print = function (d,big.mark.cur=",",digits.cur=0,caption="")
{

  if(nchar(caption)>0) {     cat("\n",caption,"\n",rep("-",nchar(caption)/2)); }

  knitr::kable(
    d,
    format.args = list(big.mark = big.mark.cur),digits = digits.cur
  )


}
