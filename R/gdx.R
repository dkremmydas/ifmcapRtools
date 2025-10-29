## GDX related ----


#' Read a symbol (set or parameter) from a gdx
#'
#' This function reads a symbol from a gdx file.
#'
#' Improvements in comparison with the gdxrrw native package:
#' - There is no need to define if it is a parameter or a set
#' - It is case insensitive (p_PMP_Linear equal to p_pmp_Linear)
#' - It always handle the returned column names well
#'
#' @param symbol A string with the symbol to load
#' @param gdx_path A string with the path of the GDX file
#' @param custom_colnames A vector for setting explicitly the names of the columns of the GDX.
#'   Provide names only for the dimensions. The column that contains the value will take the
#'   name `"value"`, unless defined otherwise via the `value_colname` argument.
#'
#'   **Example:**
#'   ```r
#'   custom_colnames = c("FDALL","SCEN","variable","dim1","dim2","dim3","dim4")
#'   ```
#'
#' @param filters A named list specifying which elements to retain.
#'   Each name in the list corresponds to a column name, and its value
#'   is a character vector of the elements to keep. Filtering is case-insensitive.
#'
#'   **Example:**
#'   ```r
#'   filters = list(
#'     variable = c("v_Objective"),
#'     SCEN  = c("BSL", "y2025")
#'   )
#'   ```
#' @param value_colname For parameters, a custom column name for the value. Default: "value"
#' @param get_te  Retrieve the explanatory text of the set elements. It works only for set elements;
#'        if you set it to TRUE for parameters, it will be ignored.
#' @param get_ts  Retrieve the explanatory text of the symbol. It places it in the ts attribute
#' @return A [`data.table::data.table`] object containing the symbol.
#' @export

read_from_gdx = function(symbol,gdx_path,filters=list(),custom_colnames=c(),value_colname="value",get_te=FALSE,get_ts=FALSE) {

  #check of the symbol is a set or a parameter
  tmp1=gdxrrw::gdxInfo(gdx_path,dump = F,returnDF=T)

  ##get the information from the set or parameter row
  tmp2 = subset(tmp1$sets, tolower(name) == tolower(symbol))
  if(nrow(tmp2)==0) {tmp2 = subset(tmp1$parameters, tolower(name) == tolower(symbol)); isSet=F  } else {isSet=T}

  ##if tmp2 is still empty, the symbol does not exist
  if(nrow(tmp2)==0) {stop("Symbol ",symbol," does not exist in ", gdx_path)}

  #get the number of dimensions
  if(isSet) {
    num_of_dims = data.table::data.table(tmp1$sets)[tolower(name)==tolower(symbol),dim]
  } else {
    get_te=F
    num_of_dims = data.table::data.table(tmp1$parameters)[tolower(name)==tolower(symbol),dim]
  }

  default_colnames = unlist(tmp2[1,"domnames"])

  if(get_te) {
    num_of_dims=num_of_dims+1
    if(length(custom_colnames)>0)  {custom_colnames=c(custom_colnames,"ts")}
    default_colnames = c(default_colnames,"ts")
  }


  #If the loaded data is a SET
  if(isSet) {
    tmp3 = data.table::data.table(
      droplevels(
        rgdx.set(
          gdx_path,
          symbol,
          te=get_te,
          compress = T, useDomInfo = F
        )
      )
    )


    if(length(custom_colnames)>0) {

      if(length(custom_colnames)==num_of_dims) {
        #using the custom column names
        data.table::setnames(tmp3,old=1:num_of_dims,new=custom_colnames)

      } else {
        warning("The custom_colnames contain ", length(custom_colnames), " elements, but the loaded gdx contains ",length(names(tmp3)), " elements. We use the gdx column names." )
        data.table::setnames(tmp3,new=default_colnames)
      }

    } else {
      #using the gdx column names
      data.table::setnames(tmp3,new=default_colnames)

    }



    #If the loaded data is a PARAMETER
  } else {

    tmp3 = data.table::data.table(
      droplevels(
        rgdx.param(
          gdx_path,
          symbol,
          compress = T, useDomInfo = F
        )
      )
    )


    if(length(custom_colnames)>0) {

      if(length(custom_colnames)==num_of_dims) {
        #using the custom column names
        data.table::setnames(tmp3,old=1:num_of_dims,new=custom_colnames)

      } else {
        warning("The custom_colnames contain ", length(custom_colnames), " elements, but the loaded gdx contains ",length(names(tmp3)), " elements. We use the gdx column names." )
        data.table::setnames(tmp3,new=c(default_colnames,"value"))
      }

    } else {
      #using the gdx column names
      data.table::setnames(tmp3,new=c(default_colnames,"value"))

    }

    data.table::setnames(tmp3,old=(num_of_dims+1),new="value")

  }



  #filter
  if(length(filters)>0) {

    for (col_name in names(filters)) {
      tmp3 <- tmp3[tolower(get(col_name)) %in% tolower(filters[[col_name]])]
    }

  }

  if(!isSet) { data.table::setnames(tmp3,old="value",new=value_colname) }

  attr(tmp3,"isSet") = isSet

  if(get_ts) { attr(tmp3,"ts") = tmp2$text  }


  return(tmp3)


}




#' Read a symbol (set or parameter) from a gdx
#'
#' This function reads a symbol from a gdx file.
#'
#' Improvements in comparison with the gdxrrw native package:
#' - There is no need to define if it is a parameter or a set
#' - It is case insensitive (p_PMP_Linear equal to p_pmp_Linear)
#' - It always handle the returned column names well
#'
#' @param symbol A string with the symbol to load
#' @param gdx_path A string with the path of the GDX file
#' @param custom_colnames A vector for setting explicitly the names of the columns of the GDX.
#'   Provide names only for the dimensions. The column that contains the value will take the
#'   name `"value"`, unless defined otherwise via the `value_colname` argument.
#'
#'   **Example:**
#'   ```r
#'   custom_colnames = c("FDALL","SCEN","variable","dim1","dim2","dim3","dim4")
#'   ```
#'
#' @param filters A named list specifying which elements to retain.
#'   Each name in the list corresponds to a column name, and its value
#'   is a character vector of the elements to keep. Filtering is case-insensitive.
#'
#'   **Examples:**
#'   ```r
#'   filters = list(
#'     variable = c("v_Objective"),
#'     SCEN  = c("BSL", "y2025")
#'   )
#'
#'   filters=list(variable=c("Solver","v_Objective"))
#'
#'   filters=list(FDALL=c("80005990095207"),variable=c("Solver","v_Objective"))
#'   ```
#' @param value_colname For parameters, a custom column name for the value. Default: "value"
#' @param get_te  Retrieve the explanatory text of the set elements. It works only for set elements;
#'        if you set it to TRUE for parameters, it will be ignored.
#' @param get_ts  Retrieve the explanatory text of the symbol. It places it in the ts attribute
#' @return A [`data.table::data.table`] object containing the symbol.
#' @export

read_from_gdx2 = function(symbol,gdx_path,filters=list(),custom_colnames=c(),value_colname="value",get_te=FALSE,get_ts=FALSE) {

  # Check that the file exists
  if (!file.exists(gdx_path)) {
    stop(sprintf("GDX file does not exist: %s", gdx_path))
  }

  # Load the contents of the file
  tmp1=gdxrrw::gdxInfo(gdx_path,dump = F,returnDF=T)

  # Check if the symbol exists in sets or parameters
  symbol_exists <- any(tolower(tmp1$sets$name) == tolower(symbol)) ||
    any(tolower(tmp1$parameters$name) == tolower(symbol))
  if (!symbol_exists) {
    stop(sprintf("Symbol '%s' does not exist in GDX file: %s", symbol, gdx_path))
  }

  #check of the symbol is a set or a parameter and set the isSet value
  ##get the information from the set or parameter row
  tmp2 = subset(tmp1$sets, tolower(name) == tolower(symbol))
  if(nrow(tmp2)==0) {tmp2 = subset(tmp1$parameters, tolower(name) == tolower(symbol)); isSet=F  } else {isSet=T}

  # Check that the custom_colnames are defined well
  # Get the number of dimensions for the symbol (from sets or parameters)
  domnames <- tmp2$domnames[[1]]
  num_of_dims <- length(domnames)

  if (!(length(custom_colnames) %in% c(0, num_of_dims))) {
    stop(sprintf(
      "custom_colnames must be either empty or have length equal to the number of dimensions (%d) for symbol '%s'. Provided: %d",
      num_of_dims, symbol, length(custom_colnames)
    ))
  }


  #Create a structure that hold all the information about the columns (number of dimensipns, original name, custom and filters )
  column_names = data.table(
    col_num = 1:length(domnames),
    gdx_column_name = domnames,
    custom_colnames = if (length(custom_colnames) == 0) rep(NA_character_, length(domnames)) else custom_colnames
  )

  column_names[,used_name:=ifelse(is.na(custom_colnames),gdx_column_name,custom_colnames)]
  column_names[used_name=="*",used_name:=paste0("V",col_num)]

  # Prepare filters as a named list for easy lookup
  filter_lookup <- stats::setNames(filters, names(filters))

  # Add filter_value column: NA if no filter, otherwise the filter value
  column_names[, filter_value := lapply(used_name, function(nm) {
    if (nm %in% names(filter_lookup)) filter_lookup[[nm]] else NA
  })]



  #Get the uels
  all_uels = gdxrrw::rgdx(gdx_path,requestList = list(name=symbol,compress=T))$uels

  uels.filters = data.table::copy(all_uels)
  for (i in seq_len(nrow(column_names))) {
    filter_vals <- column_names$filter_value[[i]]

    if (!is.na(filter_vals[1])) {
      # Find case-insensitive matches
      matches <- grepl(paste(filter_vals, collapse = "|"), all_uels[[i]], ignore.case = TRUE)

      if (any(matches)) {
        # Keep only matches
        uels.filters[[i]] <- all_uels[[i]][matches]
      } else {
        # Warn and keep original
        warning(
          sprintf(
            "No matches found for value in column %s: '%s'. Not applying the filter.",
            column_names[col_num==i,used_name],
            paste(filter_vals, collapse = ", ")
          ),
          call. = FALSE
        )
        uels.filters[[i]] <- all_uels[[i]]
      }
    }
  }

  print(column_names)
  utils::str(uels.filters)

  #load the data
  b=rgdx(gdxName=gdx_path,
         requestList = list(
           name = symbol,
           form="sparse",
           uels=uels.filters
         ),
         useDomInfo = T)


  #convert to dt approach gpt
  gdx_data <- as.data.table(b$val)
  data.table::setnames(gdx_data, c(column_names$used_name, value_colname), skip_absent = TRUE)
  for (i in seq_len(length(b$uels))) {
    cur_col_name = column_names[col_num==i,used_name]
    gdx_data[[cur_col_name]] <- factor(
      b$uels[[i]][ gdx_data[[cur_col_name]] ],
      levels = b$uels[[i]]
    )
  }

  attr(gdx_data,"src_gdxfile") = gdx_path
  attr(gdx_data,"filters") = filters
  attr(gdx_data,"column_names") = column_names
  attr(gdx_data,"uels.filters") = uels.filters


  return(gdx_data)


}



#' Load a symbol (set or parameter) from many gdx files
#'
#' Function to load many GDX files into one \code{data.table}.
#'
#' The GDX files should have the same structure.
#' <TODO> If not, an error should be thrown.
#'
#' @param symbol The symbol to load.
#' @param gdxfile_patt Character vector of file name patterns to match GDX files.
#'   Patterns are interpreted as shell-style wildcards (as in \code{Sys.glob()}):
#'   \itemize{
#'     \item \code{*} matches any number of characters
#'     \item \code{?} matches exactly one character
#'     \item \code{[...] } matches any of the specified characters
#'   }
#'   Multiple patterns can be provided in a vector, e.g.
#'   \code{c(".../batch_AT*.gdx", ".../batch_BE*.gdx")}.
#' @param custom_colnames Optional character vector specifying column names
#'   to explicitly assign to the GDX data.
#' @param filters A named list specifying filters to apply on the loaded data.
#'   Each element must be of the form \code{list(column_name = c("val1", "val2", ...))}.
#'   Filtering is case-insensitive.
#' @param keep_filename Logical. If \code{TRUE}, an extra column \code{src_file}
#'   is added indicating the filename that each record comes from.
#' @return A list with two elements: LOADING_INFO and DATA.
#' @export

read_from_many_gdxs <- function(symbol, gdxfile_patt,
                                custom_colnames = c(),
                                filters = list(),
                                keep_filename = FALSE) {

  LOADING_INFO <- data.table::data.table(FILE = character(), RECORDS = numeric())

  # Expand vector of patterns into a unified file list
  files.to.load <- unique(unlist(lapply(gdxfile_patt, Sys.glob)))

  cat("\nNumber of files to load: ", length(files.to.load), "\n")

  first_res <- TRUE
  for (gdx in files.to.load) {
    cat(paste0("R: Loading results from ", gdx, "\n"))

    tmp <- read_from_gdx(symbol, gdx_path = gdx,
                         custom_colnames = custom_colnames,
                         filters = filters)

    if (keep_filename) {
      tmp[, src_file := gdx]
    }

    LOADING_INFO <- rbindlist(list(
      LOADING_INFO,
      data.table(FILE = gdx, RECORDS = nrow(tmp))
    ), use.names = TRUE)

    if (first_res) {
      DATA <- tmp
      first_res <- FALSE
    } else {
      DATA <- rbindlist(list(DATA, tmp), use.names = TRUE)
    }
  }

  return(list(
    LOADING_INFO = LOADING_INFO,
    DATA = DATA
  ))
}



#' copy_gdx_symbol_tolist_element Function
#'
#' Reads a symbol from a gdx and creates a list element suitable for the GDXlist of ifmcap
#'
#' @param symbol The symbol to copy
#' @param gdx_path The path to the gdx file
#' @return a GDX.list with the following elements, \{ domains,ts,data\}
#' @export

copy_gdx_symbol_tolist_element = function(symbol,gdx_path) {

  tmp1 = read_from_gdx(symbol,gdx_path,get_ts = T)

  nms = colnames(tmp1)

  if(attr(tmp1,"isSet")==T) { domains.cur=nms} else {domains.cur=nms[1:(length(nms)-1)]}

  return(list(
    domains = domains.cur,
    ts = attr(tmp1,"ts"),
    data = data.table::copy(tmp1)
  ))

}


#' copy_all_gdx_symbols_tolist Function
#'
#' Reads all symbols from a gdx and returns a GDX.list
#'
#' @param gdx_path The path to the gdx file
#' @return a GDX.list with all the symbols in GDX.list
#' @export

copy_all_gdx_symbols_tolist = function(gdx_path,exclude_symbols=c()) {

  tmp1=gdxrrw::gdxInfo(gdx_path,dump = F,returnDF=T)

  symbols = c(tmp1$sets$name,tmp1$parameters$name)
  symbols <- setdiff(symbols, exclude_symbols)

  tmp2 = list()

  for (symbol in symbols) {

    cat("\nLoading ",symbol)

    tmp2[[symbol]]=copy_gdx_symbol_tolist_element(symbol,gdx_path)


  }

  return(tmp2)


}



#' Saves to gdx a GDX.list
#'
#' TODO explain what a GDX.list is
#'
#' @param GDX.list.cur A list of elements to save in the GDX with the following structure
#' @param save_gdx_path A string with the path of the GDX file
#' @return NULL
#' @export

save_to_gdx = function(GDX.list.cur,save_gdx_path) {

  GDX.to_save=list()

  for(n in names(GDX.list.cur)) {

    cat("Saving in GDX -> ",n,"\n")

    if("data.table" %in% class(GDX.list.cur[[n]][["data"]])) {
      tmp1 =data.table::copy(GDX.list.cur[[n]][["data"]])

      #check the number of elements in domain list and ocmpare to the number of columns
      data_col_num_min = length(GDX.list.cur[[n]][["domains"]])
      data_col_num_max = length(GDX.list.cur[[n]][["domains"]])+1
      if( ncol(GDX.list.cur[[n]][["data"]]) < data_col_num_min
          | ncol(GDX.list.cur[[n]][["data"]]) > data_col_num_max

      ) {
        stop("The number of columns in the domain (",paste0(GDX.list.cur[[n]][["domains"]],collapse=", ") ,
             ") is not compatible with the number of columns in the data (",
             paste0(colnames(GDX.list.cur[[n]][["data"]]),collapse=","), ")."
        )
      }

      #check the case where the data has a ts column, and this columns contains NA. Convert them to ""
      if ("ts" %in% names(tmp1)) {
        tmp1[, ts := fifelse(is.na(ts), "", ts)]  # Efficient replacement using `fifelse`
      }

      #check for NA
      na_counts <- sapply(tmp1, function(x) sum(is.na(x)))
      for (col in names(na_counts)) {
        if (na_counts[col] > 0) {
          warning(paste("\nSymbol: ",n,"\nColumn ", col, "has ", na_counts[col], " NA values"))
        }
      }

      #convert columns to factors
      names_factors = names(tmp1)[1:length(GDX.list.cur[[n]][["domains"]]) ]
      for(col in names_factors) {
        set(tmp1, j = col, value = factor(tmp1[[col]]))
      }

      #set attributes for saving the gdx
      attr(tmp1,"symName") = n
      attr(tmp1,"domains") = GDX.list.cur[[n]][["domains"]]
      attr(tmp1,"ts") = GDX.list.cur[[n]][["ts"]]

      GDX.to_save[[length(GDX.to_save)+1]] = droplevels(tmp1)

    }

    else if("numeric" %in% class(GDX.list.cur[[n]][["data"]])) {

      tmp1 = data.table::copy(GDX.list.cur[[n]][["data"]])
      attr(tmp1,"domains") =c()
      attr(tmp1,"symName") = n
      attr(tmp1,"ts") = GDX.list.cur[[n]][["ts"]]

      #check the case where the data has a ts column, and this columns contains NA. Convert them to ""
      if ("ts" %in% names(tmp1)) {
        tmp1[, ts := fifelse(is.na(ts), "", ts)]  # Efficient replacement using `fifelse`
      }


      GDX.to_save[[length(GDX.to_save)+1]] = data.table::copy(tmp1)

    }

    else {

      cat("\nSkipping ",n, '. It is not neither a data.table or a numeric.')

    }

  }



  cat("Writing the data to ",save_gdx_path)

  gdxrrw::wgdx.lst(
    save_gdx_path,
    GDX.to_save
  )


}
