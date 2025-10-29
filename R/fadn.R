## FADN related ----


#TODO
## Retrieve TF and other columns the correspondence of levels and labels. Do this for different ifmcap versions

#TODO
## Move get_FARM_INFO functions to a file functions.R into the different versions folder

#'################################################################################################################################################33
#' get_farm_info Function
#'
#' This function retrieves information data for farms from FADN, for the baseyear
#'
#' @param database_dir The database directory
#' @param d_fadn_gdx The gdx with the export of data from the d.fadn
#' @param d_fadn_tables The gdx with the FADN tables
#' @param fadn_dictionary An excel file that has the following sheets TF14, ESC that give the correspondence of TF14 to TF14.text and ESC to ESC6.text
#' @param additional_AB_columns A vector with names of the TABLE_AB table to include
#' @return DT with the FDALL and the NUTS0,NUTS1,NUTS2,TF14,TF8,ECS,ORGANIC,UAA,Weight and their text description
#' export
get_FARM_INFO <- function(
    database_dir = "E:/IFM_CAP2/Database2020",
    d_fadn_gdx = "d.fadn/ifm_cap_out/d_fadn_ifm_cap_data_2020.gdx",
    d_fadn_tables = "d.fadn/ifm_cap_out/d_fadn_ifm_cap_data_2020.FADN_tables.GDX",
    fadn_dictionary = "U:/SCIENTIFIC/FARM @ U/30-Projects/01-IFM-CAP/04-Model External Data/fadn/dictionary.xlsx",
    additional_AB_columns = c()
) {

  d_fadn_gdx.path = paste0(database_dir,"/",d_fadn_gdx)

  tmp1 = read_from_gdx("FD_TO_NUTS3",d_fadn_gdx.path)
  tmp1$NUTS2 = substring(tmp1$NUTS3,1,4)
  tmp1$NUTS1 = substring(tmp1$NUTS3,1,3)
  tmp1$NUTS0 = substring(tmp1$NUTS3,1,2)


  tmp1 = merge(
    tmp1,
    merge(
      read_from_gdx("FD_TO_TF14",d_fadn_gdx.path)
      ,
      data.table::data.table(
        openxlsx::read.xlsx(
          fadn_dictionary,
          "TF14"
        )
      )[,.(TF14=factor(TF14),TF14.text,TF8.text=factor(TF8.text),TF3.text)],
      by="TF14"
    ),
    by="FDALL"
  )


  tmp1 = merge(
    tmp1,
    read_from_gdx("FD_TO_ORGANIC",d_fadn_gdx.path),
    by="FDALL"
  )

  tmp1 = merge(
    tmp1,
    merge(
      read_from_gdx("FD_TO_ESC",d_fadn_gdx.path),
      data.table::data.table(
        openxlsx::read.xlsx(
          fadn_dictionary,
          "ESC"
        )
      )[,.(ESC=factor(ESC),ESC14.text,ESC9.text,ESC6.text,ESC6.text.short=factor(ESC6.text.short))],
      by = "ESC"
    ),
    by="FDALL"
  )

  tmp1 = merge(
    tmp1,
    read_from_gdx("p_Weight",d_fadn_gdx.path)[,.(FDALL,w=value)],
    by="FDALL"
  )

  tmp1 = merge(
    tmp1,
    read_from_gdx("p_UAAR",d_fadn_gdx.path)[,.(FDALL,uaar=value)],
    by="FDALL"
  )


  tmp1[,TF14.text:=factor(
    as.character(TF14.text),
    levels = unique(data.table(
      openxlsx::read.xlsx(
        fadn_dictionary,
        "TF14"
      )
    )[,TF14.text])
  )
  ]


  tmp1[,TF8.text:=factor(
    as.character(TF8.text),
    levels = unique(data.table(
      openxlsx::read.xlsx(
        fadn_dictionary,
        "TF14"
      )
    )[,TF8.text])
  )
  ]


  tmp1[,TF3.text:=factor(
    as.character(TF3.text),
    levels = unique(data.table(
      openxlsx::read.xlsx(
        fadn_dictionary,
        "TF14"
      )
    )[,TF3.text])
  )
  ]

  tmp1[,ESC14.text:=factor(
    as.character(ESC14.text),
    levels = unique(data.table(
      openxlsx::read.xlsx(
        fadn_dictionary,
        "ESC"
      )
    )[,ESC14.text])
  )
  ]

  tmp1[,ESC6.text:=factor(
    as.character(ESC6.text),
    levels = unique(data.table(
      openxlsx::read.xlsx(
        fadn_dictionary,
        "ESC"
      )
    )[,ESC6.text])
  )
  ]


  tmp1[,ESC6.text.short:=factor(
    as.character(ESC6.text.short),
    levels = unique(data.table(
      openxlsx::read.xlsx(
        fadn_dictionary,
        "ESC"
      )
    )[,ESC6.text.short])
  )
  ]



  #retrieve additional columns form FADN AB tables
  if(length(additional_AB_columns)>0) {

    tmp2 = read_from_gdx("TABLE_AB",paste0(database_dir,"/",d_fadn_tables))

    tmp2 = tmp2[FADN_VAR%in%additional_AB_columns,.(FDALL,FADN_VAR,value)]

    tmp2 = dcast(tmp2,FDALL~FADN_VAR,value.var = "value")

    if(nrow(tmp2)==0) {
      warning("None of the additional columns are present")
    } else {
      tmp1 = merge(
        tmp1,
        tmp2,
        by="FDALL"
      )
    }



  }



  return(tmp1)


}



#'################################################################################################################################################33
#' get_FARM_INFO_allYears Function
#'
#' This function retrieves information data for farms from FADN, for all years. It uses the d.fadn module.
#'
#' @param d_fadn_folder The folder with the data from the d.fadn that contains the rds files with the fadn tables (tableAB.rds, tableSE.rds, etc.)
#' @param fadn_dictionary An excel file that has the following sheets TF14, ESC that give the correspondence of TF14 to TF14.text and ESC to ESC6.text
#' @return DT with the FDALL and YEAR, and the NUTS0,NUTS1,NUTS2,TF14,TF8,ECS,ORGANIC,UAA,Weight and their text description
#' export
get_FARM_INFO_allYears <- function(
    d_fadn_folder = "E:/IFM_CAP2/Database2020/d.fadn/filtered_load",
    fadn_dictionary = "U:/SCIENTIFIC/FARM @ U/30-Projects/01-IFM-CAP/04-Model External Data/fadn/dictionary.xlsx"
) {

  #read from tableAB
  tmp1 = readRDS(paste0(d_fadn_folder,"/tableAB.rds"))[,.(FDALL=factor(ID),YEAR,NUTS0,NUTS1,NUTS2,NUTS3,
                                                          TF14=factor(TF14),ESC=factor(SIZC),ORGANIC,
                                                          uaa=UAAOWNED+UAARENTED,w=SYS02)]

  #TF14
  tmp1 = merge(
    tmp1,
    data.table::data.table(
      openxlsx::read.xlsx(
        fadn_dictionary,
        "TF14"
      )
    )[,.(TF14=factor(TF14),TF14.text,TF8.text=factor(TF8.text),TF3.text)],
    by="TF14"
  )

  #organic
  org_codes.l <- list(
    "2013_bef" = list(`1` = "NO_ORGA", `2` = "ORGA", `3` = "MIXT_CONVERT"),
    "2014_aft" = list(`1` = "NO_ORGA", `2` = "ORGA", `3` = "MIXT", `4` = "CONVERT")
  )
  org_codes <- data.table(year = character(), code = integer(), text = character())
  for (year in names(org_codes.l)) {
    codes <- org_codes.l[[year]]
    temp_dt <- data.table(
      year = year,
      code = as.integer(names(codes)),
      text = unlist(codes)
    )
    org_codes <- rbind(org_codes, temp_dt, fill = TRUE)
  }

  tmp1= rbindlist(use.names=T,list(

    merge(
      tmp1[YEAR<2014],
      org_codes[year=="2013_bef",.(ORGANIC=code,ORGANIC.text=text)],
      by="ORGANIC"
    ),

    merge(
      tmp1[YEAR>2013],
      org_codes[year=="2014_aft",.(ORGANIC=code,ORGANIC.text=text)],
      by="ORGANIC"
    )

  ))


  #Economic size
  tmp1 = merge(
    tmp1,
    data.table(
      openxlsx::read.xlsx(
        fadn_dictionary,
        "ESC"
      )
    )[,.(ESC=factor(ESC),ESC14.text,ESC9.text,ESC6.text,ESC6.text.short=factor(ESC6.text.short))],
    by = "ESC"
  )

  tmp1[,TF14.text:=factor(
    as.character(TF14.text),
    levels = unique(data.table::data.table(
      openxlsx::read.xlsx(
        fadn_dictionary,
        "TF14"
      )
    )[,TF14.text])
  )
  ]


  tmp1[,TF8.text:=factor(
    as.character(TF8.text),
    levels = unique(data.table::data.table(
      openxlsx::read.xlsx(
        fadn_dictionary,
        "TF14"
      )
    )[,TF8.text])
  )
  ]

  tmp1[,TF3.text:=factor(
    as.character(TF3.text),
    levels = unique(data.table::data.table(
      openxlsx::read.xlsx(
        fadn_dictionary,
        "TF14"
      )
    )[,TF3.text])
  )
  ]

  tmp1[,ESC14.text:=factor(
    as.character(ESC14.text),
    levels = unique(data.table::data.table(
      openxlsx::read.xlsx(
        fadn_dictionary,
        "ESC"
      )
    )[,ESC14.text])
  )
  ]

  tmp1[,ESC6.text:=factor(
    as.character(ESC6.text),
    levels = unique(data.table::data.table(
      openxlsx::read.xlsx(
        fadn_dictionary,
        "ESC"
      )
    )[,ESC6.text])
  )
  ]

  tmp1[,ESC6.text.short:=factor(
    as.character(ESC6.text.short),
    levels = unique(data.table::data.table(
      openxlsx::read.xlsx(
        fadn_dictionary,
        "ESC"
      )
    )[,ESC6.text.short])
  )
  ]


  return(tmp1)


}
