## Maps and spatial related ----

#'################################################################################################################################################33
#' create NUTS fill map
#'
#' This function takes a numeric vector as input and computes various statistical measures, including mean, median, mode, and standard deviation.
#'
#' @seealso <https://r-charts.com/spatial/choropleth-map-ggplot2/>
#'
#' @param DT A data.table with only two columns: {NUTS0,NUTS1,NUTS2,NUTS3} and value.
#'
#' @param resol The NUTS resolution that the data refers to. Can be NUTS0,NUTS1,NUTS2,NUTS3
#'
#' @param scale_type Should it be discrete or continous
#'
#' @param scale_fill_discrete A list with options for scale_fill_manual. hcl.pals()
#'
#' @param scale_fill_continuous A list with options for scale_fill_gradient
#'
#' @return a ggplot object
#'
#'
#' @export

create_map_fill = function(DT,
                           title = "Untitled",
                           legend.title="Untitled",
                           legend.position.cur="right",
                           resol,  #Could be NUTS0,NUTS1,NUTS2,NUTS3
                           scale_type, #{discrete,continuous}
                           scale_fill_discrete = list(n=3,palette="Inferno",rev=F,alpha=0.7),
                           scale_fill_continuous = list(name = " ",low = "#FFFFFF",high = "#FF4200",mid="#FFFF00" ,midpoint=0, label_decimals = 1,barwidth=15),
                           nuts_year = 2016,
                           external_data= "U:/SCIENTIFIC/FARM @ U/30-Projects/01-IFM-CAP/04-Model External Data",
                           add.values.as.labels=F,
                           values.as.labels.format="%0.1f",
                           do.print=F,
                           bg_fill = "grey80",
                           xlim.cur=c(2200000, 7150000),
                           ylim.cur=c(1380000, 5500000),
                           debug=F) {

  if (!require(sf)) {    stop("sf not installed") }
  if (!require(dplyr)) {    stop("dplyr not installed") }

  #default values for scale_fill_discrete
  scale_fill_discrete.cur=list(
    n=ifelse(is.null(scale_fill_discrete$n),3,scale_fill_discrete$n),
    palette=ifelse(is.null(scale_fill_discrete$palette),"Inferno",scale_fill_discrete$palette),
    rev=ifelse(is.null(scale_fill_discrete$rev),FALSE,scale_fill_discrete$rev),
    alpha=ifelse(is.null(scale_fill_discrete$alpha),0.7,scale_fill_discrete$alpha)
  )

  #default values for scale_fill_discrete
  scale_fill_continuous.cur=list(
    name=ifelse(is.null(scale_fill_continuous$name)," ",scale_fill_continuous$name),
    low=ifelse(is.null(scale_fill_continuous$low),"#FFFFFF",scale_fill_continuous$low),
    high=ifelse(is.null(scale_fill_continuous$high),"#458B74",scale_fill_continuous$high),
    mid=ifelse(is.null(scale_fill_continuous$mid),"#FFFF00",scale_fill_continuous$mid),
    midpoint=ifelse(is.null(scale_fill_continuous$midpoint),mean(DT$value,na.rm=T),scale_fill_continuous$midpoint),
    label_decimals=ifelse(is.null(scale_fill_continuous$label_decimals),1,scale_fill_continuous$label_decimals),
    barwidth=ifelse(is.null(scale_fill_continuous$barwidth),15,scale_fill_continuous$barwidth)
  )

  if(debug){
    if(scale_type=="discrete") {
      print(scale_fill_discrete.cur)
    } else {
      print(scale_fill_continuous.cur)
    }
  }

  if(nuts_year==2013) {
    nuts_shp <- st_read(paste0(external_data,"/gis/NUTS_RG_20M_2013_3035.shp/NUTS_RG_20M_2013_3035.shp"))
  } else if (nuts_year==2016) {
    nuts_shp <- st_read(paste0(external_data,"/gis/NUTS_RG_20M_2016_3035.shp/NUTS_RG_20M_2016_3035.shp"))
  } else {
    stop("We do not have a shape file related to the definition of NUTS for year ",nuts_year)
  }


  nuts_shp.2 = left_join(
    nuts_shp[nuts_shp$NUTS_ID%in%unique(DT[[resol]]),],
    DT[,.(NUTS_ID=DT[[resol]],value_to_map=value)]
  )


  p = ggplot( nuts_shp.2  ) +
    geom_sf(data = nuts_shp[nuts_shp$LEVL_CODE==0,], fill = bg_fill, color = NA)+
    geom_sf(aes(fill = value_to_map),
            color = "white",
            linetype = 1,
            lwd = 0.25)+
    xlim(xlim.cur) +
    ylim(ylim.cur) +
    ggtitle(title) +
    labs(fill = legend.title)+
    theme(legend.position = legend.position.cur)  # Adjust the legend position


  if(scale_type=="discrete") {
    p=p+scale_fill_manual(values=hcl.colors(n = scale_fill_discrete.cur$n,
                                            palette = scale_fill_discrete.cur$palette,
                                            rev = scale_fill_discrete.cur$rev,
                                            alpha = scale_fill_discrete.cur$alpha))
  } else if(scale_type=="continuous") {

    format_labels <- function(x) {
      return(sprintf(paste0("%.",scale_fill_continuous.cur$label_decimals,"f"), x))
    }

    p=p+scale_fill_gradient2(name = scale_fill_continuous.cur$name,
                             low = scale_fill_continuous.cur$low,
                             high = scale_fill_continuous.cur$high,
                             mid=scale_fill_continuous.cur$mid,
                             midpoint=scale_fill_continuous.cur$midpoint,
                             labels=format_labels)+
      guides(fill = guide_colorbar(barwidth = scale_fill_continuous.cur$barwidth))
  }

  if(add.values.as.labels) {
    p=p+geom_sf_text(aes(label = sprintf(values.as.labels.format,value_to_map)))
  }

  if(do.print) { print(p)}

 return(p)


}



create_map_fill_scen = function(DT,
                           title = "Untitled",
                           legend.title="Untitled",
                           legend.position.cur="right",
                           resol,  #Could be NUTS0,NUTS1,NUTS2,NUTS3
                           scale_type, #{discrete,continuous}
                           scale_fill_discrete = list(n=3,palette="Inferno",rev=F,alpha=0.7),
                           scale_fill_continuous = list(name = " ",low = "#FFFFFF",high = "#FF4200",mid="#FFFF00" ,midpoint=0, label_decimals = 1,barwidth=15),
                           nuts_year = 2016,
                           external_data= "U:/SCIENTIFIC/FARM @ U/30-Projects/01-IFM-CAP/04-Model External Data",
                           add.values.as.labels=F,
                           values.as.labels.format="%0.1f",
                           do.print=F,
                           bg_fill = "grey80",
                           xlim.cur=c(2200000, 7150000),
                           ylim.cur=c(1380000, 5500000),
                           debug=F) {

  if (!require(sf)) {    stop("sf not installed") }
  if (!require(dplyr)) {    stop("dplyr not installed") }

  #default values for scale_fill_discrete
  scale_fill_discrete.cur=list(
    n=ifelse(is.null(scale_fill_discrete$n),3,scale_fill_discrete$n),
    palette=ifelse(is.null(scale_fill_discrete$palette),"Inferno",scale_fill_discrete$palette),
    rev=ifelse(is.null(scale_fill_discrete$rev),FALSE,scale_fill_discrete$rev),
    alpha=ifelse(is.null(scale_fill_discrete$alpha),0.7,scale_fill_discrete$alpha)
  )

  #default values for scale_fill_discrete
  scale_fill_continuous.cur=list(
    name=ifelse(is.null(scale_fill_continuous$name)," ",scale_fill_continuous$name),
    low=ifelse(is.null(scale_fill_continuous$low),"#FFFFFF",scale_fill_continuous$low),
    high=ifelse(is.null(scale_fill_continuous$high),"#458B74",scale_fill_continuous$high),
    mid=ifelse(is.null(scale_fill_continuous$mid),"#FFFF00",scale_fill_continuous$mid),
    midpoint=ifelse(is.null(scale_fill_continuous$midpoint),mean(DT$value,na.rm=T),scale_fill_continuous$midpoint),
    label_decimals=ifelse(is.null(scale_fill_continuous$label_decimals),1,scale_fill_continuous$label_decimals),
    barwidth=ifelse(is.null(scale_fill_continuous$barwidth),15,scale_fill_continuous$barwidth)
  )

  if(debug){
    if(scale_type=="discrete") {
      print(scale_fill_discrete.cur)
    } else {
      print(scale_fill_continuous.cur)
    }
  }

  if(nuts_year==2013) {
    nuts_shp <- st_read(paste0(external_data,"/gis/NUTS_RG_20M_2013_3035.shp/NUTS_RG_20M_2013_3035.shp"))
  } else if (nuts_year==2016) {
    nuts_shp <- st_read(paste0(external_data,"/gis/NUTS_RG_20M_2016_3035.shp/NUTS_RG_20M_2016_3035.shp"))
  } else {
    stop("We do not have a shape file related to the definition of NUTS for year ",nuts_year)
  }


  nuts_shp.2 = left_join(
    nuts_shp[nuts_shp$NUTS_ID%in%unique(DT[[resol]]),],
    DT[,.(NUTS_ID=DT[[resol]],SCEN,value_to_map=value)]
  )


  p = ggplot( nuts_shp.2  ) +
    geom_sf(data = nuts_shp[nuts_shp$LEVL_CODE==0,], fill = bg_fill, color = NA)+
    geom_sf(aes(fill = value_to_map),
            color = "white",
            linetype = 1,
            lwd = 0.25)+
    xlim(xlim.cur) +
    ylim(ylim.cur) +
    ggtitle(title) +
    labs(fill = legend.title)+
    theme(legend.position = legend.position.cur)  # Adjust the legend position


  if(scale_type=="discrete") {
    p=p+scale_fill_manual(values=hcl.colors(n = scale_fill_discrete.cur$n,
                                            palette = scale_fill_discrete.cur$palette,
                                            rev = scale_fill_discrete.cur$rev,
                                            alpha = scale_fill_discrete.cur$alpha))
  } else if(scale_type=="continuous") {

    format_labels <- function(x) {
      return(sprintf(paste0("%.",scale_fill_continuous.cur$label_decimals,"f"), x))
    }

    p=p+scale_fill_gradient2(name = scale_fill_continuous.cur$name,
                             low = scale_fill_continuous.cur$low,
                             high = scale_fill_continuous.cur$high,
                             mid=scale_fill_continuous.cur$mid,
                             midpoint=scale_fill_continuous.cur$midpoint,
                             labels=format_labels)+
      guides(fill = guide_colorbar(barwidth = scale_fill_continuous.cur$barwidth))
  }

  if(add.values.as.labels) {
    p=p+geom_sf_text(aes(label = sprintf(values.as.labels.format,value_to_map)))
  }

  if(do.print) { print(p)}

  return(p)


}
