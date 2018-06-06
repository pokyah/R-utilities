#'---
#'author: "Thomas Goossens - hello.pokyah@gmail.com"
#'output: 
#'  html_document:
#'    theme: default
#'    toc: false
#'    toc_depth: 6
#'    toc_float:
#'      collapsed: false
#'      smooth_scroll: true
#'title: "R utility functions"
#'date: \ 18-04-2018\
#'---


build_topo_rasters.fun <- function() {
  # Loading the required library
  library(raster)
  library(rgdal)
  library(dplyr)
  
  # Get the Wallonia DEM
  bel.ele.ras = getData("alt", country = "BE", mask = TRUE)
  
  # but what are the units ? h
  # it seems that the data are not projected but are in longlat so we need to project it to get the units
  bel.ele.ras <- projectRaster(bel.ele.ras, crs = toString((dplyr::filter(rgdal::make_EPSG(), code=="3812"))$prj4))
  
  # compute the slope from the elevation
  bel.slope.ras <- terrain(bel.ele.ras, opt="slope", unit="degrees")
  
  # compute the aspect from the elevation
  bel.aspect.ras <- terrain(bel.ele.ras, opt="aspect", unit="degrees")
  
  # create the stack of rasters
  topo.stack.ras <- stack(bel.ele.ras, bel.slope.ras, bel.aspect.ras)
  
  # Return the stack of rasters
  return(topo.stack.ras)
}

#+ ---------------------------------
#' ## Function to create a spatial grid of the desired resolution for Wallonia using sp
#' * `res.num` is a numeric that expresses the desired resolution expressed in kilometers
#' 
#' Inspiration :  
#' [1]https://stackoverflow.com/questions/41787313/how-to-create-a-grid-of-spatial-points

build_wal_grid.sp.fun <- function(res.num){
  # load some spatial data. Administrative Boundary
  be.sp <- getData('GADM', country = 'BE', level = 1, download = FALSE)
  be.sp$NAME_1
  wallonie.sp <- be.sp[be.sp$NAME_1 == "Wallonie",]

  # check the CRS to know which map units are used  
  proj4string(wallonie.sp)

  # set CRS to "lambert 2008" which EPSG is 3812
  wallonie.3812.sp <- spTransform(wallonie.sp, CRS(projargs = dplyr::filter(rgdal::make_EPSG(), code == "3812")$prj4))

  # Create a grid of points within the bbox of the SpatialPolygonsDataFrame 
  # wallonie with meters as map units
  grid <- makegrid(wallonie.3812.sp, cellsize = res.num) # cellsize in map units!

  # grid is a data.frame. To change it to a spatial data set we have to :
  grid <- SpatialPoints(grid, proj4string = CRS(proj4string(wallonie.3812.sp)))

  # subset to wallonie polygon
  grid <- grid[wallonie.3812.sp, ]

  # Return the clipped grid
  return(grid)
}



#+ ---------------------------------
#' ## Function to create a spatial grid of the desired resolution for Wallonia using sf
#' * `res.num` is a numeric that expresses the desired resolution expressed in kilometers
#' * `geom.chr` is a character that expresses the [desired geometry](https://www.rdocumentation.org/packages/sf/versions/0.6-1/topics/st_make_grid). Can take the values `"polygons"`, `"centers"` and `"corners`
#' 
#' Inspiration :  
#' [1](https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/OverviewCoordinateReferenceSystems.pdf)
#' [2](https://gis.stackexchange.com/questions/22843/converting-decimal-degrees-units-to-km-in-r)
#' [3](https://stackoverflow.com/questions/48727511/r-grid-of-points-from-polygon-input)
build_wal_grid.sf.fun <- function(res.num, geom.chr) {
  library(sf)
  library(dplyr)
  library(raster)
  
  # Get the Wallonia adminisrative boundary - default EPSG is 4326
  wallonia.4326.sf <- dplyr::filter(st_as_sf(getData('GADM', country="BE", level=1)), NAME_1=="Wallonie")
  
  # convert from geographic lat/lon to projected in meters
  wallonia.3812.sf <- sf::st_transform(wallonia.4326.sf, 3812) # projected EPSG for Belgian Lambert 2008 
  
  # make the grid and clip it with the Wallonia boundaries 
  # st_intersection(st_make_grid(wallonia.sf, n=c(100,100), what="centers"), dataset.sf)
  grid.sf <- st_make_grid(wallonia.3812.sf, cellsize = res.num*1000, what=geom.chr)
  
  # Clip the grid with the administrative boundary of Wallonia
  grid.sf <- st_intersection(grid.sf, wallonia.3812.sf)
  
  # Return the clipped grid
  return(grid.sf)  
}

#+ ---------------------------------
#' ## Function to recursively source all the function stored in a folder (designed by its path)
#' 
#' Inspiration :
#' [1](https://stackoverflow.com/questions/32862426/load-all-files-from-folder-and-subfolders)
source_files_recursively.fun <- function(path.chr) {
  dirs <- list.dirs(path.chr, recursive = FALSE)
  dirs <- dirs[ ! grepl(".git", dirs) ]
  dirs <- dirs[ ! grepl(".Rproj.user", dirs) ]
  
  files <- list.files(path.chr, pattern = "^.*[Rr]$", include.dirs = FALSE, full.names = TRUE)
  for (f in files)
    source(f)
  for (d in dirs)
    source_files_recursively.fun(d)
}

#+ ---------------------------------
#' ## Function to list all the packages installed by user.
#' inspiration:
#' [1](https://stackoverflow.com/questions/38481980/get-the-list-of-installed-packages-by-user-in-r#40120266)
get_installed_packages.fun <- function(){ 
  ip = as.data.frame(installed.packages()[,c(1,3:4)])
  ip = ip[is.na(ip$Priority),1:2,drop=FALSE]
  return(ip)
}

#+ ---------------------------------
#' ## Function to transform a linear model list output to a dataframe.
#' 
#' Useful for markdown export with [kable](https://www.rdocumentation.org/packages/knitr/versions/1.20/topics/kable)
#' inspiration:
#' [1](http://r.789695.n4.nabble.com/Export-summary-from-regression-output-td4647109.html)
lm_output_to_df.fun <-function(lm.l){ 
  values<-c(paste(as.character(summary(lm.l)$call),collapse=" "), 
            lm.l$coefficients[1], 
            lm.l$coefficients[2], 
            length(lm.l$model), 
            summary(lm.l)$coefficients[2,2], 
            summary(lm.l)$r.squared, 
            summary(lm.l)$adj.r.squared, 
            summary(lm.l)$fstatistic, 
            pf(summary(lm.l)$fstatistic[1],summary(lm.l)$fstatistic[2],summary(lm.l)$fstatistic[3],lower.tail=FALSE)) 
  names(values)<-c("call","intercept","slope","n","slope.SE","r.squared","Adj. r.squared","F-statistic","numdf","dendf","p.value") 
  return(data.frame(values))
}
  
#+ ---------------------------------
#' ## Terms of service 
#' The present script is available under the [GNU-GPL V3](https://www.gnu.org/licenses/gpl-3.0.en.html) license and comes with ABSOLUTELY NO WARRANTY.
#' 
#' Copyright : Thomas Goossens - hello.pokyah@gmail.com 2018.  
#' 
#' *(This document was generated using [R software](https://www.r-project.org/) with the [knitr library](https://deanattali.com/2015/03/24/knitrs-best-hidden-gem-spin/))*.  
#+ TOS,echo=TRUE,warning=FALSE,message=FALSE,error=FALSE  
