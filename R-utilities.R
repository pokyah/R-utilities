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

#+ ---------------------------------
#' ## Function to recursively source all the function stored in a folder (designed by its path)
#' 
#' Inspiration :
#' [1](https://stackoverflow.com/questions/32862426/load-all-files-from-folder-and-subfolders)
source_files_recursively.fun <- function(path.chr) {
  dirs <- list.dirs(path.chr, recursive = FALSE)
  dirs <- dirs[ ! grepl(".git", dirs) ]
  dirs <- dirs[ ! grepl(".Rproj.user", dirs) ]
  
  files <- list.files(path, pattern = "^.*[Rr]$", include.dirs = FALSE, full.names = TRUE)
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
