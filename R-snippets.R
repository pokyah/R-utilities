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
#'title: "R utility snippets"
#'date: \ 18-04-2018\
#'---

#+ ---------------------------------
#' Try to avoid loops in R … but if needed, add a smart progress bar ;-)
#' help(txtProgressBar) in R

seq <- 0:20
n <- 0
pb <- txtProgressBar(style=3) ## try also style=1 (default)
for (i in seq) {
    n <- n+1
    cat(n)
    setTxtProgressBar(pb, n/length(seq))
    Sys.sleep(0.25)   
}
