.onLoad <- function(...){
  origView <- get("View", envir = .GlobalEnv)
  setGeneric("View", def = origView)
  setMethod("View", signature = "BuildFrame", def = function(x){
    View(as.data.frame(x), title = deparse(substitute(x)))})
}


