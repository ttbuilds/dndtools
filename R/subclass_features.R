#' @import tidyverse

BuildFrame$set("private", "add_subclass_features_", overwrite = TRUE,
  function(){
  subclass_list <- self$build$subclasslist
  for(subclass in subclass_list){
    private$add_subclass_(subclass)
  }
  invisible(self)
})

BuildFrame$set("private", "add_subclass_", overwrite = TRUE,
  function(subclassname){
    subclass_functions <- hash(
      Gloomstalker = add_gloomstalker,
      Battlemaster = add_battlemaster,
      Hexblade     = add_hexblade,
      Assassin     = add_assassin,
      Life         = add_life,
      DSS          = add_dss)
    private$.data <- private$.data %>%
      mutate("{tolower(subclassname)}" := as.integer(cumsum(new_subclass == subclassname) > 0))
    if(subclassname %in% keys(subclass_functions)){
      private$.data <- subclass_functions[[subclassname]](self)
    }
    invisible(self)
  })

add_gloomstalker <- function(build_frame){
  result <- build_frame$data %>%
    mutate(
      dread_ambusher = as.integer(gloomstalker),
      dread_dice     = paste0(dread_ambusher, "d8"))
  return(result)
}

add_battlemaster <- function(build_frame){
  result <- build_frame$data %>%
    mutate(
      precision_atk = battlemaster,
      )
  return(result)
}

add_hexblade <- function(build_frame){
  result <- build_frame$data %>%
    mutate(hbc = hexblade)
  return(result)
}

add_assassin <- function(build_frame){
  result <- build_frame$data %>%
    mutate(
      assassinate = assassin)
  return(result)
}

add_life <- function(build_frame){
  result <- build_frame$data
  return(result)
}

add_dss <- function(build_frame){
  result <- build_frame$data
  return(result)
}
