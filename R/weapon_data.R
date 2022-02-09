#' @import tidyverse
require(hash)
require(R6)

Weapon <- R6Class(
  classname = "Weapon",
  public = list(
    initialize = function(name, dice, tags, ability){
      self$name = as.character(name)
      self$dice = as.character(dice)
      self$tags = parse(tags)
    },
    name    = NULL,
    dice    = NULL,
    tags    = NULL,
    ability = NULL
  ))

WeaponTable <- R6Class(
  classname = "WeaponTable",
  public = list(
    initialize  = function(data_table){
      private$.data = as(data_table, "data.frame") %>%
        column_to_rownames("id")},
    weapon      = function(weapon){
      entry = self$data[id,]
      Weapon$new(
        name = entry$name,
        dice = entry$dice,
        tags = entry$tags,
        ability = self$ability(weapon))},
    get         = function(weapon, property){return(self$data[weapon, property])},
    name        = function(weapon){self$get(weapon,"name")},
    dice        = function(weapon){self$get(weapon,"dice")},
    tags        = function(weapon){self$get(weapon,"tags")},
    has_tag     = function(weapon, tag){grepl(tag, self$tags(weapon), ignore.case = TRUE)},
    is_ranged   = function(weapon){self$has_tag(weapon, "ranged")},
    is_finesse  = function(weapon){self$has_tag(weapon, "finesse")},
    is_polearm  = function(weapon){self$has_tag(weapon, "polearm")},
    cbe_bonus   = function(weapon){self$has_tag(weapon, "cbe_bonus")},
    is_heavy    = function(weapon){self$has_tag(weapon, "heavy")},
    ability     = function(weapon){
      if_else(
        self$is_finesse(weapon),
        "pmax(str,dex)",
        if_else(
          self$is_ranged(weapon),
          "dex",
          "str"))}
  ),
  active = list(
    data = function(){return(private$.data)}
  ),
  private = list(
    .data = NULL
  ))

read_weapon_data <- function(file_path){
  weapon_data <- read_csv(file_path, col_types = "cccc", show_col_types = FALSE)
  return(WeaponTable$new(data_table = weapon_data))
}

setOldClass(c("WeaponTable", "R6"))
setMethod("show", "WeaponTable", def = function(object){
  show(object$data)
})

setOldClass(c("Weapon", "R6"))
setMethod("show", signature = c("Weapon"), definition = function(object){
  cat("Weapon:",object$name,"\n")
  cat("Damage Dice:",object$dice,"\n")
  cat("Properties:",object$tags,"\n")
})
