#' @import tidyverse
require(hash)
require(R6)

GameParameters <- R6Class(
  classname = "GameParameters",
  public    = list(
    initialize = function(weapons, magic_weapon_progression, enemy_init, baseline_ac) {
      private$.weapons                  = as(weapons, "WeaponTable")
      private$.magic_weapon_progression = parse(text = magic_weapon_progression)
      private$.enemy_init               = as.integer(enemy_init)
      private$.baseline_ac              = baseline_ac
    }
  ),
  active = list(
    weapons = function(value){
      if(missing(value)) return(private$.weapons)
      else private$.weapons = WeaponTable$new(value)
    },
    magic_bonus = function(value){
      if(missing(value)) return(private$.magic_weapon_progression)
      else private$.magic_weapon_progression = parse(text = magic_weapon_progression)
    },
    enemy_init = function(value){
      if(missing(value)) return(private$.enemy_init)
      else private$.enemy_init = as.numeric(value)
    },
    baseline_ac = function(value){
      if(missing(value)) return(private$baseline_ac)
      else private$.baseline_ac = value
    }
  ),
  private = list(
    .weapons                  = NULL,
    .magic_weapon_progression = NULL,
    .enemy_init               = NULL,
    .baseline_ac              = NULL
))

make_game_params <- function(weapon_file, magic_weapon_progression, enemy_init_mod){
  weapon_data <- read_weapon_data(weapon_file)
  result <- GameParameters$new(
    weapons                  = weapon_data,
    magic_weapon_progression = magic_weapon_progression,
    enemy_init               = enemy_init_mod,
    baseline_ac              = baseline_ac)
  return(result)
}

baseline_ac <- function(level){
  return(11 + (level>=4) + (level>=8) + get_proficiency(level))
}
