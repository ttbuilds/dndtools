require(hash)
require(R6)

BuildFrame <- R6Class(
  classname = "BuildFrame",
  public = list(
    initialize = function(build, game){
      private$.build = as(build, "CharacterBuild")
      private$.game  = as(game, "GameParameters")
      private$.data = tibble(
        build_label = name(build)) %>%
        bind_cols(progression(build)) %>%
        mutate(prof = get_proficiency(level))
      private$add_class_features_()
      private$add_subclass_features_()
      private$add_asis_()
      private$add_ability_scores_()
      private$add_spell_slots_()
      private$add_attack_data_()
    }
  ),
  active = list(
    data  = function(){return(private$.data)},
    build = function(){return(private$.build)},
    game  = function(){return(private$.game)}
  ),
  private = list(
    .data  = NULL,
    .build = NULL,
    .game  = NULL,
    add_class_features_    = function() {},
    add_class_             = function(classname) {},
    add_subclass_features_ = function() {},
    add_subclass_          = function(subclassname) {},
    add_asis_              = function() {},
    add_ability_scores_    = function() {},
    add_spell_slots_       = function() {},
    add_attack_data_       = function() {}
  )
)

make_build_frame <- function(build, game_params){
  return(BuildFrame$new(build = build, game = game_params))
}

setOldClass(c("BuildFrame", "R6"))
setMethod("show", signature = "BuildFrame",
  definition = function(object){
    show(object$.data)
} )

setMethod("as.data.frame", signature = "BuildFrame", definition = function(x){
  return(x$data)
})


setGeneric("build", def = function(object){standardGeneric("build")})
setMethod("build", signature = "BuildFrame",
  definition = function(object){
    return(object$build)
  })

setGeneric("game", def = function(object){standardGeneric("game")})
setMethod("game", signature = "BuildFrame",
  definition = function(object){
    return(theObject$game)
  })

BuildFrame$set("private", "add_attack_data_", overwrite = TRUE,
  function() {
    weapon_data <- self$game$weapons
    baseline_ac <- self$game$baseline_ac
    private$.data <- private$.data %>%
      mutate(
        init_mod        = mod(dex) + 5 * alert,
        default_ac      = baseline_ac(level),
        atks_per_action = 1 + extra_attack,
        weapon          = eval(parse(text = self$build$policy("weapon")))) %>%
    rowwise() %>%
    mutate(
      weapon_dice    = weapon_data$dice(weapon),
      weapon_ability = weapon_data$ability(weapon),
      weapon_abi_mod = parse(text = weapon_ability) %>% eval() %>% mod(),
      magic_weapon   = eval(self$game$magic_bonus),
      atk_mod        =
        prof + weapon_abi_mod + 2 * archery * weapon_data$is_ranged(weapon) + magic_weapon,
      ba_atks        = as.integer(
        1 * (cbe * weapon_data$has_tag(weapon, "cbe_bonus")) |
             pam * weapon_data$is_polearm(weapon))) %>%
    ungroup()
  })

BuildFrame$set("private", "add_spell_slots_", overwrite = TRUE,
  function() {
    result <- private$.data %>% select(caster_level)
    for(spell_level in 1:9){
      result <- result %>%
        mutate(
          "{paste0('spell_slots_', spell_level)}" :=
            spell_slots_for_level(caster_level, spell_level))
    }
    result <- result %>% select(-caster_level)
    private$.data <- private$.data %>% bind_cols(result)
  })
