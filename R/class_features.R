BuildFrame$set("private", "add_class_features_", overwrite = TRUE,
  function(){
    class_list <- self$build$classlist
    private$.data <- private$.data %>%
      mutate(
        new_subclass         = "",
        asi_level            = as.integer(
          feat_at_1st(self$build) & level == 1 | current_class_level %in% c(4,8,12,16,19), 1, 0),
        caster_level         = 0,
        fighting_style_level = 0,
        extra_attack         = 0
      )
    for(c in class_list){
      private$add_class_(c)
    }
    private$.data <- private$.data %>% bind_cols(fighting_styles(self))
    invisible(self)
  })

BuildFrame$set("private", "add_class_", overwrite = TRUE,
    function(classname) {
      class_functions <- hash(
        Artificer = add_artificer,
        Barbarian = add_barbarian,
        Bard      = add_bard,
        Cleric    = add_cleric,
        Druid     = add_druid,
        Fighter   = add_fighter,
        Monk      = add_monk,
        Paladin   = add_paladin,
        Ranger    = add_ranger,
        Rogue     = add_rogue,
        Sorcerer  = add_sorcerer,
        Warlock   = add_warlock,
        Wizard    = add_wizard)
      private$.data <- private$.data %>%
        mutate(
          "{tolower(classname)}"                  := as.integer(cumsum(current_class == classname) > 0),
          "{paste0(tolower(classname),'_level')}" := cumsum(current_class == classname))
      if(classname %in% keys(class_functions)){
        private$.data <- class_functions[[classname]](self)
      }
      invisible(self)
    })


fighting_styles <- function(build_frame){
    known_styles <- c(
      "Archery",
      "Blindfighting",
      "Defense",
      "Dueling",
      "GWF",
      "TWF")
    taken_styles <- build_frame$build$feature_list("Fighting Styles")
    total_fighting_styles = sum(build_frame$data$fighting_style_level)
    result <- build_frame$data %>%
      select(fighting_style_level) %>%
        mutate(new_fighting_style  = "") %>%
        mutate(
          new_fighting_style = replace(
            new_fighting_style,
            fighting_style_level == 1,
            taken_styles[1:total_fighting_styles])) %>%
      select(-fighting_style_level)
    for(fs in known_styles){
      result <- result %>%
        mutate(
          "{tolower(fs)}" := as.integer(cumsum(new_fighting_style == fs) > 0)
        )
    }
    return(result)
  }


add_artificer <- function(build_frame){
  classname = "Artificer"
  subclassname = build_frame$build$subclass(classname)
  result <- build_frame$data %>%
    mutate(
      caster_level = caster_level + ceiling(artificer_level/2),
      new_subclass = replace(new_subclass, class_level == "Artificer 3", subclassname))
  return(result)
}

add_barbarian <- function(build_frame){
  classname = "Barbarian"
  subclassname = build_frame$build$subclass(classname)
  result <- build_frame$data %>%
    mutate(
      new_subclass = replace(new_subclass, class_level == "Barbarian 3", subclassname),
      extra_attack = if_else(barbarian_level >= 5,  pmax(extra_attack, 1), extra_attack))
  return(result)
}

add_bard <- function(build_frame){
  classname = "Bard"
  subclassname = build_frame$build$subclass(classname)
  result <- build_frame$data %>%
    mutate(
      new_subclass = replace(new_subclass, class_level == "Bard 3", subclassname),
      caster_level = caster_level + bard_level)
  return(result)
  }

add_cleric <- function(build_frame){
  classname = "Cleric"
  subclassname = build_frame$build$subclass(classname)
  result <- build_frame$data %>%
    mutate(
      new_subclass = replace(new_subclass, class_level == "Cleric 1", subclassname),
      caster_level = caster_level + cleric_level)
  return(result)
}

add_druid <- function(build_frame){
  classname = "Druid"
  subclassname = build_frame$build$subclass(classname)
  result <- build_frame$data %>%
    mutate(
      new_subclass = replace(new_subclass, class_level == "Druid 2", subclassname),
      caster_level = caster_level + druid_level)
  return(result)
}

add_fighter <- function(build_frame){
  classname = "Fighter"
  subclassname = build(build_frame) %>% subclass(classname)
  result <- build_frame$data %>%
    mutate(
      new_subclass = replace(new_subclass, class_level == "Fighter 3", subclassname),
      action_surge = as.integer(fighter_level >= 2),
      extra_attack = case_when(
        fighter_level >= 20 ~ pmax(extra_attack, 3),
        fighter_level >= 11 ~ pmax(extra_attack, 2),
        fighter_level >= 5 ~  pmax(extra_attack, 1),
        TRUE               ~  extra_attack
      ),
      asi_level = replace(asi_level, class_level %in% c("Fighter 6", "Fighter 14"), 1),
      fighting_style_level =
        if_else(class_level == "Fighter 1", 1, fighting_style_level)
    )
  return(result)
}

add_monk <- function(build_frame){
  classname = "Monk"
  subclassname = build(build_frame) %>% subclass(classname)
  result <- build_frame$data %>%
    mutate(
      new_subclass = replace(new_subclass, class_level == "Monk 3", subclassname),
      extra_attack = if_else(monk_level >= 5,  pmax(extra_attack, 1), extra_attack))
  return(result)
}

add_paladin <- function(build_frame){
  classname = "Paladin"
  subclassname = build(build_frame) %>% subclass(classname)
  result <- build_frame$data %>%
    mutate(
      new_subclass = replace(new_subclass, class_level == "Paladin 3", subclassname),
      divine_smite = as.integer(paladin_level >= 2),
      extra_attack = if_else(paladin_level >= 5,  pmax(extra_attack, 1), extra_attack),
      caster_level = caster_level + floor(paladin_level/2),
      fighting_style_level =
        if_else(class_level == "Paladin 2", 1, fighting_style_level))
  return(result)
}

add_ranger <- function(build_frame){
  classname = "Ranger"
  subclassname = build(build_frame) %>% subclass(classname)
  result <- build_frame$data %>%
    mutate(
      new_subclass = replace(new_subclass, class_level == "Ranger 3", subclassname),
      extra_attack = if_else(ranger_level >= 5,  pmax(extra_attack, 1), extra_attack),
      caster_level = caster_level + floor(ranger_level/2),
      fighting_style_level =
        if_else(class_level == "Ranger 2", 1, fighting_style_level))
  return(result)
}

add_rogue <- function(build_frame){
  classname = "Rogue"
  subclassname = build(build_frame) %>% subclass(classname)
  result <- build_frame$data %>%
    mutate(
      new_subclass = replace(new_subclass, class_level == "Rogue 3", subclassname),
      asi_level = replace(asi_level, class_level %in% c("Rogue 10"), 1),
      sneak_dice = paste0(ceiling(rogue_level/2), "d6")
    )
  return(result)
}

add_sorcerer <- function(build_frame){
  classname = "Sorcerer"
  subclassname = build(build_frame) %>% subclass(classname)
  result <- build_frame$data %>%
    mutate(
      new_subclass = replace(new_subclass, class_level == "Sorcerer 1", subclassname),
      caster_level = caster_level + sorcerer_level)
  return(result)
}

add_warlock <- function(build_frame){
  classname = "Warlock"
  subclassname = build_frame$build$ subclass(classname)
  invocations_taken = build_frame$build$feature_list("Eldritch Invocations")
  result <- build_frame$data %>%
    mutate(
      new_subclass = replace(new_subclass, class_level == "Warlock 1", subclassname),
      pact_slots    =
        (warlock_level >= 1) +
        (warlock_level >= 2) +
        (warlock_level >= 11) +
        (warlock_level >= 17),
      pact_slot_level  = ceiling(warlock_level/2),
      new_invocations  = (current_class == "Warlock") *
        ((current_class_level == 2) +
         (current_class_level %in% c(2,5,7,9,12,15,18))),
      mystic_arcanum_6 = mystic_arcanum_uses(warlock_level, 6),
      mystic_arcanum_7 = mystic_arcanum_uses(warlock_level, 7),
      mystic_arcanum_8 = mystic_arcanum_uses(warlock_level, 8),
      mystic_arcanum_9 = mystic_arcanum_uses(warlock_level, 9)
    ) %>%
    add_invocations(invocations_taken)
  return(result)
}

mystic_arcanum_uses <- function(warlock_level, spell_level){
  return(as.numeric(warlock_level >= 2*spell_level-1))
}

add_invocations <- function(build_frame_data, invocations_taken){
  total_invocations = length(invocations_taken)
  result <- build_frame_data %>%
    mutate(invocations_learned = "") %>%
    mutate(invocations_learned = replace(
      invocations_learned,
      new_invocations == 2,
      str_c(invocations_taken[1:2], collapse=","))) %>%
    mutate(invocations_learned = replace(
      invocations_learned,
      new_invocations == 1,
      invocations_taken[3:total_invocations]))
    for(invocation in invocations_taken){
      invocation <- gsub(" ", "_", invocation) %>% tolower()
      result <- result %>% add_invocation(invocation)
    }
  return(result)
}

add_invocation <- function(build_frame_data, invocation){
  invocation_functions <- hash(
    eldritch_smite  = add_eldritch_smite,
    agonizing_blast = do_nothing)
  result <- build_frame_data
  if(invocation %in% keys(invocation_functions)){
    result <- invocation_functions[[invocation]](result)
  }
  return(result)
}

add_eldritch_smite <- function(build_frame_data){
  invocation_name <- "eldritch_smite"
  result <- build_frame_data %>%
    mutate(
      eldritch_smite  = as.integer(cumsum(
        grepl(invocation_name, tolower(gsub(" ", "_", invocations_learned)))) > 0),
      smite_dice      = paste0(eldritch_smite * (1 + pact_slot_level), "d8")
    )
  return(result)
}


add_wizard <- function(build_frame){
  classname = "Wizard"
  subclassname = build_frame$build$subclass(classname)
  result <- build_frame$data %>%
    mutate(
      new_subclass = replace(new_subclass, class_level == "Wizard 2", subclassname),
      caster_level = caster_level + wizard_level)
  return(result)
}

unimplemented_class <- function(classname){
  message <- paste("The",classname, "class does not have any implemented mechanics")
  warning(message, call. = FALSE, noBreaks.=TRUE)
}

do_nothing <- function(build_frame){
  result <- build_frame$data
  return(result)
}
