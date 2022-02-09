#' @import tidyverse

mod <- function(ability_score)
{
  return(floor((ability_score - 10)/2))
}

get_proficiency <- function(level){
  return(floor((level+3)/4) + 1)
}

spell_slots_for_level <- function(caster_level, slot_level){
  switch(
    slot_level,
    ## 1st
    2 * (caster_level >= 1) + (caster_level >= 2) + (caster_level >= 3),
    ## 2nd
    2 * (caster_level >= 3) + (caster_level >= 4),
    ## 3rd
    2 * (caster_level >= 5) + (caster_level >= 6),
    ## 4th
    1 * (caster_level >= 7) + 1 * (caster_level >= 8) + 1 * (caster_level >= 9),
    ## 5th
    1 * (caster_level >= 9) + 1 * (caster_level >= 10) + 1 * (caster_level >= 18),
    ## 6th
    1 * (caster_level >= 11) + 1 * (caster_level >= 19),
    ## 7th
    1 * (caster_level >= 13) + 1 * (caster_level >= 20),
    ## 8th
    1 * (caster_level >= 15),
    ## 9th
    1 * (caster_level >= 17),
  )
}
