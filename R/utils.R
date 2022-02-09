#' @import tidyverse

options(dplyr.summarise.inform = FALSE)

hit_chance <- function(atk, AC, adv = 0, disadv = 0, elven_acc = 0){
  min_roll_tohit <- AC - atk
  base_tohit <- max(min((21 - min_roll_tohit) / 20, 0.95), 0.05)
  adv_result <- 1 - (1 - base_tohit)^(2+elven_acc)
  disadv_result <- base_tohit^2
  result <- case_when(
    adv > 0 & disadv == 0 ~ adv_result,
    adv == 0 & disadv > 0 ~ disadv_result,
    TRUE                  ~ base_tohit
  )
  return(result)
}

at_least_one_success <- function(n_tries, success_chance){
  return(1 - (1 - success_chance)^n_tries)
}

crit_chance <- function(min_to_crit = 20, adv = 0, elven_acc = 0){
  base_chance <- (21 - min_to_crit) / 20
  result <- 1 - (1 - base_chance)^(1+adv+adv*elven_acc)
  return(result)
}

check_save_chance <- function(bonus, DC, adv = 0, disadv = 0){
  min_roll_tosucceed <- DC - bonus
  base_chance <- max(min((21 - min_roll_tosucceed) / 20, 1.00), 0.00)
  adv_result <- 1 - (1 - base_chance)^2
  disadv_result <- base_chance^2
  result <- case_when(
    adv > 0 & disadv == 0 ~ adv_result,
    adv == 0 & disadv > 0 ~ disadv_result,
    TRUE                  ~ base_chance
  )
  return(result)
}

win_contest_pct <- function(
  bonus,
  opposing,
  win_ties = 0,
  adv      = 0, disadv     = 0,
  opp_adv  = 0, opp_disadv = 0)
{
  our_probs <- rep(0.05, 20)
  if(adv & !disadv) our_probs <- (39-2*(19:0))/400
  if(!adv & disadv) our_probs <- (39-2*(0:19))/400
  their_probs <- rep(0.05, 20)
  if(opp_adv & !opp_disadv) their_probs <- (39-19:0)/400
  if(!opp_adv & opp_disadv) their_probs <- (39-0:19)/400
  prob_matrix <- outer(our_probs, their_probs, "*")
  success_matrix <- outer(1:20+bonus, 1:20+opposing, `>`)
  win_pct <- sum(prob_matrix * success_matrix)
  return(win_pct)
}
