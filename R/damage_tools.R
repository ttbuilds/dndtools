dpa <- function(to_hit, to_crit, dmg_formula)
{
  dmg_on_hit = parse_dice_expression(dmg_formula)
  (to_hit + to_crit) * dice_avg(dmg_on_hit) + to_hit * static_dmg(dmg_on_hit)
}

first_hit_is_crit <- function(to_hit, to_crit, n_atks)
{
  if(n_atks == 1) return(to_crit)
  else return(to_crit + (1 - to_hit) * first_hit_is_crit(to_hit, to_crit, n_atks - 1))
}

pad_with_last <- function(v,target_n)
{
  cur_n <- length(v)
  if(length(v) < target_n){
    v <- c(v, rep(v[cur_n], target_n - cur_n))
  }
  return(v)
}

dpt_by_attack <- function(atks, to_hit, to_crit){
  n = atks
  to_hit  = pad_with_last(to_hit,  n)
  to_crit = pad_with_last(to_crit, n)
  prob_nohit         = c(1, cumprod((1 - to_hit[1:(n-1)])))
  prob_nocrit        = c(1, cumprod((1 - to_crit[1:(n-1)])))
  prob_no_future_hit = rev(cumprod(c(1, rev(1 - to_hit)[1:n-1])))
  prob_future_crit   = 1 - rev(cumprod(c(1, rev(1 - to_crit)[1:n-1])))
  crit_fish          = prob_future_crit > prob_no_future_hit
  # cat("Chance we crit:", to_crit, "\n")
  # cat("Chance we hit:", to_hit, "\n")
  # cat("To crit fish?:", crit_fish, "\n")
  # cat("Chance we havent hit yet:", prob_nohit,"\n")
  # cat("Chance we haven't crit yet:", prob_nocrit,"\n")
  use_chance_if_remains <- c(to_crit[crit_fish], to_hit[!crit_fish])
  prob_remains = cumprod(c(1,1 - use_chance_if_remains[1:(n-1)]))
  # cat("Chance we have the thing left:",prob_remains,"\n")
  procs_by_atk = (2 * to_crit + (!crit_fish) * (to_hit-to_crit)) * prob_remains
  return(tibble(procs = procs_by_atk, use_on_noncrit = (1 - crit_fish)))
}


bonus_dpt <- function(to_hit, to_crit, n_atks, dmg_formula, crit_fish = TRUE, max_uses = 1)
{
  if(max_uses == 0 | n_atks == 0) return(0)
  bonus_dmg = parse_dice_expression(dmg_formula)
  if(n_atks == 1 | crit_fish == FALSE) {
    base_result   <- at_least_one_success(n_atks, to_hit) * total_dmg(bonus_dmg)
    bonus_is_crit <- first_hit_is_crit(to_hit, to_crit, n_atks)
    result        <- base_result + bonus_is_crit * dice_avg(bonus_dmg)
  } else {
    greedy_dmg          <-
      total_dmg(bonus_dmg) +
      bonus_dpt(to_hit, to_crit, n_atks - 1, dmg_formula, crit_fish = TRUE, max_uses = max_uses-1)
    future_expected_dpr <- bonus_dpt(
      to_hit, to_crit, n_atks - 1, dmg_formula, crit_fish = TRUE, max_uses = max_uses)
    # if(future_expected_dpr > greedy_dmg) print("Delay on hit")
    result <-
      to_crit            *
          (dice_avg(bonus_dmg) + greedy_dmg +
           bonus_dpt(to_hit, to_crit, n_atks-1, dmg_formula, crit_fish = TRUE, max_uses = max_uses-1)) +
      (1 - to_hit)       * future_expected_dpr  +
      (to_hit - to_crit) * max(greedy_dmg, future_expected_dpr)
  }
  return(result)
}
