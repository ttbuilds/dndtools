#' @import tidyverse

BuildFrame$set("private", "add_asis_", overwrite = TRUE,
  function(){
    known_feats <- c("Alert", "CBE", "GWM", "Lucky", "PAM", "SS")
    taken_feats <- self$build$feature_list("Feats")
    total_asis  <- sum(self$data$asi_level)
    result <- private$.data %>%
      select(asi_level) %>%
      mutate(asi_feat = "") %>%
      mutate(asi_feat = replace(asi_feat, asi_level == 1, taken_feats[1:total_asis])) %>%
      select(-asi_level)
    for(feat in known_feats){
      result <- result %>%
        mutate(
          "{tolower(feat)}" := as.integer(cumsum(tolower(asi_feat) == tolower(feat)) > 0))
    }
    private$.data <- bind_cols(private$.data, result)
  })

BuildFrame$set("private", "add_ability_scores_", overwrite = TRUE,
  function(){
    known_stats <- c("str", "dex", "con", "int", "wis", "cha")
    result <- tibble(asi_sequence = pull(private$.data, asi_feat))
    for(stat in known_stats){
      result <- result %>%
        mutate("{stat}" := get_current_stat(self$build, asi_sequence, stat))
    }
    private$.data <- private$.data %>% bind_cols(result) %>% select(-asi_sequence)
  })

get_current_stat <- function(build, feats, ability){
  build$stat(ability) +
  cumsum(
    grepl(paste0("\\(",ability), feats, ignore.case = TRUE) +
    grepl(paste0(ability,"\\)"), feats, ignore.case = TRUE) -
    grepl(paste0("\\(",ability,"\\)"), feats, ignore.case = TRUE))
}
