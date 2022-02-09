require(hash)
require(R6)

CharacterBuild <-
  R6Class(
    classname = "CharacterBuild",
    public = list(
      initialize = function(
        buildname, progression, subclasses, stats, features, feat_at_1st, policies){
          private$.name         = as(buildname, "character")
          private$.progression  = as(progression, "tbl_df")
          private$.subclasses   = as(subclasses, "hash")
          private$.stats        = as(stats, "StatMap")
          private$.features     = as(features, "FeatureMap")
          private$.feat_at_1st  = as(feat_at_1st, "logical")
          private$.policies     = as(policies, "PolicyMap")
          private$.classlist    = unique(private$.progression$current_class)
          private$.subclasslist = values(private$.subclasses)
      },
      print = function(){
          cat("Build: ", self$name, "\n\n")
          cat("Starting Stats:\n")
          cat(" ")
          show(self$stats)
          cat("\n")
          show(self$features)
          cat("\n")
          cat("Level Progression:\n\n")
          show(self$progression)
      },
      stat = function(statname, value){
        if(missing(value)) return(private$.stats[[statname]])
        else private$.stats[[statname]] <- as.integer(value)
      },
      subclass = function(subclassname, value){
        if(missing(value)) return(private$.subclasses[[subclassname]])
        else private$.subclasses[[subclassname]] <- as.character(subclassname)
      },
      feature_list = function(featuretype){
        return(private$.features[[featuretype]])
      },
      policy = function(policytype){
        return(private$.policies[[policytype]])
      }
    ),
    active = list(
      name = function(){return(private$.name)},
      progression = function(){return(private$.progression)},
      stats = function(){return(private$.stats)},
      subclasses = function(){return(private$.subclasses)},
      features = function(){return(private$.features)},
      feat_at_1st = function(value){
        if(missing(value)) return(private$.feat_at_1st)
        else private$.feat_at_1st = value
      },
      policies = function(){return(private$.policies)},
      classlist = function(){
        return(private$.classlist)
      },
      subclasslist = function(){
        return(private$.subclasslist)
      }
    ),
    private = list(
      .name        = NULL,
      .progression = NULL,
      .stats       = NULL,
      .subclasses  = NULL,
      .features    = NULL,
      .feat_at_1st = NULL,
      .policies    = NULL,
      .classlist   = NULL,
      .subclasslist= NULL
    )
  )

new_stat_map     <- setClass("StatMap",    contains = "hash")
new_feature_list <- setClass("FeatureMap", contains = "hash")
new_policies     <- setClass("PolicyMap",  contains = "hash")

read_build_from_files <- function(
  buildname,
  progression_file,
  subclasses_file,
  stats_file,
  feature_file         = NULL,
  feat_at_first        = FALSE,
  policyfile           = NULL){
  progression <- read_progression(progression_file)
  statmap <- read_stats(stats_file)
  build <- CharacterBuild$new(
    buildname      = buildname,
    progression    = progression$progression,
    subclasses     = read_subclasses(progression$subclasses),
    stats          = read_stats(stats_file),
    features       = read_features(feature_file),
    feat_at_1st    = feat_at_first,
    policies       = read_policies(policyfile))
  return(build)}


read_progression <- function(filename){
  require(tidyverse)
  level_list <- read_csv(filename, col_names = FALSE, show_col_types = FALSE)
  colnames(level_list) <- "classlevel"
  pattern <- "^([A-Za-z]+) *(\\(([A-Za-z ]+)\\))? *([0-9]+)?-?([0-9]+)?"
  level_list <- level_list %>%
    mutate(
      base_class    = gsub(pattern, "\\1", classlevel),
      subclass      = gsub(pattern, "\\3", classlevel),
      initial_level = gsub(pattern, "\\4", classlevel),
      end_level     = gsub(pattern, "\\5", classlevel),
    ) %>%
    mutate(
      end_level = as.integer(ifelse(end_level == "", initial_level, end_level))) %>%
     group_by(base_class) %>%
    mutate(prev_max = lag(end_level)) %>%
    ungroup() %>%
    mutate(
      initial_level = as.integer(ifelse(initial_level == "", prev_max+1, initial_level)))
  levels <- tibble(
    class_level = with(level_list,
      mapply(paste, base_class,
        mapply(seq, from = initial_level, to = end_level)) %>%
      unlist())) %>%
    mutate(
      current_class       = gsub("([A-Za-z]+) ?([0-9]+)", "\\1", class_level),
      current_class_level = gsub("([A-Za-z]+) ?([0-9]+)", "\\2", class_level))
  progression <- bind_cols(level = 1:nrow(levels), levels)
  subclasses <-
    level_list %>% filter(subclass != "") %>% select(base_class, subclass)
  return(list(progression = progression, subclasses = subclasses))
}

read_subclasses <- function(df){
  require(hash)
  keys = df %>% pull(base_class)
  values = df %>% pull(subclass)
  result <- hash(
    keys   = df %>% pull(base_class),
    values = df %>% pull(subclass)
  )
  return(result)
}

read_stats <- function(filename){
  require(hash)
  stat_list <- read_delim(
    filename,
    col_names = FALSE,
    delim = ":",
    col_types = "cn",
    show_col_types = FALSE,
    trim_ws = TRUE)
  colnames(stat_list) <- c("stat", "score")
  stat_list <- hash(
    keys = tolower(stat_list$stat),
    values = stat_list$score
  )
  required_scores <- c("str", "dex", "con", "int", "wis", "cha")
  provided_scores <- keys(stat_list)
  missing_scores <- setdiff(required_scores, provided_scores)
  unknown_scores <- setdiff(provided_scores, required_scores)
  for(ability in unknown_scores) warning("Unknown ability score ", ability)
  for(ability in missing_scores) warning("Missing expected ability score ", ability)
  result <- as(stat_list, "StatMap")
  return(result)
}

read_features <- function(filename){
  require(hash)
  if(!is_empty(filename)) {
    features <- read_delim(
      filename,
      delim          = ":",
      col_names      = FALSE,
      show_col_types = FALSE,
      trim_ws        = TRUE)
    colnames(features) <- c("Feature", "Items")
    keys   <- features$Feature
    values <- str_split(features$Items, pattern = ", ")
    result <- as(hash(keys = keys, values = values), "FeatureMap")
  } else {
    result <- new_feature_list()
    }
  return(result)
}

read_policies <- function(policyfile){
  result <- new_policies()
  if(!is_empty(policyfile)){
    policy_frame <- read_delim(
      policyfile,
      col_names = FALSE,
      delim = ":",
      show_col_types = FALSE,
      trim_ws = TRUE)
    stopifnot(ncol(policy_frame) == 2)
    for(r in 1:nrow(policy_frame)){
      result[[policy_frame[[r,1]]]] = as.character(policy_frame[[r,2]])
    }
  }
  return(result)
}

setOldClass(c("CharacterBuild", "R6"))

setMethod("show", "StatMap",
  function(object){
  required_scores <- c("str", "dex", "con", "int", "wis", "cha")
  for(ability in required_scores){
    cat(toupper(ability), ":", object[[ability]], "  ")
  }
  cat("\n")
  })

setMethod("show", "FeatureMap",
  function(object){
    features <- keys(object)
    for(f in features)
    {
      n_items <- length(object[[f]])
      cat(f,": ")
      for(i in 1:n_items){
        cat(object[[f]][[i]])
        if(i < n_items) cat(", ")
      }
      cat("\n")
    }
    cat("\n")
    })

setMethod("show", signature = "PolicyMap", def = function(object){
  for(policy in keys(object))
  cat(policy, ":", object[[policy]], "\n")
})

setGeneric("name", def = function(object){standardGeneric("name")})
setMethod("name", signature = "CharacterBuild",
  definition = function(object){return(object$name)})

setGeneric("progression", def = function(object){standardGeneric("progression")})
setMethod("progression", signature = "CharacterBuild",
  definition = function(object){return(object$progression)})

setGeneric("subclass", def = function(object,classname){standardGeneric("subclass")})
setMethod("subclass", signature = c("CharacterBuild", "character"),
  definition = function(object, classname){
    return(object$subclass(classname))})

setGeneric("features", def = function(object,featuretype){standardGeneric("features")})
setMethod("features", signature = c("CharacterBuild", "character"),
  definition = function(object, featuretype){
    return(object$feature_list(featuretype))
  })

setGeneric("policy", def = function(object,policytype){standardGeneric("policy")})
setMethod("policy", signature = c("CharacterBuild","character"), def =
  function(object, policytype){
    return(object$policy(policytype))
})

setGeneric("classes", def = function(object){standardGeneric("classes")})
setMethod("classes", signature = "CharacterBuild",
  definition = function(object){
    return(object$classlist)})

setGeneric("subclasses", def = function(object){standardGeneric("subclasses")})
setMethod("subclasses", signature = "CharacterBuild",
  definition = function(object){
    return(object$subclasslist)})

setGeneric("feat_at_1st", def = function(object){standardGeneric("feat_at_1st")})
setMethod("feat_at_1st", signature = "CharacterBuild",
  definition = function(object){return(object$feat_at_1st)})
