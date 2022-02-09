parse_dice_expression <- function(dice_expression)
{
  result = list(
    n_dice = gsub("([0-9]+)d([0-9]+) ?\\+? ?([0-9]+)?", "\\1", dice_expression),
    d_type = gsub("([0-9]+)d([0-9]+) ?\\+? ?([0-9]+)?", "\\2", dice_expression),
    mod    = gsub("([0-9]+)d([0-9]+) ?\\+? ?([0-9]+)?", "\\3", dice_expression)
  )
  if(result$mod == "") result$mod = 0
  result <- lapply(result, as.numeric)
  return(result)
}

dice_avg <- function(dice_object)
{
  return(with(dice_object, n_dice * (d_type + 1) / 2))
}

static_dmg <- function(dice_object)
{
  return(dice_object$mod)
}

total_dmg <- function(dice_object)
{
  return(dice_avg(dice_object) + static_dmg(dice_object))
}
