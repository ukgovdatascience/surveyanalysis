get_mode <- function(x) {
  # reference: https://stackoverflow.com/a/55758630/13416265
  # ignores NAs
  ux <- unique(x)
  mode <- ux[which.max(tabulate(match(x, ux)))]
  return(mode)
}
