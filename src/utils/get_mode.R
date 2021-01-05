get_mode <- function(x) {
  # reference: https://stackoverflow.com/a/55758630/13416265
  ux <- unique(x)
  mode <- ux[which.max(tabulate(match(x, ux)))]
  return(mode)
}
