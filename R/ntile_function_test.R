quintile_ties <- function(x, n = 5, tie_rule = min) {
  g <- dplyr::ntile(x, n)
  ok <- !is.na(x)
  g[ok] <- ave(g[ok], x[ok], FUN = tie_rule)   # tied values share one group
  g
}

quintile_ties(c(1, 2, 2, 3, 4, 5, 6, 7, 8, 9))

      # 1 1 1 2 3 3 4 4 5 5

ntile_5  <- function(x){
  dplyr:: ntile(x,5)
 }


ntile_5(c(1, 2, 2, 3, 4, 5, 6, 7, 8, 9))
    # 1 1 2 2 3 3 4 4 5 5