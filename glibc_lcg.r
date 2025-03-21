LCG <- function(n, seed=0, a=1103515245, c=12345, m=0x7fffffff) {
  x <- numeric(n)
  x[1] <- seed
  for (i in 2:n) {
    x[i] <- (a * x[i-1] + c) & m
    print(x[i])
  }
}
LCG(2)

print()