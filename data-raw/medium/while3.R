params <- 7:9
m <- n;
while(n > 1) {
  k <- m - n 
  while(k > 1) {
    k <- k %/% 2
    cat(k, '')
  }
    n <- n - 1;
}

