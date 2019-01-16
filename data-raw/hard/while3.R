params <- 10:15
m <- (2*n) %/% 3
while(n > 0 || m > 0) {
  if(n < m) {
    m <- m %/% 2
    cat(m, '') 
  } else {
    cat(n, '') 
    n <- n %/% 2;
  }
}

