params <- 17:24
while(n > 0) {
  m <- n 
  while(m > 1) {
    m <- m %/% 3
    cat(m, '')
  }
  n <- n %/% 2;
}

