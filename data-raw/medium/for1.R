params <- 5:8
for(i in 1:n) {
  for(j in 1:n) {
    if( (i + j) %% 5 == 0) {
      cat(j, '')
    }
  }
}
