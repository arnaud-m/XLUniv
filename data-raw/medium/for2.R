params <- 5:8
for(i in 1:n) {
  for(j in 1:n) {
    if( i < j && (i + j) %% 3 == 0) {
      cat(j, '')
    }
  }
}
