n = 1000000
mean = 0

for(i in rnorm(n)){
  if(i > -1 & i<1){
    mean = mean + 1
  }
}
mean/n