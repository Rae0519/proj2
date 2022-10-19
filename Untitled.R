
success <- function(n,k, p, strategy) {
  prisoner = sample(1:(2*n))
  if ( p == 1){
    prisoner[1] = k
  }
  count = 0
  escape = rep(0, p)
  box = sample(1:(2 * n))
  if (strategy == 1) {
    for (i in 1:p) {
      card = box[prisoner[i]]
      for (time in 1:n){
        if (card == prisoner[i]){
          escape[i] = 1
        break
        }
        card = box[card]
      }
    }
  }
  
  else if (strategy == 2) {
    for (i in 1:p) {
      card = sample(1:(2*n), 1)
      for (time in 1:n){
        if (card == prisoner[i]){
          escape[i] = 1
        break
        }
        card = box[card]
      }
      
    }
  }
  
  else if (strategy == 3) {
    for (i in 1:p) {
      escape[i] = i %in% sample(box, n)
    }
    
  }
  if (sum(escape) == p){
    count + 1
  }
  else {
    count
  }
}


Pone <- function(n,k,strategy,nreps = 10000){
  successtimes = 0
  for (i in 1:nreps){
    successtimes = successtimes + success(n,k,1,strategy)
  }
  successtimes/nreps
}

Pall <- function(n,strategy,nreps = 10000){
  allsuccess = 0
  for (i in 1:nreps){
    allsuccess = allsuccess + success(n,1,2*n,strategy)
  }
  allsuccess/nreps
}


# Pall(5,3)

Pall(50,3)
Pone(50, 21, 3)
