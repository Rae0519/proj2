
success <- function(n,k, p, strategy) {
  prisoner = sample(1:(2*n))
  if ( p == 1){
    prisoner[1] = k
  }
  count = 0
  escape = rep(0, p)
  card = sample(1:(2 * n))
  if (strategy == 1) {
    for (i in 1:p) {
      boxes = 0
      box_start = prisoner[i]
      while (boxes < n && !escape[i]) {
        boxes = boxes + 1
        escape[i] = card[box_start] == prisoner[i]
        box_start = card[box_start]
      }
    }
  }
  
  else if (strategy == 2) {
    for (i in 1:p) {
      boxes = 0
      box_start = sample(1:(2*n),1)
      while (boxes < n && !escape[i]) {
        boxes = boxes + 1
        escape[i] = card[box_start] == k_1[i]
        box_start = card[box_start]
      }
      
    }
  }
  
  else if (strategy == 3) {
    for (i in 1:p) {
      escape[i] = i %in% sample(card, n)
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

# Pall(50,1)
