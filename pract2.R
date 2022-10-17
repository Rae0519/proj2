Pone <- function(n,k,strategy,nreps){
  box <- 1:(2*n)
  count <- 10000
  # strategy 1
  if (strategy == 1){
    for (i in 1:nreps) {
      box <- sample(box,2*n)
      num <- k
      t <- n
      while (t > 0){
        num <- box[num]
        if (num == k){break}
        t = t - 1
      }
      if (t == 0){
        count = count - 1
      }
    }
  }
  
  # strategy 2
  if (strategy == 2){
    for (i in 1:nreps) {
      box <- sample(box,2*n)
      num <- sample(box,1)
      t <- n
      while (t > 0){
        num = box[num]
        if (num == k){break}
        t = t - 1
      }
      if (t == 0){
        count = count - 1
      }
    }
  }
  # strategy 3
  if (strategy == 3){
    for (i in 1:nreps) {
      box <- sample(box,n)
      if (k %in% box[] == T){
        break
      }
      else {
        count = count -1
      }
      }
  }
  
  prob <- count/nreps
  return(prob)
  }
  
Pall <- function(n,strategy,nreps){
  prob <- 0
  for (i in 1:(2*n)) {
    k <- sample(1:(2*n),1)
    prob = prob + Pone(n,k,strategy,nreps)
  }
  prob <- prob/(2*n)
  return(prob)
}
  

nrep <- 10000
k <- 4
n <- 50
Pone(50,1,1,10000)
Pall(50,1,10000)
