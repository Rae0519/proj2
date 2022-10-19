
success <- function(n, k, p, strategy) {
  success = 0
  escape = rep(0, p)
  card = sample(1:(2 * n))
  if (strategy == 1) {
    for (i in 1:p) {
      boxes = 0
      box_start = k
      while (boxes < n && !escape[i]) {
        boxes = boxes + 1
        escape[i] = card[box_start] == k
        box_start = card[box_start]
      }
    }
  }
  
  else if (strategy == 2) {
    for (i in 1:p) {
      boxes = 0
      box_start = sample(card,1)
      while (boxes < n && !escape[i]) {
        boxes = boxes + 1
        escape[i] = card[box_start] == k
        box_start = card[box_start]
      }
      
    }
  }
  
  else if (strategy == 3) {
    for (i in 1:p) {
      escape[i] = i %in% sample(card, n)
    }
    
  }
  success = sum(escape) == p
  success
}


Pone <- function(n,k,strategy,nreps = 10000){
  success = 0
  for (i in 1:nreps){
    success = success + success(n,k,1,strategy)
  }
  success/nreps
}

Pall <- function(n,strategy,nreps = 10000){
  success = 0
  for (i in 1:nreps){
    k <- sample(1:(2*n),1)
    success = success + success(n,k,2*n,strategy)
  }
  success/nreps
}


# Pall(5,3)

# Pall(50,1)








