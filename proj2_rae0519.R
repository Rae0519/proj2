

Pone <- function(n,k,strategy,nreps = 10000){
  success = 0
  for (i in 1:nreps){
    if (strategy == 1){
      box <-  sample(1:(2*n),2*n)
      card = box[k]
      for(time in 1:(n-1)){
        card = box[card]
        if ( card == k){
          success = success + 1
          break
        }
      }
    }
    else if (strategy == 2){
      box <-  sample(1:(2*n),2*n)
      card = sample(1:(2*n),1)
      for(time in 1:(n-1)){
        card = box[card]
        if ( card == k){
          success = success + 1
          break
        }
      }
    }
    else if (strategy == 3){
      random_n <-  sample(1:(2*n),n)
      if (length(which(random_n == k )) > 0){
        success = success + 1
      }
    }
  }
  success/nreps
}




Pone <- function(n,k,strategy,nreps = 10000){
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

p = Pone(50,34,1,1)


Pone <- function(n,k,strategy,nreps = 10000){
  A <- 1: (2*n)
  escape = 0
  if (strategy == 1){
    j = 1
    for (j in 1 : nreps){
      prisoner <- sample(A, 2*n, replace = FALSE)
      cardinbox <- sample(A, 2*n, replace = FALSE)
      i = 1
      numforpk <- prisoner[k]
      m <- cardinbox[numforpk]
      for ( i in 1 : n ){
        if (m == prisoner[k]){
          escape = escape + 1 
          break
        }
        else { if ( i < n ){
          m = cardinbox[m]
          i = i + 1
        }
          else{
            escape = escape
            break
          }
        } 
      }
      j = j + 1
    }
  }
  if (strategy == 2){
    j = 1
    for (j in 1 : nreps){
      prisoner <- sample(A, 2*n, replace = FALSE)
      cardinbox <- sample(A, 2*n, replace = FALSE)
      i = 1
      k_1 <- sample(A, 1, replace = FALSE)
      m <- cardinbox[k_1]
      for ( i in 1:n ){
        if (m == prisoner[k]){
          escape = escape + 1 
          break
        }
        else { if ( i < n){
          m = cardinbox[m]
          i = i + 1
        }
          else{
            escape = escape
            break
          }
        } 
      }
      j = j + 1
    }
    
  }
  if (strategy == 3){
    j = 0
    for (j in 1 : nreps){
      prisoner <- sample(A, 2*n, replace = FALSE)
      cardinbox <- sample(A, 2*n, replace = FALSE)
      box_open <- sample(A, n, replace = FALSE)
      if (prisoner[k] %in% cardinbox[box_open]){
        escape = escape + 1
      }
      else {escape = escape}
    }
    j = j + 1
  }
  escape / nreps
}

Pall <-  function(n, strategy, nreps = 10000){
  Pall_prob = 0
  for (i in 1:nreps){
    success = rep(0, 2*n)
    for (k in 1:(2*n)){
      if (Pone(n, k, strategy, 1) > 0){
        success[k] = 1
      }
    }
    if (sum(success) == 2*n){
      Pall_prob = Pall_prob + 1
    }
  }
  Pall_prob/nreps
}







Pall(5,3)

Pall(100,1)
Pall(100, 3)



dloop <- function(n, nreps = 10000){
  d_prob <- rep(0, 2*n)
  for (m in 1:nreps){
    u <- sample(1:(2*n), 2*n)
    k <-  sample(1:(2*n), 2*n)
    for (i in 1:(2*n)){
      card = u[k[i]]
      for(length_loop in 2:(2*n)){
        if (card == k[i]){
          d_prob[length_loop-1] <- d_prob[length_loop-1] + 1
          break
        }
        card = u[card]
      } 
    }
  }
  d_prob/sum(d_prob)
}


dloop(50)


#  k 如何决定？是随机一个还是要遍历？




dloop_probability <- dloop(50)

# for probability that there is no loop longer than 50.
# it means the addition of the front 50 probability

dloop_probability <- dloop(50)

sum(dloop_probability[1:50])







# n = 50
# d_prob <- rep(0, 2*n)
#   u <- sample(1:(2*n), 2*n)
#   for (k in 1:(2*n)){
#     card = u[k]
#     for(length_loop in 2:(2*n)){
#       if (card == k){
#         d_prob[length_loop-1] <- d_prob[length_loop-1] + 1
#         break
#       }
#       card = u[card]
#     }
#   }
# 
# d_prob








