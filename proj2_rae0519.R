

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



Pall <-  function(n, strategy, nreps = 10000){
  Pall_prob = 0
  for (i in 1:nreps){
    for (k in 1:(2*n)){
      success = 0
      if (Pone(n, k, strategy, 1) > 0){
        success = 1
      }  
    }
    if (success == 1){
      Pall_prob = Pall_prob + 1
    }
  }
  Pall_prob/nreps
}


Pall(5,3)

Pall(50,1)


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








