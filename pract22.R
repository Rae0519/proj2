"
Group member1: Rui Zeng      ID:s2298181
Group member2: Lifu Zheng    ID:s2314868
Group member3: Sihan Liu     ID:s2337553

Contribution:
Task 1, 2, 3 and 4 were completed by Member 3 and Member 2.
Task 5, 6 were completed by Member 1.
We both add comments and integrate the code.
Every group member has made contribution to the final code. We all completed the practical work in our own way.
The proportion of contribution is roughly equal in our group.

Overview:


"
## 'success' function is to 

success <- function(n, k, p, strategy) {
  prisoner = sample(1:(2 * n))
  if (p == 1) {
    prisoner[1] = k
  }
  count = 0
  found = rep(0, p)
  card = sample(1:(2 * n))
  if (strategy == 1) {
    for (i in 1:p) {
      boxes = 0
      box_start = prisoner[i]
      while (boxes < n) {
        boxes = boxes + 1
        found[i] = card[box_start] == prisoner[i]
        if (found[i] == 1) {
          break
        }
        box_start = card[box_start]
      }
    }
  }
  
  else if (strategy == 2) {
    for (i in 1:p) {
      boxes = 0
      box_start = sample(card, 1)
      while (boxes < n) {
        boxes = boxes + 1
        found[i] = card[box_start] == prisoner[i]
        if (found[i] == 1) {
          break
        }
        box_start = card[box_start]
      }
      
    }
  }
  
  else if (strategy == 3) {
    for (i in 1:p) {
      found[i] = i %in% sample(card, n)
    }
    
  }
  count <- (sum(found) == p)
}


Pone <- function(n, k, strategy, nreps = 10000) {
  success_times = 0
  for (i in 1:nreps) {
    success_times = success_times + success(n, k, 1, strategy)
  }
  success_times / nreps
}

Pall <- function(n, strategy, nreps = 10000) {
  all_success = 0
  for (i in 1:nreps) {
    all_success = all_success + success(n, k, 2 * n, strategy)
  }
  all_success / nreps
}


# Pall(5,3)

# Pall(50,1)
