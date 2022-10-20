

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

" success() is to decide whether the prisoners can go free, using loop to traverse 'p' prisoners according to 3 strategies.
  In this function we can define how many prisoners can succeed finding their cards.
  The success can be determined by whether the card number = prisoner's number. And we realise the process of findng card by 'for' loop less than 'n' times.
  n, k(the prisoner’s number), p(the number of prisoners who enter the room), strategy (1, 2 or 3)
  the output will be 0 or 1."

success <- function(n, k, p, strategy) {  
  prisoner = sample(1:(2 * n))  # random sample of prisoner numbers with no repeat, create the sequence to enter the room
  if (p == 1) {
    prisoner[1] = k  # if only 1 prisoner enter, then the first prisoner number is 'k'
  }
  found = rep(0, p)  # create a 0 vector in length 'p'
  box = sample(1:(2 * n))  # random sample of boxes number with no repeat, then cards are randomly placed one in each box.
  
  # strategy 1
  if (strategy == 1) {
    for (i in 1:p) {   # the loop will stop until 'p' prisoners found they cards
      card = box[prisoner[i]]  # find the card number in the box of prisoner[i]
      for (time in 1:n) {      # open at most n boxes
        if (card == prisoner[i]) {  # card number is equal to prisoner[i]'s number
          found[i] = 1     # successfully find the card
          break
        }
        card = box[card]   # otherwise continue to find the card in box of last card number
      }
    }
  }
  
  # strategy 2 
  else if (strategy == 2) {  # the structure is similar
    for (i in 1:p) {
      card = sample(1:(2 * n), 1)  # decide the first open box randomly, so the card number is random too
      for (time in 1:n) {
        if (card == prisoner[i]) {
          found[i] = 1
          break
        }
        card = box[card]
      }
    }
  }
  
  # strategy 3
  else if (strategy == 3) {
    for (i in 1:p) {                    
      found[i] = i %in% sample(box, n)  # find out whether card number 'i' is in randomly picked n boxes for prisoner 'i'
    }
    
  }
  sum(found) == p  # if all 'p' prisoners found the cards then the summation of vector 'found' is equal to 'p'
}                  # if it's True return 1, and False return 0



" Pone() is to estimate the probabilities of a single prisoner succeeding in finding their number under each strategy
  n, k(the prisoner’s number), strategy (1, 2 or 3), nreps(the number of replicate simulations)=10000
  the output will be the probability estimate"

Pone <- function(n, k, strategy, nreps = 10000) {
  success_times = 0   # define the times of success
  for (i in 1:nreps) {   # simulate 10000 times
    success_times = success_times + success(n, k, 1, strategy)  # apply success() when p = 1
  }
  success_times / nreps  # calculate the probability of success
}


" Pall() is to estimate the probabilities of all prisoners finding their number under each strategy
  n, strategy (1, 2 or 3), nreps(the number of replicate simulations)=10000
  the output will be the probability estimate"

Pall <- function(n, strategy, nreps = 10000) {
  all_success = 0   # define the times that all prisoners succeed
  for (i in 1:nreps) {
    all_success = all_success + success(n, 1, 2 * n, strategy)  # apply success() when p = 2*n
  }
  all_success / nreps  # calculate the probability of success
}

Pone(5, 63, 1)
Pone(5, 63, 2)
Pone(5, 63, 3)
Pone(50, 63, 1)
Pone(50, 63, 2)
Pone(50, 63, 3)
Pall(5,1)
Pall(5,2)
Pall(5,3)
Pall(50, 1)
Pall(50, 2)
Pall(50, 3)







