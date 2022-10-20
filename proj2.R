
"
Group member1: Rui Zeng      ID:s2298181
Group member2: Lifu Zheng    ID:s2314868
Group member3: Sihan Liu     ID:s2337553

Github Repo: https://github.com/Rae0519/proj2.git

Contribution:
Task 1, 2, 3 and 4 were completed by Member 3 and Member 2.
Task 5, 6 were completed by Member 1.
We both add comments and integrate the code.
Every group member has made contribution to the final code. We all completed the practical work in our own way.
The proportion of contribution is roughly equal in our group.
"


"
Overview:
The project focuses on the simulation of prisoner problem. The prisoners will have opportunities to be free.
Let assume there are 2n prisoners with their unique numbers. Each prisoner will give a chance to choose n boxes from
2n boxes. Each boxes has a random and unique number card from 1 to 2n. If all 2n prisoners can find their number cards 
successfully by opening a maximum on n boxes, they all go free. It seems that this task is impossible but under a strategy,
prisoners will have a reasonably good chance to have freedom. Let us see how that works and the reason of that. 
"


"
There are three possible strategies to be attempted:

1) The prisoner starts at the box with their number k on it. If card in that box is not their prisoner number, 
   they go to box with that number on previous card, open it and repeat the process until they have either found the card 
   with their number on it, or opened n boxes without finding it.
   
2) As strategy 1, but starting from a randomly selected box.

3) They open n boxes at random, checking each card for their number.
"


"
Fisrtly we need to find the probability of each prisoner to find their card and the probability of all prisoners getting 
free under each strategy.
"


" 
success() is to decide whether the prisoner or prisoners can go free, using loop to traverse 'p' prisoners according to three strategies.

Inputs are n (the half number of prisoner), k (the prisoner’s number), p (the number of prisoners who enter the room), strategy (1, 2 or 3)
The output will the logical object(0/1): whether the prisoner or prisoners go free.

General step:
To determine whether all prisoners find their numbers (card == prisoner[i]) under each strategy, we create a vector 'found' to record (0/1)
whether each prisoner succeeds. If the summation of vector 'found' is equal to the number of prisoners (sum(found) == p), it means prisoners 
who enter the room all find their cards.
"

success <- function(n, k, p, strategy) {  
  prisoner = sample(1:(2 * n))                # random sample of 2n prisoner numbers with no repeat, create the sequence to enter the room
  if (p == 1) {
    prisoner[1] = k                           # if only one prisoner enter, then the first and unique prisoner number is 'k'
  }
  found = rep(0, p)                           # create a p-vector with value zero.
  box = sample(1:(2 * n))                     # random sample of 2n boxes number with no repeat, then cards are randomly placed one in each box.
  
  # strategy 1
  if (strategy == 1) {
    for (i in 1:p) {                          # the loop will stop until 'p' prisoners found they cards
      card = box[prisoner[i]]                 # find the card number in the box of 'prisoner[i]'
      for (time in 1:n) {                     # open at most n boxes
        if (card == prisoner[i]) {            # card number is equal to 'prisoner[i]'
          found[i] = 1                        # successfully find the card
          break                               # end 'time' loop
        }
        card = box[card]                      # otherwise continue to find the card in box of last card number
      }
    }
  }
  
  # strategy 2 
  else if (strategy == 2) {                   # the structure is similar to strategy 1
    for (i in 1:p) {
      card = sample(1:(2 * n), 1)             # decide the first open box randomly, so the card number is random too
      for (time in 1:n) {
        if (card == prisoner[i]) {            
          found[i] = 1                        # Notice: for strategy 2, the loop (e.g the card in box 4 is 5 and the card in box 5 is 4) 
          break                               #         may happen. Under this situation, the prisoner will constantly open box 4 and 5 
        }                                     #         until time n to open boxes. It means he doesn't find his number and is failed.
        card = box[card]
      }
    }
  }
  
  # strategy 3
  else if (strategy == 3) {
    for (i in 1:p) {                                        # random open n boxes is equal to randomly select n numbers in [1, 2n]    
      found[i] = prisoner[i] %in% sample(1:(2*n), n)        # %in% is to check whether the ith prisoner number is in those n selected numbers.
    }                                                       # if 'prisoner[i]' is in those numbers, it means the ith prisoner finds his card.
  }                                                           
 
  sum(found) == p  # if all 'p' prisoners find their cards then the summation of vector 'found' is equal to 'p'
}                  # if it's True return 1, and False return 0



" 
Pone() is to estimate the probability of a single prisoner succeeding in finding his number under each strategy.
It takes n (the half number of prisoner), k (the prisoner’s number), strategy (1, 2 or 3), 
nreps(the number of replicate simulations, default=10000) as arguments.
The output will be the probability after simulation (success_times / nrep).
"


Pone <- function(n, k, strategy, nreps = 10000) {
  success_times = 0                                                  # 'success_times' defines the number of success.
  for (i in 1:nreps) {                                               # simulate 'nreps' times
    success_times = success_times + success(n, k, 1, strategy)       # accumulate the times of success during simulation
  }                                                                  # apply 'success' function to determine whether the prisoner can find his card in one simulation
  success_times / nreps                                              # calculate the probability of success
}


" 
Pall() is to estimate the probabilities of all prisoners finding their number under each strategy
It takes n (the half number of prisoner), strategy (1, 2 or 3), nreps (the number of replicate simulations, default=10000) as arguments.
The output will be the probability after simulation (success_times / nrep).
"

Pall <- function(n, strategy, nreps = 10000) {
  all_success = 0                                                    # 'all_success' defines the times that all prisoners succeed
  for (i in 1:nreps) {
    all_success = all_success + success(n, 1, 2 * n, strategy)       # ccumulate the times of success during simulation
  }                                                                  # apply 'success' function to determine whether 2n prisoners can all find their cards in one simulation
  all_success / nreps                                                # calculate the probability of success
}


# example code

# individual probability (n = 5, k = 3, nreps = 10000)
Pone(5, 3, 1)                # one possible result: 0.4991
Pone(5, 3, 2)                # one possible result: 0.4044
Pone(5, 3, 3)                # one possible result: 0.5033

# individual probability (n = 50, k = 63, nreps = 10000)
Pone(50, 63, 1)              # one possible result: 0.5078          
Pone(50, 63, 2)              # one possible result:   
Pone(50, 63, 3)              # one possible result:


# joint probability (n = 5, nreps = 10000)
Pall(5,1)                    # one possible result:0.3487
Pall(5,2)                    # one possible result:1e-04
Pall(5,3)                    # one possible result:7e-04

# joint probability (n = 50, nreps = 10000)
Pall(50, 1)                  # one possible result:0.3119
Pall(50, 2)                  # one possible result:0
Pall(50, 3)                  # one possible result:0



"
Comment:
Through 'Pall' function, we can find that the prisoners through using strategy 1 can escape together with a probability of approximately 0.31. 
This is a surprising result because from the function 'Pone', we can see that the probability of each individual escaping is about 0.5. 
If we consider them to be independent of each other, the probability that they escape together is 0.5^(100), which is nearly 0. 

Reflecting on the problem itself, we can find that the prisoners are independent of each other only under the following circumstance: 
when each prisoner leaves and we immediately determine the position of the box and card randomly. If the room is returned exactly to its original state 
after each prisoner leaves, they all operate in the same sequence with boxes and cards. Consequently, they are not independent of each other. 

Then, we will consider under what circumstance can the prisoners escape together. Through mathematical argument, we can have the following supposition.
For 2n prisoners, if there is no loop greater than n boxes and cards, all prisoners can escape together.

Next, we try to run the code to prove our supposition.
"



"
In order to find the reason why such a high probability under join success probabilities happen,
we try to find the loops( when u[u[· · · u[u[k]] · · · ]] = k) when randomly shuffling cards.

'dloop' function is to estimate the probability of each loop length from 1 to 2n 
occurring at least once in a random shuffling of cards to boxes.

The function takes 'n' (the half number of card) and 'nrep' (the number of replicate simulations, default value is 10000) as input.
Then output the 2n-vector of probabilities.

General Step:
To compute the probability for each loop length, we need to count the existence of loops in one shuffling.
Open the boxes (2n times) similar to strategy 1 for each prisoner k (2n prisoners in total). 
Count 1 for that loop when the prisoner finds his card. 
(Notice: only count 1 even if two prisoners find their numbers through the same loop length under one random shuffling.)
Then, simulating the shulffing card for 'nreps' times.
Finally, calculate the probability for 2n loops.(count numbers of each loop/nreps)
"



dloop <- function(n, nreps = 10000){
  d_prob <- rep(0, 2*n)                     # 'd_prob' is a 2n-vector with value 0. 
                                            # It is to count the occurrence of each loop.
  
  for (m in 1:nreps){                       # loop through 'nreps' times of shuffling
    u <- sample(1:(2*n))                    # randomly choose 2n cards in [1, 2n] without repeating.
    k <- sample(1:(2*n))                    # randomly choose 2n prisoners in [1, 2n] without repeating.
    success <- rep(0, 2*n)                  # 'success' is a 2n-vector with value 0 for counting the occurrence of each loop in one random shuffling.
    
    for (i in 1:(2*n)){                     # loop through 2n prisoner in one shuffling 
      card = u[k[i]]                        # choose the card in the box of the ith prisoner number k[i].
      
      for(length_loop in 1:(2*n)){          # loop through each 'loop' (u[u[·u[u[k]]·]] = k)
        
        if (card == k[i]){                  # Run if the 'loop' occurrs: the prisoner number is found.
          success[length_loop] = 1          # count 1 for the occurrence of that 'loop' at least once in one random shuffling.
          break                             # end 'length_loop' loop 
        }
        card = u[card]                      # open the next box
      }                              
    }                            
    d_prob <- d_prob + success              # accumulate the existence of 'loop' in all shufflings.
  }                                  
  d_prob/nreps                              # calculate the probabilities of 'loop' 
}                                           # it is also the return value of the function.


# example code for 'dloop' function when n = 50 and nreps = 10000 (default).

dloop_probability <- dloop(50)


# We find the probability that there is no loop longer than 50 ([51:100]) in a random reshuffling of cards to boxes.

1 - sum( dloop_probability[51:100] )

# one possible result: 0.3111. 
# It is similar to the result of joint success probabilities of strategy 1.
# It makes sense. When all prisoners all go free under strategy 1, it means no prisoner opens more than n boxes (here n = 50).
# It is the same question as the probability that there is no loop > 50.


# Let us to see the graph in bar chart.
cols <-  rep(c("black", "red"), each = 50)
plot(dloop_probability,type = "h", xlab ="Loop Length",ylab="Probability", main = "probabilities of loop length when n = 50", col = cols)
legend("topright", legend = c("loop length <= 50", "loop length > 50"), fill = c("black", "red"))
# The trend of the probabilities seems like the function of 1/x. The probabilities decrease with the growth of loop length.



