


"
In order to find the reason why such a high probability under join success probabilities happen,
we try to find the loops( when u[u[· · · u[u[k]] · · · ]] = k) when randomly shuffling cards.

'dloop' function is to estimate the probability of each loop length from 1 to 2n 
occurring at least once in a random shuffling of cards to boxes.

The function takes 'n' (the half number of card) and 'nrep' (the number of replicate simulations, default value is 10000) as input.
Then output the 2n-vector of probabilities.

Detailed Step:
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

1-sum(dloop_probability[51:100])

# one possible result: 0.3111. 
# It is similar to the result of joint success probabilities of strategy 1.
# It makes sense. When all prisoners all goes free under strategy 1, it means no prisoner opens more than n boxes (here n = 50).
# It is the same question as the probability that there is no loop > 50.


# Let us to see the graph in bar chart.

barplot(dloop_probability, xlab="Loop Length",ylab="Probability")

# The trend of the probabilities seems like the function of 1/x. The probabilities decrease with the growth of loop length.



