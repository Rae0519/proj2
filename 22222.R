
Pone <- function(n,k,strategy,nreps){
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
    escape_rate = escape / nreps
    cat(escape_rate)
}
