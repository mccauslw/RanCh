# Function to test whether or not a random choice structure satisfies random utility
# Inputs are P, the random choice structure and compute_Q, a boolean value indicating
# whether or not to compute Q, the matrix of Block-Marschak polynomials corresponding to P
# If compute_Q is true, output is a list with elements Q and is.ru, where is.ru is a
# boolean value that is TRUE or FALSE according to whether or not P satisfies random utility
# If compute_Q is FALSE, output is TRUE or FALSE according to whether or not P satisfies
# random utility.
ru = function(P, compute_Q, binary=FALSE) {
  if(binary)P=P[1:7,1:3]
  n = ncol(P)
  n.sets = bitwShiftL(1, n)-1 # Number of subsets of {1,...,n}, excluding empty set
  if (compute_Q) {
    is.ru = TRUE
    Q = P*0 # Inherit dimension, row and column names from P, set to zero
  }

  # Precompute the cardinality of all subsets of {1,...,n}
  card = vector('integer', n.sets)
  for (A in 1:n.sets) {
    for (S in bitwShiftL(1, 0:(n-1))) {
      if (bitwAnd(S, A) > 0) {
        card[A] = card[A]+1
      }
    }
  }

  for (T in 1:n.sets) { # Run through all non-empty subsets T of {1,...,n}
    for (s in 1:n) {       # Run through all element of {1,...,n}
      Ss = bitwShiftL(1, s-1)   # Ss is {s}, the singleton set containing s
      if (bitwAnd(Ss, T) > 0) { # Make sure s is in T
        BM = 0.0       # Initialize Block-Marshak polynomial for (s, T) to 0
        for (U in T:n.sets) { # Run through supersets U of T
          if (bitwAnd(U, T) == T) { # Make sure U is indeed a superset
            if (U==T || (card[U-T] %% 2 == 0)) {
              BM = BM + P[U, s] # Cardinality of U\T is even
            }
            else {
              BM = BM - P[U, s] # Cardinality of U\T is odd
            }
          }
        }
        if (compute_Q) {
          Q[T, s] = BM
          if (BM < 0.0) is.ru=FALSE
        }
        else {
          if (BM < 0.0) return(FALSE) # Violation of random utility found
        }
      }
    }
  }
  if (compute_Q) return(list(Q=Q, is.ru=is.ru)) else return(TRUE)
}
