# A random choice structure P is represented as a 2^n by n matrix of probabilities
# Element (A,x) of P gives the probability of choosing x from set A
# The binary representation of A gives inclusion indicators, so that, for example,
#   A = 6 (decimal) = 110 (binary) indicates that the two elements of A are 2 and 3
# If A = {x}, P[A, x] = 1 and this needs to be set in the input
# If x is not an element of A, this entry is ignored

# A random choice structure P is represented as a 2^n by n matrix of probabilities
# Element (A,x) of P gives the probability of choosing x from set A
# The binary representation of A gives inclusion indicators, so that, for example,
#   A = 6 (decimal) = 110 (binary) indicates that the two elements of A are 2 and 3
# If A = {x}, P[A, x] = 1 and this needs to be set in the input
# If x is not an element of A, this entry is ignored

# Function to test the regularity of a random choice structure.
# Input is P, the random choice structure
# Output is TRUE or FALSE, accoring to whether P satisfies regularity
regularity = function(P,binary=FALSE) {
  if(binary)P=P[1:7,1:3]
  n = ncol(P)    # Number of objects in master set
  n.sets = 2^n-1 # Number of subsets of {1,...,n}, excluding empty set
  for (A in 1:n.sets) { # Run through all non-empty subsets A of {1,...,n}
    for (a in 1:n) {  # Run through all elements of {1,...,n}
      Sa = bitwShiftL(1, a-1)   # Sa is {a}, the singleton set containing a
      if (bitwAnd(Sa, A) > 0) { # Make sure a is in A
        for (b in 1:n) { # Run through all elements of {1,...,n}
          Sb = 2^(b-1) # Sb is {b}
          if (bitwAnd(Sb, A) > 0 && (a!=b)) { # Make sure b is in A, not = to a
            if (P[A,a] > P[A-Sb,a]) return(FALSE)
          }
        }
      }
    }
  }
  return(TRUE)
}
