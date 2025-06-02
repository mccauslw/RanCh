n_objects = 8
object_names = c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h')
n_subsets = 2^n_objects - 1

# Compute names and cardinality of all subsets of {1,2,...,n_objects}
subset_names = vector(mode='character', length=n_subsets)
subset_card = vector(mode='integer', length=n_subsets)
subset_vectors = vector(mode='list', length=n_subsets)
for (subset in seq(1, n_subsets)) {
  name = ''
  card = 0
  v = c()
  for (i in seq(1, n_objects)) {
    if (bitwAnd(subset, bitwShiftL(1, i-1))) {
      name = paste(name, object_names[i], sep='')
      card = card+1
      v = c(v, i)
    }
  }
  subset_names[subset] = name
  subset_card[subset] = card
  subset_vectors[[subset]] = v
}

# Singletons are special because we can set choice probability to one
# without data
n_singletons = 1:n_objects
singletons = (1:n_subsets)[subset_card==1]
singleton_names = object_names

# Doubletons are special because they figure in revealed preference
# calculations
n_doubletons = choose(1:n_objects, 2)
doubletons = (1:n_subsets)[subset_card==2]
doubleton_names = subset_names[subset_card==2]

# Tripleons are special because they figure in 2D graphics
n_tripletons = choose(1:n_objects, 3)
tripletons = (1:n_subsets)[subset_card==3]
tripleton_names = subset_names[subset_card==3]

# Create table of revealed preference
# Value at (subset, object, doubleton) is 1, 0, or -1
#   1 if object is first object in doubleton, doubleton a subset of subset.
#  -1 if object is second object in doubleton, doubleton a subset of subset.
#   0 otherwise
RP_table = array(0, c(n_subsets, n_objects, n_doubletons[n_objects]),
                    dimnames = list(subset=subset_names,
                                    choice=object_names,
                                    doubleton=doubleton_names))
for (subset in seq(1, n_subsets)) {
  for (d in seq(1, n_doubletons[n_objects])) {
    if (bitwAnd(doubletons[d], subset)==doubletons[d]) {
      # Do this if doubleton is a subset of subset
      for (i in seq(1, n_objects)) {
        singleton = bitwShiftL(1, i-1)
        if (bitwAnd(singleton, bitwAnd(doubletons[d], subset)) > 0)
          RP_table[subset, i, d] =
            ifelse ((doubletons[d] - singleton > singleton), 1, -1)
      }
    }
  }
}

# R objects related to impossible choice probabilities
#  - membership function returns one if object obj in in subset sub, NA otherwise
#  - vmembership vectorizes membership function in both dimensions
#  - member_table gives 1 or NA for every element of subset X object table
membership = function(subs, obj) {ifelse(bitwAnd(subs, bitwShiftL(1, obj-1)) > 0, 1, NA)}
vmembership = Vectorize(membership)
member_table = outer(1:n_subsets, 1:n_objects, vmembership)

#' Vector of default object names, corresponding to \code{\link{subset_names}}
"object_names"

#' Vector of default subset names, corresponding to \code{\link{object_names}}
"subset_names"

#' Cardinality of subsets up to the set 111111 of six objects.
"subset_card"

#' List of vectors of choice objects in each choice subset.
"subset_vectors"

#' Vector of singleton subset indices, for universes up to size six
"singletons"

#' Vector of doubleton subset indices, for universes up to size six
"doubletons"

#' Vector of tripleton subset indices, for universes up to size six
"tripletons"

#' Vector of doubleton names, for universes up to size six
#'
#' Names correspond to \code{\link{object_names}}
"doubleton_names"

#' Vector of tripleton names, for universes up to size six
#'
#' Names correspond to \code{\link{object_names}}
"tripleton_names"

#' Array of revealed preference indicators
#'
#' Value at (subset, object, doubleton) is 1, 0, or -1
#' 1 if object is first object in doubleton, doubleton a subset of subset.
#'  -1 if object is second object in doubleton, doubleton a subset of subset.
#'   0 otherwise
#' Names correspond to \code{\link{object_names}}
"RP_table"

#' Matrix of set membership indicators
#'
#' Value at (subset, object) is 1 if object is an element of subset, NA otherwise
"member_table"

usethis::use_data(object_names,
                  subset_names,
                  subset_card,
                  subset_vectors,
                  singletons,
                  doubletons,
                  tripletons,
                  doubleton_names,
                  tripleton_names,
                  RP_table,
                  member_table,
                  overwrite = TRUE)

#' Precompute tables and matrices depending only on n, the number of objects
#'
#' For a choice universe of size n, precompute a list of frequently used
#' quantities, including tables and convenient row names. The result is a
#' required input for many functions in this package.
#'
#' @param n number of elements in choice universe
#' @param object_names vector of strings giving names of objects
#'
#' @return A list with the following elements
#'\describe{
#'   \item{n}{same as input with that name}
#'   \item{n_orders}{number of preference orders, equal to n!}
#'   \item{n_subsets}{number of non-empty subsets of choice universe}
#'   \item{n_probs}{total number of choice probabilities, over all doubleton and larger subsets}
#'   \item{orders}{list of preference orders, with each order a numeric n-vector}
#'   \item{order_strings}{list of strings (e.g. 'abc', 'acb') representing preference orders}
#'   \item{inv_orders}{list of inverse (in the sense of permutation inverse) of preference orders}
#'   \item{singletons}{vector of singletons, with sets coded as bit strings}
#'   \item{Ax_table}{matrix, n_probs by 2. Each row gives a unique pair of choice set and element}
#'   \item{Ax_strings}{vector of strings naming rows of Ax_table; the string 'ab;a', for example,
#'   identifies the pair consisting of choice set \{a,b\} and choice object a}
#'   \item{A_table}{n_subsets by 2 matrix. Each row gives the number of choice probabilities associated
#'   with the choice subset and the index of the first row of Ax_table where these choice probabilities
#'   are stored.}
#'   \item{A_strings}{vector of strings naming rows of A_table; the strings 'a' and
#'   'ab', for example, identify the subsets \{a\} and \{a,b\}}
#'   \item{order_strings}{vector of strings naming orders; the string 'abcde', for example, identifies
#'   the preference order \eqn{a \succ b \succ c \succ d \succ e}}
#'   \item{pi_to_P}{matrix giving choice probabilities when left multiplying preference probabilities}
#'   \item{pi_to_P_logical}{matrix, same as pi_to_P but with TRUE/FALSE replacing 1/0}
#' }
#'
#' @export
#'
#' @importFrom combinat combn permn
#' @importFrom Matrix invertPerm
#'
#' @examples
#' create_universe(3)      # Example where output is not excessively long
#' u = create_universe(5)  # Example corresponding to datasets in reference, above.
#'
#' @references
#' McCausland, W. (2024). Sequential Monte Carlo for Random Prefernces. Unpublished manuscript.
#' @author
#' William McCausland, \email{william.j.mccausland@umontreal.ca}
create_universe <- function(n, object_names = as.character(1:9)) {
  n_objects <- n               # Number of objects in universe
  n_orders <- factorial(n)     # Number of orders (permutations) over objects
  orders <- combinat::permn(n) # List of all orders (permutations)

  # Put orders in lexicographic order, with respect to numeric indices.
  # order_strings will be in lexicographic order if object names are letters in
  # alphabetic order.
  m <- do.call(rbind, orders)  # Convert list of vectors to matrix
  perm <- do.call(order, as.data.frame(m))  # Lexicographic order
  orders <- orders[perm]
  order_strings <- sapply(orders, function(x){paste(object_names[x], collapse='')})
  inv_orders <- lapply(orders, Matrix::invertPerm)  # Compute inverse permutations

  n_subsets <- 2^n-1           # Number of non-empty subsets
  singletons <- 2^(0:(n-1))    # Singleton sets in binary notation
  # Vector of flat indices giving singleton locations in matrix indexed by A and x
  singleton_is <- singletons + n_subsets * (0:(n-1))
  n_probs <- n*(2^(n-1)-1)     # Number of probabilities in an RCS

  # Initialize tables and matrices
  pi_to_P_logical <- matrix(nrow=n_probs, ncol=n_orders)
  A_table <- matrix(0, nrow=n_subsets, ncol=2)
  colnames(A_table) <- c('nA', 'Ax')
  Ax_table <- matrix(0, nrow=n_probs, ncol=3)
  colnames(Ax_table) <- c('A', 'x', 'i')

  # Fill in tables and matrices
  A_strings <- vector('character', n_subsets)
  Ax_strings <- vector('character', n_probs)
  Ax_index <- 1
  for (set_size in 1:n) {
    combin_table <- combinat::combn(n, set_size, simplify=FALSE)
    for (combin_index in 1:length(combin_table)) {
      # A is a subset of {1,...,n} of size set_size
      A_as_vector <- combin_table[[combin_index]]
      A_index <- sum(singletons[A_as_vector])
      A_strings[A_index] <- paste(object_names[A_as_vector], collapse='')
      if (set_size > 1) {
        A_table[A_index, 'nA'] <- set_size
        A_table[A_index, 'Ax'] <- Ax_index
        for (x in A_as_vector) {
          # x is an element of A
          x_as_string <- paste(object_names[x])
          Ax_table[Ax_index, 'A'] <- A_index
          Ax_table[Ax_index, 'x'] <- x
          Ax_table[Ax_index, 'i'] <- A_index + n_subsets * (x-1)
          Ax_strings[Ax_index] <- paste(A_strings[A_index], x_as_string, sep=';')
          for (order_index in 1:n_orders) {
            inv_order <- inv_orders[[order_index]]
            pi_to_P_logical[Ax_index, order_index] <-
              (inv_order[x]==min(inv_order[A_as_vector]))
          }
          Ax_index <- Ax_index + 1
        }
      }
    }
  }
  pi_to_P <- pi_to_P_logical
  pi_to_P[pi_to_P] <- 1
  rownames(Ax_table) <- Ax_strings
  rownames(A_table) <- A_strings
  dimnames(pi_to_P) <- list(Ax_strings, order_strings)
  dimnames(pi_to_P_logical) <- list(Ax_strings, order_strings)

  list(n=n, n_orders=n_orders, n_subsets=n_subsets, n_probs=n_probs,
       orders=orders, inv_orders=inv_orders, singletons=singletons,
       singleton_is=singleton_is,
       Ax_table=Ax_table, A_table=A_table,
       object_names = object_names[1:n],
       Ax_strings=Ax_strings, A_strings=A_strings, order_strings=order_strings,
       pi_to_P=pi_to_P, pi_to_P_logical=pi_to_P_logical)
}

