# This file creates a named list u_const with various constant scalars,
# vectors and matrices. u_const is documented, but not exported.

max_objects <- 6
n_obj_grid <- seq.int(max_objects)
object_names <- letters[n_obj_grid]
max_subsets <- 2^max_objects - 1
n_subset_grid <- seq.int(max_subsets)

# Compute names and cardinality of all subsets of {1,2,...,max_objects}
subset_names <- vector(mode='character', length=max_subsets)
subset_card <- vector(mode='integer', length=max_subsets)
subset_vectors = vector(mode='list', length=max_subsets)
for (subset in n_subset_grid) {
  name <- ''
  card <- 0
  v = c()
  for (i in n_obj_grid) {
    if (bitwAnd(subset, bitwShiftL(1, i-1))) {
      name <- paste(name, object_names[i], sep='')
      card <- card+1
      v <- c(v, i)
    }
  }
  subset_names[subset] <- name
  subset_card[subset] <- card
  subset_vectors[[subset]] <- v
}

n_subsets <- 2^n_obj_grid - 1
n_orders <- factorial(n_obj_grid)
n_Ax <- 0.5 * (n_subsets-1) * n_obj_grid

# Singletons are special because we the single choice probability is always
# equal to one.
n_singletons <- n_obj_grid
singletons <- n_subset_grid[subset_card==1]
singleton_names <- object_names

# Doubletons are special because they figure in revealed preference
# calculations, and because some datasets only feature binary choice
n_doubletons <- choose(n_obj_grid, 2)
doubletons <- n_subset_grid[subset_card==2]
doubleton_names <- subset_names[subset_card==2]

# Tripleons are special because they figure in 2D graphics, and in context
# effects.
n_tripletons <- choose(n_obj_grid, 3)
tripletons <- n_subset_grid[subset_card==3]
tripleton_names <- subset_names[subset_card==3]

# Create table of revealed preference
# Value at (subset, object, doubleton) is 1, 0, or -1
#   1 if object is first object in doubleton, doubleton a subset of subset.
#  -1 if object is second object in doubleton, doubleton a subset of subset.
#   0 otherwise
RP_table = array(0, c(max_subsets, max_objects, n_doubletons[max_objects]),
                  dimnames = list(subset=subset_names,
                                  choice=object_names,
                                  doubleton=doubleton_names))
for (subset in n_subset_grid) {
  for (d in seq(1, n_doubletons[max_objects])) {
    if (bitwAnd(doubletons[d], subset)==doubletons[d]) {
      # Do this if doubleton is a subset of subset
      for (i in n_obj_grid) {
        singleton <- bitwShiftL(1, i-1)
        if (bitwAnd(singleton, bitwAnd(doubletons[d], subset)) > 0) {
          RP_table[subset, i, d] <-
            ifelse ((doubletons[d] - singleton > singleton), 1, -1)
        }
      }
    }
  }
}

# R objects related to impossible choice probabilities
#  - membership function returns one if object obj in in subset sub, NA otherwise
#  - vmembership vectorizes membership function in both dimensions
#  - member_table gives 1 or NA for every element of subset X object table
membership <- function(subs, obj) {ifelse(bitwAnd(subs, bitwShiftL(1, obj-1)) > 0, 1, NA)}
vmembership <- Vectorize(membership)
member_table <- outer(n_subset_grid, n_obj_grid, vmembership)

u_const <- list(
  n_subsets         = n_subsets,
  n_orders          = n_orders,
  n_Ax              = n_Ax,
  object_names      = object_names,
  subset_names      = subset_names,
  subset_card       = subset_card,
  subset_vectors    = subset_vectors,
  n_singletons      = n_singletons,
  singletons        = singletons,
  n_doubletons      = n_doubletons,
  doubletons        = doubletons,
  doubleton_names   = doubleton_names,
  n_tripletons      = n_tripletons,
  tripletons        = tripletons,
  RP_table          = RP_table,
  member_table      = member_table
)

usethis::use_data(u_const, overwrite = TRUE)
