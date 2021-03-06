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
n_singletons = n_objects
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

#usethis::use_data(member_table,
#                  RP_table,
#                  internal = TRUE,
#                  overwrite = TRUE)

