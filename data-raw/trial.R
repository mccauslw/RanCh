# Compute standard variables for *_trials databases
# Input tb needs to be a tibble with set_vector and choice_int defined
compute_set_choice_vars <- function(tb)
{
  tb %>% mutate(

    # Choice set variables
	# --------------------
	# Permutation of objects in choice set presented to subject
    set_perm = map_chr(set_vector, function(l) paste(na.omit(object_names[l]), collapse='')),
	# Choice set as a subset of the universe of objects (binary representation)
    set_bin = map_int(set_vector, function(l) sum(as.integer(bitShiftL(1, l-1)), na.rm=TRUE)),
	# Choice set as a factor, with levels a, b, ab, c, ac, ...
    set = as.factor(subset_names[set_bin]),
	# Cardinality of the choice set
    set_card = map_int(set_vector, function(l) length(na.omit(l))),

    # Choice object variables
	# -----------------------
	# Choice object as a factor, with levels a, b, c, ...
    choice = as.factor(object_names[choice_int]))
}

# Put standard variables for a *_trials database first in a standard, easy to read order
arrange_set_choice_vars <- function(tb)
{
  tb %>% select(
	  # Standard variables to identify the trial,
	  matches('domain'), matches('subject'), matches('block'), trial, matches('duration'),
	  # Variables for choice set and choice object that are always present
	  set, choice, set_perm, set_card, set_bin, choice_int)
}
