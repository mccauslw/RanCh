library(tidyverse)
source('./data-raw/trial.R')

MMS_2019_raw = read.csv("data-raw/MMS_2019.csv")
MMS_2019_raw <- subset(MMS_2019_raw, select = -feedback)
n_lines = nrow(MMS_2019_raw)

# Names of the 32 choice domains in the experiment
domain_names = c(
	'Male stars',           # 1
	'Female stars',
	'Films',
	'Star pairs',
	'Pizzas',               # 5
	'Juices',
	'Colours',
	'Colour Combinations',
	'Events',
	'Radio formats',        # 10
	'Musical artists',
	'Aboriginal art',
	'Impressionist art',
	'Sentences',
	'Travel',               # 15
	'Marijuana',
	'Latitude',
	'Dots',
	'Triangles',
	'Population',           # 20
	'Surface area',
	'Beer',
	'Cars',
	'Restaurants',
	'Flight layovers',      # 25
	'Future payments',
	'Phone plans',
	'Hotel rooms',
	'Two-flight itineraries',
	'Televisions',          # 30
	'Coffee',
	'Charity')
n_domains = length(domain_names)

# Other experiment parameters
n_objects = 5
n_subsets = 2^n_objects-1
n_subjects_per_set = 40
n_subjects_extra = 2
n_subjects = (n_subsets - n_objects) * n_subjects_per_set + n_subjects_extra

# Demographic categories
sex_names = c('Male', 'Female')
location_names = c('Alberta', 'British Columbia', 'Manitoba', 'New Brunswick',
                   'Newfoundland/Labrador', 'Northwest Territories',
                   'Nova Scotia', 'Ontario', 'Prince Edward Island',
                   'Quebec', 'Saskatchewan', 'Yukon Territory')

# Create MMS_2019_trials tibble, a database of all trials
MMS_2019_trials <- MMS_2019_raw %>% as_tibble() %>%

  mutate(
    domain = as.factor(domain_names[domain]),
    subject = as.integer((0:(n_lines-1)) %/% n_domains + 1),
    trial = set,
    duration = expdur/1000,
    set_vector = pmap(.[sprintf("obj%d", 1:n_objects)], c),
    choice_int = map2_int(set_vector, choice, function(v, i) v[i])) %>%

  # Compute variables for choice sets and choice objects
  compute_set_choice_vars() %>%

  # Put standard variables in order for easy reading
  arrange_set_choice_vars()

# Add revealed preference information to MMS_2019_trials
MMS_2019_trials[doubleton_names[1:choose(n_objects, 2)]] =
  t(apply(MMS_2019_trials[c('set_index', 'choice_int')], 1,
          function(v) as.integer(RP_table[v['set_index'], v['choice_int'],
                                          1:choose(n_objects, 2)])))

# Set up MMS_2019_demographics database
subj_seq = seq(from=1, by=32, length.out=n_subjects)
MMS_2019_demographics =
  tibble(sex = as.factor(sex_names[MMS_2019_raw[subj_seq, 'gender']]),
         age = MMS_2019_raw[subj_seq, 'age'],
         location = as.factor(location_names[MMS_2019_raw[subj_seq, 'location']]))

# Construct 3-D array MMS_2019_counts to contain choice counts, indexed by
# (domain, choice set, object), where
# domain is the index of the domain (see order in domain_names, above),
# choice set is the binary set representation of the choice subset, with
#   0 the empty set and 32=11111b the master set of all five objects in the domain,
#   2^(i-1) the singleton set with object i, i=1,2,3,4,5
#   (Note: bitwise OR (AND) of representations of two sets gives the representation of the union (intersection, repectively),
# object is the index (one of 1,2,3,4,5) of the chosen object.

# Compute choice counts by domain, subset and choice object
MMS_2019_table = table(MMS_2019_trials[c('domain', 'set_index', 'choice_int')])

# Dimension naming for MMS_2019_counts
MMS_2019_count_dimnames = dimnames(MMS_2019_table)
names(MMS_2019_count_dimnames) = c('Domain', 'Subset', 'Object')
MMS_2019_count_dimnames$Subset = subset_names[1:n_subsets]
MMS_2019_count_dimnames$Object = object_names[1:n_objects]

# Create matrix with correct names, fill in counts for all subsets, even singletons
MMS_2019_counts = array(0, dim=c(n_domains, n_subsets, n_objects), dimnames = MMS_2019_count_dimnames)
MMS_2019_counts[, (1:n_subsets)[subset_card[1:n_subsets]>1], ] = MMS_2019_table

# Set counts that don't make sense (e.g. number of times a chosen from {b,c}) to NA
MMS_2019_counts = MMS_2019_counts * outer(rep(1, n_domains), member_table[1:n_subsets, 1:n_objects])

usethis::use_data(MMS_2019_raw, MMS_2019_counts, MMS_2019_trials, MMS_2019_demographics, overwrite=TRUE)
