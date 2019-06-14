library(bitops)
library(tidyverse)

PC_raw = read.csv("data-raw/Population.csv")
n_lines = nrow(PC_raw)

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

# Set up storage for PC_trials database
PC_trials = <- PC_raw %>% as_tibble() %>%
  mutate(
    subj = (0:(n_lines-1)) %/% n_domains + 1,
    domain = as.factor(domain_names[domain]),
    trial = set,
    choice_int = ?,
    choice = as.factor(object_names[choice_int]),
    subs_conf = map_chr(subs_list, function(l) paste(na.omit(object_names[l]), collapse='')),
    subs_bin = map_int(sub_list, function(l) sum(as.integer(bitShiftL(1, l-1)), na.rm=TRUE)),
    subs = as.factor(subset_names[subs_bin]),
    choice = as.factor(object_names[choice_int])
	)

# Choice subset and chosen object as factors with string and letter values
PC_trials$subs = as.factor(subset_names[PC_trials$subs_bin])
PC_trials$choice = as.factor(object_names[PC_trials$choice_int])

# Add revealed preference information to PC_trials
PC_trials[doubleton_names[1:choose(n_objects, 2)]] =
  t(apply(PC_trials[c('subs_bin', 'choice_int')], 1,
          function(v) {RP_table[v['subs_bin'], v['choice_int'], 1:choose(n_objects, 2)]}))

# Set up PC_demographics database
subj_seq = seq(from=1, by=32, length.out=n_subjects)
PC_demographics =
  data.frame(sex = as.factor(sex_names[PC_raw[subj_seq, 'gender']]),
             age = PC_raw[subj_seq, 'age'],
             location = as.factor(location_names[PC_raw[subj_seq, 'location']]))

# Construct 3-D array PC_counts to contain choice counts, indexed by
# (domain, choice set, object), where
# domain is the index of the domain (see order in domain_names, above),
# choice set is the binary set representation of the choice subset, with
#   0 the empty set and 32=11111b the master set of all five objects in the domain,
#   2^(i-1) the singleton set with object i, i=1,2,3,4,5
#   (Note: bitwise OR (AND) of representations of two sets gives the representation of the union (intersection, repectively),
# object is the index (one of 1,2,3,4,5) of the chosen object.

# Compute choice counts by domain, subset and choice object
PC_table = table(PC_trials[c('domain', 'subs_bin', 'choice_int')])
# Dimension naming for MC_counts
PC_count_dimnames = dimnames(PC_table)
names(PC_count_dimnames) = c('Domain', 'Subset', 'Object')
PC_count_dimnames$Object = object_names[1:n_objects]
PC_count_dimnames$Subset = subset_names[1:n_subsets]
# Create matrix with correct names, fill in counts for all subsets, even singletons
PC_counts = array(0, dim=c(n_domains, n_subsets, n_objects), dimnames = PC_count_dimnames)
PC_counts[, (1:n_subsets)[subset_card[1:n_subsets]>1], ] = PC_table
# Set counts that don't make sense (e.g. number of times a chosen from {b,c}) to NA
PC_counts = PC_counts * outer(rep(1, n_domains), member_table[1:n_subsets, 1:n_objects])

usethis::use_data(PC_raw, PC_counts, PC_trials, PC_demographics, overwrite=TRUE)
