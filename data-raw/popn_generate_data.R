library(bitops)
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

# Demographic categories
sex_names = c('Male', 'Female')
location_names = c('Alberta', 'British Columbia', 'Manitoba', 'New Brunswick',
                   'Newfoundland/Labrador', 'Northwest Territories',
                   'Nova Scotia', 'Ontario', 'Prince Edward Island',
                   'Quebec', 'Saskatchewan', 'Yukon Territory')

# Construct the 3-D array PC_counts to contain all choice counts.
# PC_counts is indexed by (domain, choice set, object),
# where domain is the index of the domain (see order in domain_names),
# choice set is the binary set representation of the subset, with
#   0 the empty set and 32=11111b the master set of all five objects in the domain,
#   2^(i-1) the singleton set with object i, i=1,2,3,4,5
#   (Note: bitwise OR (AND) of representations of two sets gives the representation of the union (intersection, repectively).
# object the index (i=1,2,3,4,5) of the set.
n_domains = length(domain_names)
n_objects = 5
n_subsets = 2^n_objects-1
n_doubletons = choose(n_objects, 2)
n_trial_subsets = n_subsets - n_objects
n_subjects = 1042

# Set up storage for PC_trials database
PC_trials = data.frame(subj = (0:(n_lines-1)) %/% n_domains + 1,
                       domain = as.factor(domain_names[PC_raw$domain]),
                       trial = PC_raw$set,
                       subs = NA,
                       choice = NA,
                       subs_conf = apply(v, 1, function(v) paste(na.omit(object_names[v[1:5]]), collapse='')),
                       subs_bin = apply(v, 1, function(v) sum(bitShiftL(1, v[1:5]-1), na.rm = TRUE)),
                       choice_int = apply(v, 1, function(v) v[v[6]]))

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

# Create PC_counts, a 3D array indexed by domain, subset, choice
PC_table = table(PC_trials[c('domain', 'subs_bin', 'choice_int')])
PC_count_dimnames = dimnames(PC_table)
names(PC_count_dimnames) = c('Domain', 'Subset', 'Object')
PC_count_dimnames$Object = object_names[1:n_objects]
PC_count_dimnames$Subset = subset_names[1:n_subsets]
PC_counts = array(0, dim=c(n_domains, n_subsets, n_objects), dimnames = PC_count_dimnames)
PC_counts[, (1:n_subsets)[subset_card[1:n_subsets]>1], ] = PC_table
PC_counts = PC_counts * outer(rep(1, n_domains), member_table[1:n_subsets, 1:n_objects])

usethis::use_data(PC_raw, PC_counts, PC_trials, PC_demographics, overwrite=TRUE)
