library(bitops)
library(tidyverse)

YG_raw = read.csv('data-raw/YouGov.csv')
n_lines = nrow(YG_raw)

# Names of the 16 domains in the experiment
domain_names = c('Art',         # 1
                 'Author',
                 'City',
                 'Beer',
                 'Destination', # 5
                 'Actor',
                 'Hobby',
                 'President',
                 'Group',
                 'Genre',       # 10
                 'Instrument',
                 'Credit',
                 'League',
                 'Outcome',
                 'Smartphone',  # 15
                 'Car')
n_domains = length(domain_names)

# Other experiment parameters
n_objects = 4
n_subsets = 2^n_objects-1
n_reps = 2 # Number of times a subject sees each domain
n_subjects_per_set = 10
n_subjects = (n_subsets - n_objects) * n_domains * n_subjects_per_set

# Demographic categories
sex_names = c('Male', 'Female')
educ_names = c('No high school', 'High school graduate', 'Some college',
               '2-year', '4-year', 'Post-grad')
race_names = c('White', 'Black', 'Hispanic', 'Asian', 'Native American',
               'Mixed', 'Other', 'Middle Eastern')
region_names = c('Northeast', 'Midwest', 'South', 'West')
age_names = c('18 to 34', '35-54', '55+')

# Create YG_trials tibble, a database of all trials
YG_trials <- YG_raw %>% as_tibble() %>%

  # Compute standard variables
  mutate(
    domain = as.factor(domain_names[domain]),
    subj = design,
    trial = order,
    subs_vec = pmap(.[sprintf("option_%d", 1:n_objects)], c),
    choice_int = response,
    choice = as.factor(object_names[choice_int]),
    subs_conf = choiceset,
    subs_bin = map_int(subs_vec, function(l) sum(as.integer(bitShiftL(1, l-1)), na.rm=TRUE)),
    subs = as.factor(subset_names[subs_bin])) %>%

  # Drop intermediate unused variables
  select(-starts_with("option"), -design, -card, -combo, -perm, -choiceset, -response,
         -gender, -educ, -region, -race, -age_cross)

# Add revealed preference information to YG_trials
YG_trials[doubleton_names[1:choose(n_objects, 2)]] =
  t(apply(YG_trials[c('subs_bin', 'choice_int')], 1,
          function(v) RP_table[v['subs_bin'], v['choice_int'], 1:choose(n_objects, 2)]))

# Set up YG_demographics database
subj_seq = seq(from=1, by=32, length.out=n_subjects)
YG_demographics =
  tibble(sex = as.factor(sex_names[YG_raw[subj_seq, 'gender']]),
         educ = as.factor(educ_names[YG_raw[subj_seq, 'educ']]),
         region = as.factor(region_names[YG_raw[subj_seq, 'region']]),
         race = as.factor(race_names[YG_raw[subj_seq, 'race']]),
         age = as.factor(age_names[YG_raw[subj_seq, 'age_cross']]))

# Compute choice counts by domain, subset and choice object
YG_table = table(YG_trials[c('domain', 'subs_bin', 'choice_int')])
# Dimension naming for MC_counts
YG_count_dimnames = dimnames(YG_table)
names(YG_count_dimnames) = c('Domain', 'Subset', 'Object')
YG_count_dimnames$Object = object_names[1:n_objects]
YG_count_dimnames$Subset = subset_names[1:n_subsets]
# Create matrix with correct names, fill in counts for all subsets, even singletons
YG_counts = array(0, dim=c(n_domains, n_subsets, n_objects), dimnames = YG_count_dimnames)
YG_counts[, (1:n_subsets)[subset_card[1:n_subsets]>1], ] = YG_table
# Set counts that don't make sense (e.g. number of times a chosen from {b,c}) to NA
YG_counts = YG_counts * outer(rep(1, n_domains), member_table[1:n_subsets, 1:n_objects])

usethis::use_data(YG_raw, YG_counts, YG_trials, YG_demographics, overwrite=TRUE)
