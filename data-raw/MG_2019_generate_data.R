library(tidyverse)
source('./data-raw/trial.R')

MG_2019_raw <- read.csv('data-raw/MG_2019.csv')
n_lines <- nrow(MG_2019_raw)

# Names of the 16 domains in the experiment
domain_names <- c('Art',         # 1
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
n_domains <- length(domain_names)

# Other experiment parameters
n_objects <- 4
n_subsets <- 2^n_objects-1
n_blocks <- 2 # Number of times a subject sees each domain
n_subjects_per_set <- 10
n_subjects <- (n_subsets - n_objects) * n_domains * n_subjects_per_set
n_waves <- 2

# Demographic categories
sex_names <- c('Male', 'Female')
educ_names <- c('No high school', 'High school graduate', 'Some college',
               '2-year', '4-year', 'Post-grad')
race_names <- c('White', 'Black', 'Hispanic', 'Asian', 'Native American',
               'Mixed', 'Other', 'Middle Eastern')
region_names <- c('Northeast', 'Midwest', 'South', 'West')
age_names <- c('18 to 34', '35-54', '55+')

# Create MG_2019_trials tibble, a database of all trials
MG_2019_trials <- MG_2019_raw %>% as_tibble() %>%

  # Compute standard variables
  mutate(
    domain = as.factor(domain_names[domain]),
    subject = design,
    block = rep(c(rep(1, n_domains), rep(2, n_domains)), n_subjects),
    trial = order,
    set_vector = pmap(.[sprintf("option_%d", 1:n_objects)], c),
    choice_int = response) %>%

  # Compute variables for choice sets and choice objects
  compute_set_choice_vars() %>%

  # Put standard variables in order for easy reading
  arrange_set_choice_vars()

# Add revealed preference information to MG_2019_trials
MG_2019_trials[doubleton_names[1:choose(n_objects, 2)]] =
  t(apply(MG_2019_trials[c('set_index', 'choice_int')], 1,
          function(v) as.integer(RP_table[v['set_index'], v['choice_int'],
                                          1:choose(n_objects, 2)])))

# Set up MG_2019_demographics database
subj_seq = seq(from=1, by=32, length.out=n_subjects)
MG_2019_demographics =
  tibble(sex = as.factor(sex_names[MG_2019_raw[subj_seq, 'gender']]),
         educ = as.factor(educ_names[MG_2019_raw[subj_seq, 'educ']]),
         region = as.factor(region_names[MG_2019_raw[subj_seq, 'region']]),
         race = as.factor(race_names[MG_2019_raw[subj_seq, 'race']]),
         age = as.factor(age_names[MG_2019_raw[subj_seq, 'age_cross']]))

# Compute choice counts by domain, subset and choice object
MG_2019_table <- table(MG_2019_trials[c('domain', 'block', 'set_index', 'choice_int')])

# Dimension naming for MG_2019_counts
MG_2019_count_dimnames <- dimnames(MG_2019_table)
names(MG_2019_count_dimnames) <- c('Domain', 'Block', 'Subset', 'Object')
MG_2019_count_dimnames$Block <- c('1st wave', '2nd wave')
MG_2019_count_dimnames$Subset <- subset_names[1:n_subsets]
MG_2019_count_dimnames$Object <- object_names[1:n_objects]

# Create matrix with correct names, fill in counts for all subsets, even singletons
MG_2019_counts <- array(0, dim=c(n_domains, n_waves, n_subsets, n_objects),
                        dimnames = MG_2019_count_dimnames)
MG_2019_counts[, , (1:n_subsets)[subset_card[1:n_subsets]>1], ] = MG_2019_table

# Set counts that don't make sense (e.g. number of times a chosen from {b,c}) to NA
MG_2019_counts <- MG_2019_counts * outer(rep(1, n_domains) %o% rep(1, n_waves),
                              member_table[1:n_subsets, 1:n_objects])

usethis::use_data(MG_2019_raw, MG_2019_counts, MG_2019_trials, MG_2019_demographics,
                  overwrite=TRUE)
