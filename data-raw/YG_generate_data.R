library(bitops)

YG_raw = read.csv('data-raw/YouGov.csv')
n_lines = dim(YG_raw)[1]

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

# Demographic categories
sex_names = c('Male', 'Female')
educ_names = c('No high school', 'High school graduate', 'Some college',
               '2-year', '4-year', 'Post-grad')
race_names = c('White', 'Black', 'Hispanic', 'Asian', 'Native American',
               'Mixed', 'Other', 'Middle Eastern')
region_names = c('Northeast', 'Midwest', 'South', 'West')
age_names = c('18 to 34', '35-54', '55+')

# Other experiment parameters
n_objects = 4
n_subsets = 2^n_objects-1
n_reps = 2 # Number of times a subject sees each domain
n_subjects_per_set = 10
n_subjects = (n_subsets - n_objects) * n_domains * n_subjects_per_set

# YG_counts dataset
YG_counts = array(0, c(n_domains, n_subsets, n_objects),
                  dimnames = list(domain=domain_names,
                                  subset=subset_names[1:n_subsets],
                                  objectd=object_names[1:n_objects]))

# YG_trials dataset
v = YG_raw[sprintf("option_%i", 1:n_objects)]
YG_trials = data.frame(subj = YG_raw$design,
                       domain = as.factor(domain_names[YG_raw$domain]),
                       trial = YG_raw$order,
                       subs = NA,
                       choice = as.factor(object_names[YG_raw$response]),
                       subs_conf = as.factor(YG_raw$choiceset),
                       subs_bin = apply(v, 1, function(v) sum(bitShiftL(1, v-1), na.rm = TRUE)),
                       choice_int = YG_raw$response)

# Make factors of subset and object names
YG_trials$subs = as.factor(subset_names[YG_trials$subs_bin])

# Add revealed preference information to YG_trials
YG_trials[doubleton_names[1:choose(n_objects, 2)]] =
  t(apply(YG_trials[c('subs_bin', 'choice_int')], 1,
          function(v) RP_table[v['subs_bin'], v['choice_int'], 1:choose(n_objects, 2)]))

# Set up YG_demographics database
subj_seq = seq(from=1, by=32, length.out=n_subjects)
YG_demographics =
  data.frame(sex = as.factor(sex_names[YG_raw[subj_seq, 'gender']]),
             educ = as.factor(educ_names[YG_raw[subj_seq, 'educ']]),
             region = as.factor(region_names[YG_raw[subj_seq, 'region']]),
             race = as.factor(race_names[YG_raw[subj_seq, 'race']]),
             age = as.factor(age_names[YG_raw[subj_seq, 'age_cross']]))

# Set up counts table, a 3D array indexed by domain, subset, choice
YG_counts = table(YG_trials[c('domain', 'subs_bin', 'choice_int')])

usethis::use_data(YG_raw, YG_counts, YG_trials, YG_demographics, overwrite=TRUE)
