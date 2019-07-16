library(bitops)
library(tidyverse)
source('data-raw/trial.R')

# There is one data file for each experimental subject.
# File names consist of subject number, letter code, extension .txt
# Thus, file names are 1A.txt, 2B.txt, ..., 141A.txt
letterCodes = c(
  'A', 'B', 'C', 'D', 'A', # 1-10
  'C', 'D', 'B', 'B', 'A',
  'A', 'C', 'D', 'B', 'C', # 11-20
  'A', 'D', 'D', 'C', 'A',
  'A', 'C', 'B', 'A', 'B', # 21-30
  'C', 'D', 'B', 'A', 'B',
  'C', 'D', 'D', 'C', 'A', # 31-40
  'B', 'D', 'D', 'B', 'A',
  'C', 'A', 'D', 'C', 'B', # 41-50
  'B', 'B', 'C', 'C', 'A',
  'D', 'D', 'A', 'B', 'C', # 51-60
  'C', 'D', 'D', 'B', 'A',
  'B', 'B', 'A', 'B', 'C', # 61-70
  'D', 'A', 'C', 'D', 'A',
  'C', 'D', 'A', 'B', 'B', # 71-80
  'D', 'C', 'C', 'D', 'D',
  'A', 'A', 'B', 'C', 'D', # 81-90
  'A', 'B', 'C', 'D', 'A',
  'B', 'D', 'C', 'A', 'B', # 91-100
  'C', 'D', 'A', 'B', 'C',
  'D', 'A', 'B', 'C', 'D', # 101-110
  'A', 'B', 'C', 'D', 'A',
  'B', 'B', 'C', 'D', 'D', # 111-120
  'C', 'A', 'C', 'A', 'B',
  'D', 'C', 'A', 'C', 'D', # 121-130
  'A', 'A', 'B', 'D', 'A',
  'C', 'D', 'A', 'A', 'B', # 131-140
  'C', 'D', 'D', 'B', 'C',
  'A' # 141
)
n_subjects <- length(letterCodes)

# Other experiment parameters
n_objects <- 5
n_subsets <- 2^n_objects-1
n_trials_per_subset = 6
n_trials_per_subject = (n_subsets - n_objects) * n_trials_per_subset
n_trials <- n_trials_per_subject * n_subjects

# Gamble names, distractor names, vector of all names
g_names <- sprintf('Gamble_%s.bmp', LETTERS[1:5])
d_names <- sprintf('D%i.bmp', 1:9)
all_names <- c(g_names, d_names, 'b.bmp')
numeric_objects <- c(1:(length(all_names)-1), NA)
names(numeric_objects) <- all_names

# Read in and vertically stack all subjects' responses.
for (i in 1:81) {
  filename <- sprintf('data-raw/MC_data_files/%i%s.txt', i, letterCodes[i])
  t_i <- read.csv2(filename, skip=1, header=TRUE, fileEncoding='UTF-16', sep='\t', skipNul=TRUE)
  t_i$Subject <- i
  t_i[sprintf('WaitClick%i.RT', 2:5)] <- NA
  MC_raw <- if (i==1) t_i else rbind(MC_raw, t_i)
}
for (i in 82:length(letterCodes)) {
  filename <- sprintf('data-raw/MC_data_files/%i%s.txt', i, letterCodes[i])
  t_i2 <- read.csv2(filename, header=TRUE, fileEncoding='UTF-16', sep='\t', skipNul=TRUE)
  t_i$Subject <- i
  MC_raw <- rbind(MC_raw, t_i2)
}

# Create MC_trials tibble, a database of all trials
MC_trials <- MC_raw %>% as_tibble() %>%

  select(num_range("g", 1:5), subject=Subject, index=ClickedGamble) %>%

  # Convert character codes for gambles in integer (including NA) codes
  mutate_at(vars(num_range("g", 1:5)), function(g) numeric_objects[as.character(g)]) %>%

  # Drop unconfirmed clicks, distractor trials
  filter(g1 <= n_objects, !is.na(index)) %>%

  # Compute standard variables
  mutate(
    trial = as.integer(1 + (0:(n_trials-1) %% n_trials_per_subject)),
    set_vector = pmap(.[1:5], c),
    choice_int = map2_int(set_vector, index, function(v, i) v[i])) %>%

  # Compute variables for choice sets and choice objects
  compute_set_choice_vars() %>%

  # Put standard variables in order for easy reading
  arrange_set_choice_vars()

# Add revealed preference information to MC_trials
MC_trials[doubleton_names[1:choose(n_objects, 2)]] =
  t(apply(MC_trials[c('set_bin', 'choice_int')], 1,
          function(v) as.integer(RP_table[v['set_bin'], v['choice_int'], 1:choose(n_objects, 2)])))

# Compute choice counts by subject, subset and choice object
MC_table <- table(MC_trials[c('subject', 'set_bin', 'choice_int')])
# Dimension naming for MC_counts
MC_count_dimnames <- dimnames(MC_table)
names(MC_count_dimnames) <- c('Subject', 'Subset', 'Object')
MC_count_dimnames$Subset <- subset_names[1:n_subsets]
MC_count_dimnames$Object <- object_names[1:n_objects]
# Create matrix with correct names, fill in counts for all subsets, even singletons
MC_counts <- array(0, dim=c(n_subjects, n_subsets, n_objects), dimnames = MC_count_dimnames)
MC_counts[, (1:n_subsets)[subset_card[1:n_subsets]>1], ] <- MC_table
# Set counts that don't make sense (e.g. number of times a chosen from {b,c}) to NA
MC_counts <- MC_counts * outer(rep(1, n_subjects), member_table[1:n_subsets, 1:n_objects])

usethis::use_data(MC_raw, MC_trials, MC_counts, overwrite=TRUE)
