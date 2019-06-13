library(bitops)
library(tidyverse)

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

object_names = c('a', 'b', 'c', 'd', 'e')
n_objects = length(object_names)

# Gamble names, distractor names, vector of all names
g_names = sprintf('Gamble_%s.bmp', LETTERS[1:5])
d_names = sprintf('D%i.bmp', 1:9)
all_names = c(g_names, d_names, 'b.bmp')
numeric_objects = c(1:(length(all_names)-1), NA)
names(numeric_objects) = all_names

# Read in and vertically stack all the subjects' responses.
for (i in 1:81) {
  filename = sprintf('data-raw/MC_data_files/%i%s.txt', i, letterCodes[i])
  t_i = read.csv2(filename, skip=1, header=TRUE, fileEncoding='UTF-16', sep='\t', skipNul=TRUE)
  t_i[sprintf('WaitClick%i.RT', 2:5)] = NA
  MC_raw = if (i==1) t_i else rbind(MC_raw, t_i)
}
for (i in 82:length(letterCodes)) {
  filename = sprintf('data-raw/MC_data_files/%i%s.txt', i, letterCodes[i])
  t_i2 = read.csv2(filename, header=TRUE, fileEncoding='UTF-16', sep='\t', skipNul=TRUE)
  MC_raw = rbind(MC_raw, t_i2)
}

code <- function(g) {numeric_objects[as.character(g)]}
clean <- function(index) {as.integer(index)}
choice <- function(v) {v[[v[[8]]]]}

lfn <- function(v) list(1:v)

MC_trials <- MC_raw %>% as_tibble() %>%
  select(num_range("g", 1:5), subj = Subject, trial = Trial, index=ClickedGamble) %>%
  mutate_at(vars(num_range("g", 1:5)), code) %>%
  filter(g1 <= n_objects, !is.na(index)) %>%
  mutate(
    choice_int = case_when(
      index==1 ~ g1, index==2 ~ g2, index==3 ~ g3, index==4 ~ g4, index==5 ~ g5
    ),
    choice = as.factor(object_names[choice_int])
  )

vec <- function(x, y, z) c(x, y, z)

# MC_trials dataset
v = apply(MC_raw[sprintf('g%i', 1:n_objects)], 2, function(x) numeric_objects[as.character(x)])
v = cbind(v, MC_raw$ClickedGamble)
#MC_trials = data.frame(subj = MC_raw$Subject,
#                       trial = MC_raw$Trial,
#                       subs = NA,
#                       choice = as.factor(object_names[MC_raw$response]),
#                       subs_conf = apply(v, 1, function(v) paste(na.omit(object_names[v[1:5]]), collapse='')),
#                       subs_bin = apply(v, 1, function(v) sum(bitShiftL(1, v-1), na.rm = TRUE)),
#                       choice_int = apply(v, 1, function(v) v[v[6]]))
#MC_trials$subs = as.factor(subset_names[MC_trials$subs_bin])

