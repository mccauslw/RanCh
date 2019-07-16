# Create count matrices for data in Tversky (1972)

n_objects <- 3
n_subsets <- 2^n_objects - 1
domain_names <- c("Dots", "Gambles", "Applicants")
n_domains <- length(domain_names)
n_subjects <- 8

local_object_names <- c('x', 'y', 'z')
local_subset_names <- c('x', 'y', 'xy', 'z', 'xz', 'yz', 'xyz')

T_1972_counts <- array(0, dim=c(n_domains, n_subjects, n_subsets, n_objects),
                       dimnames=list(Domain=domain_names, NULL,
                                     Subset=local_subset_names,
                                     Object=local_object_names))

T_1972_counts <- T_1972_counts * outer(outer(rep(1, n_domains), rep(1, n_subjects)),
                                       member_table[1:n_subsets, 1:n_objects])

T_1972_counts["Dots", 1, 'xz', 'x'] = 10
T_1972_counts["Dots", 1, 'xz', 'z'] = 10
T_1972_counts["Dots", 1, 'yz', 'y'] = 9
T_1972_counts["Dots", 1, 'yz', 'z'] = 11
T_1972_counts["Dots", 1, 'xyz', 'x'] = 9
T_1972_counts["Dots", 1, 'xyz', 'y'] = 9
T_1972_counts["Dots", 1, 'xyz', 'z'] = 12
T_1972_counts["Dots", 2, 'xz', 'x'] = 12
T_1972_counts["Dots", 2, 'xz', 'z'] = 8
T_1972_counts["Dots", 2, 'yz', 'y'] = 7
T_1972_counts["Dots", 2, 'yz', 'z'] = 13
T_1972_counts["Dots", 2, 'xyz', 'x'] = 6
T_1972_counts["Dots", 2, 'xyz', 'y'] = 8
T_1972_counts["Dots", 2, 'xyz', 'z'] = 16
T_1972_counts["Dots", 3, 'xz', 'x'] = 5
T_1972_counts["Dots", 3, 'xz', 'z'] = 15
T_1972_counts["Dots", 3, 'yz', 'y'] = 8
T_1972_counts["Dots", 3, 'yz', 'z'] = 12
T_1972_counts["Dots", 3, 'xyz', 'x'] = 8
T_1972_counts["Dots", 3, 'xyz', 'y'] = 9
T_1972_counts["Dots", 3, 'xyz', 'z'] = 13
T_1972_counts["Dots", 4, 'xz', 'x'] = 14
T_1972_counts["Dots", 4, 'xz', 'z'] = 6
T_1972_counts["Dots", 4, 'yz', 'y'] = 6
T_1972_counts["Dots", 4, 'yz', 'z'] = 14
T_1972_counts["Dots", 4, 'xyz', 'x'] = 15
T_1972_counts["Dots", 4, 'xyz', 'y'] = 10
T_1972_counts["Dots", 4, 'xyz', 'z'] = 5
T_1972_counts["Dots", 5, 'xz', 'x'] = 13
T_1972_counts["Dots", 5, 'xz', 'z'] = 7
T_1972_counts["Dots", 5, 'yz', 'y'] = 7
T_1972_counts["Dots", 5, 'yz', 'z'] = 13
T_1972_counts["Dots", 5, 'xyz', 'x'] = 12
T_1972_counts["Dots", 5, 'xyz', 'y'] = 7
T_1972_counts["Dots", 5, 'xyz', 'z'] = 11
T_1972_counts["Dots", 6, 'xz', 'x'] = 8
T_1972_counts["Dots", 6, 'xz', 'z'] = 12
T_1972_counts["Dots", 6, 'yz', 'y'] = 9
T_1972_counts["Dots", 6, 'yz', 'z'] = 11
T_1972_counts["Dots", 6, 'xyz', 'x'] = 7
T_1972_counts["Dots", 6, 'xyz', 'y'] = 12
T_1972_counts["Dots", 6, 'xyz', 'z'] = 11
T_1972_counts["Dots", 7, 'xz', 'x'] = 3
T_1972_counts["Dots", 7, 'xz', 'z'] = 17
T_1972_counts["Dots", 7, 'yz', 'y'] = 9
T_1972_counts["Dots", 7, 'yz', 'z'] = 11
T_1972_counts["Dots", 7, 'xyz', 'x'] = 5
T_1972_counts["Dots", 7, 'xyz', 'y'] = 11
T_1972_counts["Dots", 7, 'xyz', 'z'] = 14
T_1972_counts["Dots", 8, 'xz', 'x'] = 3
T_1972_counts["Dots", 8, 'xz', 'z'] = 17
T_1972_counts["Dots", 8, 'yz', 'y'] = 9
T_1972_counts["Dots", 8, 'yz', 'z'] = 11
T_1972_counts["Dots", 8, 'xyz', 'x'] = 2
T_1972_counts["Dots", 8, 'xyz', 'y'] = 16
T_1972_counts["Dots", 8, 'xyz', 'z'] = 12

T_1972_counts["Gambles", 1, 'xz', 'x'] = 7
T_1972_counts["Gambles", 1, 'xz', 'z'] = 13
T_1972_counts["Gambles", 1, 'yz', 'y'] = 10
T_1972_counts["Gambles", 1, 'yz', 'z'] = 10
T_1972_counts["Gambles", 1, 'xyz', 'x'] = 2
T_1972_counts["Gambles", 1, 'xyz', 'y'] = 13
T_1972_counts["Gambles", 1, 'xyz', 'z'] = 15
T_1972_counts["Gambles", 2, 'xz', 'x'] = 12
T_1972_counts["Gambles", 2, 'xz', 'z'] = 8
T_1972_counts["Gambles", 2, 'yz', 'y'] = 14
T_1972_counts["Gambles", 2, 'yz', 'z'] = 6
T_1972_counts["Gambles", 2, 'xyz', 'x'] = 8
T_1972_counts["Gambles", 2, 'xyz', 'y'] = 15
T_1972_counts["Gambles", 2, 'xyz', 'z'] = 7
T_1972_counts["Gambles", 3, 'xz', 'x'] = 5
T_1972_counts["Gambles", 3, 'xz', 'z'] = 15
T_1972_counts["Gambles", 3, 'yz', 'y'] = 10
T_1972_counts["Gambles", 3, 'yz', 'z'] = 10
T_1972_counts["Gambles", 3, 'xyz', 'x'] = 6
T_1972_counts["Gambles", 3, 'xyz', 'y'] = 7
T_1972_counts["Gambles", 3, 'xyz', 'z'] = 17
T_1972_counts["Gambles", 4, 'xz', 'x'] = 12
T_1972_counts["Gambles", 4, 'xz', 'z'] = 8
T_1972_counts["Gambles", 4, 'yz', 'y'] = 14
T_1972_counts["Gambles", 4, 'yz', 'z'] = 6
T_1972_counts["Gambles", 4, 'xyz', 'x'] = 10
T_1972_counts["Gambles", 4, 'xyz', 'y'] = 7
T_1972_counts["Gambles", 4, 'xyz', 'z'] = 13
T_1972_counts["Gambles", 5, 'xz', 'x'] = 4
T_1972_counts["Gambles", 5, 'xz', 'z'] = 16
T_1972_counts["Gambles", 5, 'yz', 'y'] = 10
T_1972_counts["Gambles", 5, 'yz', 'z'] = 10
T_1972_counts["Gambles", 5, 'xyz', 'x'] = 3
T_1972_counts["Gambles", 5, 'xyz', 'y'] = 11
T_1972_counts["Gambles", 5, 'xyz', 'z'] = 16
T_1972_counts["Gambles", 6, 'xz', 'x'] = 13
T_1972_counts["Gambles", 6, 'xz', 'z'] = 7
T_1972_counts["Gambles", 6, 'yz', 'y'] = 12
T_1972_counts["Gambles", 6, 'yz', 'z'] = 8
T_1972_counts["Gambles", 6, 'xyz', 'x'] = 12
T_1972_counts["Gambles", 6, 'xyz', 'y'] = 8
T_1972_counts["Gambles", 6, 'xyz', 'z'] = 10
T_1972_counts["Gambles", 7, 'xz', 'x'] = 11
T_1972_counts["Gambles", 7, 'xz', 'z'] = 9
T_1972_counts["Gambles", 7, 'yz', 'y'] = 13
T_1972_counts["Gambles", 7, 'yz', 'z'] = 7
T_1972_counts["Gambles", 7, 'xyz', 'x'] = 8
T_1972_counts["Gambles", 7, 'xyz', 'y'] = 11
T_1972_counts["Gambles", 7, 'xyz', 'z'] = 11
T_1972_counts["Gambles", 8, 'xz', 'x'] = 11
T_1972_counts["Gambles", 8, 'xz', 'z'] = 9
T_1972_counts["Gambles", 8, 'yz', 'y'] = 14
T_1972_counts["Gambles", 8, 'yz', 'z'] = 6
T_1972_counts["Gambles", 8, 'xyz', 'x'] = 7
T_1972_counts["Gambles", 8, 'xyz', 'y'] = 10
T_1972_counts["Gambles", 8, 'xyz', 'z'] = 13

T_1972_counts["Applicants", 1, 'xz', 'x'] = 13
T_1972_counts["Applicants", 1, 'xz', 'z'] = 7
T_1972_counts["Applicants", 1, 'yz', 'y'] = 6
T_1972_counts["Applicants", 1, 'yz', 'z'] = 14
T_1972_counts["Applicants", 1, 'xyz', 'x'] = 11
T_1972_counts["Applicants", 1, 'xyz', 'y'] = 5
T_1972_counts["Applicants", 1, 'xyz', 'z'] = 14
T_1972_counts["Applicants", 2, 'xz', 'x'] = 11
T_1972_counts["Applicants", 2, 'xz', 'z'] = 9
T_1972_counts["Applicants", 2, 'yz', 'y'] = 15
T_1972_counts["Applicants", 2, 'yz', 'z'] = 5
T_1972_counts["Applicants", 2, 'xyz', 'x'] = 6
T_1972_counts["Applicants", 2, 'xyz', 'y'] = 14
T_1972_counts["Applicants", 2, 'xyz', 'z'] = 10
T_1972_counts["Applicants", 3, 'xz', 'x'] = 11
T_1972_counts["Applicants", 3, 'xz', 'z'] = 9
T_1972_counts["Applicants", 3, 'yz', 'y'] = 12
T_1972_counts["Applicants", 3, 'yz', 'z'] = 8
T_1972_counts["Applicants", 3, 'xyz', 'x'] = 8
T_1972_counts["Applicants", 3, 'xyz', 'y'] = 9
T_1972_counts["Applicants", 3, 'xyz', 'z'] = 13
T_1972_counts["Applicants", 4, 'xz', 'x'] = 8
T_1972_counts["Applicants", 4, 'xz', 'z'] = 12
T_1972_counts["Applicants", 4, 'yz', 'y'] = 8
T_1972_counts["Applicants", 4, 'yz', 'z'] = 12
T_1972_counts["Applicants", 4, 'xyz', 'x'] = 11
T_1972_counts["Applicants", 4, 'xyz', 'y'] = 6
T_1972_counts["Applicants", 4, 'xyz', 'z'] = 13
T_1972_counts["Applicants", 5, 'xz', 'x'] = 13
T_1972_counts["Applicants", 5, 'xz', 'z'] = 7
T_1972_counts["Applicants", 5, 'yz', 'y'] = 11
T_1972_counts["Applicants", 5, 'yz', 'z'] = 9
T_1972_counts["Applicants", 5, 'xyz', 'x'] = 10
T_1972_counts["Applicants", 5, 'xyz', 'y'] = 8
T_1972_counts["Applicants", 5, 'xyz', 'z'] = 12
T_1972_counts["Applicants", 6, 'xz', 'x'] = 11
T_1972_counts["Applicants", 6, 'xz', 'z'] = 9
T_1972_counts["Applicants", 6, 'yz', 'y'] = 8
T_1972_counts["Applicants", 6, 'yz', 'z'] = 12
T_1972_counts["Applicants", 6, 'xyz', 'x'] = 4
T_1972_counts["Applicants", 6, 'xyz', 'y'] = 10
T_1972_counts["Applicants", 6, 'xyz', 'z'] = 16
T_1972_counts["Applicants", 7, 'xz', 'x'] = 15
T_1972_counts["Applicants", 7, 'xz', 'z'] = 5
T_1972_counts["Applicants", 7, 'yz', 'y'] = 7
T_1972_counts["Applicants", 7, 'yz', 'z'] = 13
T_1972_counts["Applicants", 7, 'xyz', 'x'] = 20
T_1972_counts["Applicants", 7, 'xyz', 'y'] = 4
T_1972_counts["Applicants", 7, 'xyz', 'z'] = 6
T_1972_counts["Applicants", 8, 'xz', 'x'] = 11
T_1972_counts["Applicants", 8, 'xz', 'z'] = 9
T_1972_counts["Applicants", 8, 'yz', 'y'] = 8
T_1972_counts["Applicants", 8, 'yz', 'z'] = 12
T_1972_counts["Applicants", 8, 'xyz', 'x'] = 13
T_1972_counts["Applicants", 8, 'xyz', 'y'] = 5
T_1972_counts["Applicants", 8, 'xyz', 'z'] = 12

usethis::use_data(T_1972_counts, overwrite=TRUE)
