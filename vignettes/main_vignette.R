## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = " "
)
library(RanCh)
library(klaR)
library(HDInterval)
library(tidyverse)

## ----counts_T_1972------------------------------------------------------------
print(T_1972_counts['Gambles', 1, c('xz', 'yz', 'xyz'), ], na.print='-')

## ----counts_RDS_2011----------------------------------------------------------
print(RDS_2011_counts['Cash 1', 5, c(3, 5, 6, 9, 10, 12, 17, 18, 20, 24), ], na.print='-')
n_objects = 5
print(RDS_2011_counts['Cash 1', n_objects, doubletons[1:choose(n_objects, 2)], ], na.print='-')

## ----trials-------------------------------------------------------------------
head(MMS_2019_trials[c('domain', 'subject', 'trial', 'set', 'choice', 'set_perm', 'set_index', 'choice_int')], 5)

## ----RP-----------------------------------------------------------------------
MMS_2019_trials %>% select(set, choice, matches("^[a-e]{2}$")) %>% head(5)

## ----RP_plus------------------------------------------------------------------
MMS_2019_trials %>% filter(domain=="Beer") %>% select(matches("^[a-e]{2}$")) %>% colSums

## ----counts-------------------------------------------------------------------
N_bce <- marginalize(MMS_2019_counts['Beer', , ], c(2, 3, 5))
print(N_bce, na.print='-')

## ----demographic--------------------------------------------------------------
head(MMS_2019_demographics, 5)

## ----compare.waves------------------------------------------------------------
my.chisq.test <- function(A)
{
  x <- na.omit(t(A))
  if (nrow(x) > 1) {
    chi2 <- chisq.test(x)
    chi2$p.value
  } else NA
}
M <- apply(MG_2019_counts, c(1, 3), my.chisq.test)
hist(M, 20)

## ----write_RCS----------------------------------------------------------------
P_bce <- proportions(N_bce)  # Compute proportions from count data
print(P_bce, na.print='-')   # and display.

## ----plot_RCS-----------------------------------------------------------------
triplot(label=c('b', 'c', 'e')) # Set up ternary plot, with labels and grid
plot_P3(P_bce)                  # Plot points 

## ----regularity---------------------------------------------------------------
triplot(label=c('b', 'c', 'e')) # Set up ternary plot, with labels and grid
plot_P3(P_bce)                  # Plot points 
polygon(tritrafo(regularity_X3(P_bce)), border='blue')
polygon(tritrafo(multiplicative_X3(P_bce)), border='red')

## ----check regularity bce-----------------------------------------------------
print(regularity(P_bce))

## ----check regularity xyz-----------------------------------------------------
P_xyz <- P_Luce(c(2, 3, 5))
print(P_xyz, na.print='-')
print(regularity(P_xyz))

## ----similarity---------------------------------------------------------------
S <- similarity_X3(pxz = 0.6, pyz = 0.4)
triplot(label=c('x', 'y', 'z')) # Set up ternary plot, with labels and grid
polygon(tritrafo(S$So), col=grey(0.95)); text(tritrafo(colMeans(S$So)), 'So')
polygon(tritrafo(S$Sx), col=grey(0.9)); text(tritrafo(colMeans(S$Sx)), 'Sx')
polygon(tritrafo(S$Sy), col=grey(0.9)); text(tritrafo(colMeans(S$Sy)), 'Sy')
polygon(tritrafo(S$Sxy), col=grey(0.8)); text(tritrafo(colMeans(S$Sxy)), 'Sxy')

## ----similarity_rotate--------------------------------------------------------
triplot(label=c('a', 'b', 'c')) # Set up ternary plot, with labels and grid
Sacb <- S$Sxyz[, c(1, 3, 2)]
polygon(tritrafo(Sacb), col=grey(0.95))
text(tritrafo(colMeans(Sacb)), 'Sacb')

## ----compromise---------------------------------------------------------------
C <- compromise_X3(pyx = 0.6, pyz = 0.4)
triplot(label=c('x', 'y', 'z')) # Set up ternary plot
polygon(tritrafo(C$Co), col=grey(0.95)); text(tritrafo(colMeans(C$Co)), 'Co')
polygon(tritrafo(C$Cx), col=grey(0.9)); text(tritrafo(colMeans(C$Cx)), 'Cx')
polygon(tritrafo(C$Cz), col=grey(0.9)); text(tritrafo(colMeans(C$Cz)), 'Cz')
polygon(tritrafo(C$Cxz), col=grey(0.8)); text(tritrafo(colMeans(C$Cxz)), 'Cxz')

## ----check_sim_comp-----------------------------------------------------------
n <- 500
filt1 <- vector('logical', n); filt2 <- vector('logical', n)
p <- rDirichlet(n, c(1, 1, 1)) # Uniform distribution on 2-simplex
for (i in 1:n) {
  P <- create_P3(0.4, 0.4, 0.6, p[i,1], p[i,2])
  filt1[i] <- similarity(P, target=1, decoy=2, competitor=3, two_sided=TRUE)
  filt2[i] <- compromise(P, target=2, decoy=3, competitor=1, two_sided=TRUE)
}
triplot(label=c('x', 'y', 'z'))
points(tritrafo(p[filt1, ]), pch=20)
triplot(label=c('x', 'y', 'z'))
points(tritrafo(p[filt2, ]), pch=20)

## ----HPD----------------------------------------------------------------------
library(klaR)
prior_Alpha <- DirRC_constant_sum(ncol(N_bce), 2.0)
post_Alpha <- prior_Alpha + N_bce
HD3 <- Dir2_3_HD_region(post_Alpha, 0.9, c(1,2,3))
triplot(label=c('b', 'c', 'e'))               # Set up ternary plot
lines(tritrafo(HD3$HD12), lwd=4)              # Plot three binaries
lines(tritrafo(HD3$HD23), lwd=4)
lines(tritrafo(HD3$HD13), lwd=4)
polygon(tritrafo(HD3$HD123), border='lightgreen') # Plot ternary
plot_P3(P_bce)                           # Plot proportions from data

## ----logML--------------------------------------------------------------------
N = MMS_2019_counts['Colours',,]
n_objects = ncol(N)

# Zero parameter models
print(dmultinomRC(P_uniform(n_objects), N, categorical=TRUE))
Alpha <- DirRC_constant_shape(n_objects, 1.0)
print(dDirMultinomRC(Alpha, N, categorical=TRUE, log=TRUE))

# One parameter models
alpha <- 2.0
Alpha <- DirRC_constant_sum(n_objects, 2.0)
print(dDirMultinomRC(Alpha, N, categorical=TRUE, log=TRUE))

# n-parameter models
Alpha <- DirRC_constant_sum(c(1.0, 2.0, 1.0, 2.0, 1.0), 4.0)
print(dDirMultinomRC(Alpha, N, categorical=TRUE, log=TRUE))

