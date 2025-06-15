#' Plot a Random Choice Structure in barycentric coordinates
#'
#' \code{plot_P3} plots four points specifying a Random Choice Structure for
#' a universe of three objects.
#' @param P A random choice structure for a universe of three objects
#' @param perm A permutation of \eqn{(1, 2, 3)} specifying which objects in the universe
#' correspond to the bottom left, top, and bottom right vertex, respectively of
#' the ternary plot.
#' @param binary_pch Plotting character (\code{pch}) for binary choice probabilities. Defaults
#' to a hollow circle.
#' @param ternary_pch Plotting character (\code{pch}) for ternary choice probability. Defaults
#' to a solid circle. The convention established with the defaults for binary_pch and
#' ternary_pch allow one to distinguish between a binary choice probability and a ternary
#' choice probability that happens to be on the boundary of the triangle.
#' @importFrom klaR tripoints
#' @export
#' @examples
#' P <- create_P3(0.7, 0.6, 0.8, 0.6, 0.3, names = c('x', 'y', 'z'))
#' plot_P3(P)
plot_P3 <- function(P, perm=c(1, 2, 3), binary_pch = 1, ternary_pch = 20) {
  # Object and subset shortcuts
  x <- perm[1]; y <- perm[2]; z <- perm[3]
  xy <- set_index(c(x, y))
  yz <- set_index(c(y, z))
  xz <- set_index(c(x, z))
  xyz <- set_index(c(x, y, z))

  # Sides of triangle
  tripoints(P[xy, x], P[xy, y], 0.0, pch=binary_pch)
  tripoints(0.0, P[yz, y], P[yz, z], pch=binary_pch)
  tripoints(P[xz, x], 0.0, P[xz, z], pch=binary_pch)

  # Centre of triangle
  tripoints(P[xyz, x], P[xyz, y], P[xyz, z], pch=ternary_pch)
}

plot_axioms = function(P,
                       do.reg=TRUE, do.mul=TRUE,
                       do.ternary=TRUE, do.context=FALSE,
                       between=2,
                       grid=seq(0.2, 0.8, by=0.2),
                       binary.pch=1, ternary.pch=20,
                       reg.col='blue', mul.col='red',
                       one.ce.color = '#F0F0F0', both.ce.color = '#E0E0E0',
                       border=NA,
                       panel.label=NULL) {

  label <- colnames(P)
  pxy <- P[3, 1]; pyx <- P[3, 2]
  pxz <- P[5, 1]; pzx <- P[5, 3]
  pyz <- P[6, 2]; pzy <- P[6, 3]
  PTx <- P[7, 1]; PTy <- P[7, 2]; PTz <- P[7, 3]

  # Set up empty plot with no frame (with grid that will be overwritten)
  tripl = klaR::triplot(frame=FALSE, grid=grid)
  text(0, -0.4, panel.label, pos=1)

  # Set up context effect region based on object whose tripleton probability is favoured
  if (between==1) pc = c(pxy, pxz) # c=x, a=y, b=z
  if (between==2) pc = c(pyz, pyx) # c=y, a=z, b=x
  if (between==3) pc = c(pzx, pzy) # c=z, a=x, b=y

  if (do.context) {
    # Generic computation for context effect regions
    pca = pc[1]
    pac = 1-pca
    pcb = pc[2]
    pbc = 1-pcb
    pac = 1-pca
    pbc = 1-pcb
    den = pac*pcb + pca*pbc + pca*pcb
    r1a = c(1, pac*pcb/den, pac, 1) # Region 1, context effect in one direction only
    r1b = c(0, pca*pbc/den, 0, 0)
    r1c = c(0, pca*pcb/den, pca, 0)
    r2a = c(0, pac*pcb/den, 0, 0)   # Region 2, context effect in other direction only
    r2b = c(1, pca*pbc/den, pbc, 1)
    r2c = c(0, pca*pcb/den, pcb, 0)
    r3a = c(pac, pac*pcb/den, 0, 0, pac) # Region 3, context effect in both directions
    r3b = c(0, pca*pbc/den, pbc, 0, 0)
    r3c = c(pca, pca*pcb/den, pcb, 1, pca)

    # Plotting, according to between/dissimilar object
    if (between==1) {
      polygon(klaR::tritrafo(r1c, r1a, r1b), col=one.ce.color, border=border)
      polygon(klaR::tritrafo(r2c, r2a, r2b), col=one.ce.color, border=border)
      polygon(klaR::tritrafo(r3c, r3a, r3b), col=both.ce.color, border=border)
    }
    if (between==2) {
      polygon(klaR::tritrafo(r1b, r1c, r1a), col=one.ce.color, border=border)
      polygon(klaR::tritrafo(r2b, r2c, r2a), col=one.ce.color, border=border)
      polygon(klaR::tritrafo(r3b, r3c, r3a), col=both.ce.color, border=border)
    }
    if (between==3) {
      polygon(klaR::tritrafo(r1a, r1b, r1c), col=one.ce.color, border=border)
      polygon(klaR::tritrafo(r2a, r2b, r2c), col=one.ce.color, border=border)
      polygon(klaR::tritrafo(r3a, r3b, r3c), col=both.ce.color, border=border)
    }
  }

  klaR::triframe(label=label)
  klaR::trigrid(grid)

  # Sides of triangle
  klaR::tripoints(pxy, pyx, 0.0, pch=binary.pch)
  klaR::tripoints(0.0, pyz, pzy, pch=binary.pch)
  klaR::tripoints(pxz, 0.0, pzx, pch=binary.pch)

  # Centre of triangle
  if (do.ternary) klaR::tripoints(PTx, PTy, PTz, pch=ternary.pch)

  # Regularity and multiplicative regions
  pmin.x = pxy*pxz
  pmax.x = min(pxy, pxz)
  pmin.y = pyx*pyz
  pmax.y = min(pyx, pyz)
  pmin.z = pzx*pzy
  pmax.z = min(pzx, pzy)

  cycle = pxy + pyz + pzx
  if (do.reg && cycle <= 2 && cycle >= 1) {
    klaR::trilines(c(pmax.x, pmax.x, 1-pmax.y-pmax.z, pmax.x),
                   c(pmax.y, 1-pmax.x-pmax.z, pmax.y, pmax.y),
                   c(1-pmax.x-pmax.y, pmax.z, pmax.z, 1-pmax.x-pmax.y),
                   col=reg.col)
  }
  if (do.mul) {
    klaR::trilines(c(pmin.x, pmin.x, 1-pmin.y-pmin.z, pmin.x),
                   c(pmin.y, 1-pmin.x-pmin.z, pmin.y, pmin.y),
                   c(1-pmin.x-pmin.y, pmin.z, pmin.z, 1-pmin.x-pmin.y),
                   col=mul.col)
  }
  tripl
}

#' Simplex plot for a 3-object random choice structure, in standard orientation
#'
#' @param P3 random choice structure
#'
#' @returns Invisibly returns \code{NULL}. Called for its side effects.
#' @export
#'
#' @examples
bin_tern_MR_plot <- function(P3) {
  # Permute P3 matrix so that ternary probabilities are decreasing
  P3 <- permute_A_x_matrix(P3, order(P3[7,]))
  plot_axioms(P3)
}

#' Array of simplex plots for all tripletons in a 5-object random choice
#' structure, all in standard orientation.
#'
#' @param N A count matrix for a discrete choice experiment
#'
#' @returns Invisibly returns \code{NULL}. Called for its side effects.
#' @export
#'
#' @examples
bin_term_MR_plot_all <- function(N) {
  P <- proportions(N)
  op = par(mfrow=c(4, 3), mar = c(1, 1, 0.5, 0.5))
  for (i_triple in 1:n_tripletons[5]) {
    triple <- tripletons[i_triple]
    triple_v <- subset_vectors[[triple]]
    P3 <- marginalize(P, triple_v)
    bin_tern_MR_plot(P3)
  }
  par(op)
}
