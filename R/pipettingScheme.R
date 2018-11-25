###############################################################################
#
# author: Christoph Kiefer
# email: christophak@bmb.sdu.dk
#
################################################################################
#'
#' Get A Tibble With A Pipetting Scheme For qPCR
#'
#' @param names_gene character vector containing names of the genes measured
#' @param names_cond character vector containing names of the conditions
#' @param rep_qpcr number of technical replicates in the qPCR per sample
#' @param rep_tech number of technical replicates e.g., in cell culture per condition of a biological replicate
#' @param rep_biol number of biological replicates
#' @param first_col first column with samples
#' @param nrow number of rows with samples
#'
#' @examples get.pipettingScheme(names_gene = c("Ucp1", "Gapdh"),
#'     names_cond = c("NTC", "CL", "iso"), rep_qpcr = 2,
#'     rep_tech = c(3,3,2), rep_biol = 2)
#'
#' @import dplyr
#'
#' @export
get.pipettingScheme <- function(names_gene, names_cond, rep_qpcr, rep_tech,
    rep_biol = 1, first_col = 1, nrow = 16) {

num_cond <- length(names_cond)
num_gene <- length(names_gene)

if (length(rep_tech) == 1) {
        rep_tech <- rep(rep(rep_tech, num_cond), rep_biol)
    } else if (length(rep_tech) == num_cond) {
        rep_tech <- rep(rep_tech, rep_biol)
    } else if (length(rep_tech) != num_cond * rep_biol) {
        stop("rep_tech must be either a single integer or a vector of the same
            length as there are conditions or of the same length as there are
            conditions * biological replicates.")
}

length_gene <- sum(rep_tech) * rep_qpcr
cols_gene <- ((length_gene - 1) %/% nrow) + 1
samples_per_column <- rep(c(rep(nrow, (length_gene %/% nrow)), length_gene %% nrow)[c(rep(nrow, (length_gene %/% nrow)),
    length_gene %% nrow) != 0], num_gene)

tibble(gene = rep(names_gene, each = length_gene),
        cond = as.factor(rep(rep(rep(names_cond, rep_biol), rep_qpcr * rep_tech), num_gene)),
        repl_biol = rep(rep(rep(1:rep_biol, each = num_cond), rep_qpcr * rep_tech), num_gene),
        repl_tech = rep(rep(unlist(lapply(rep_tech, seq)), each = rep_qpcr), num_gene),
        col = rep(1:(cols_gene * num_gene), samples_per_column),
        row = rep(1:length_gene %% nrow, num_gene)) %>%
    mutate(row = LETTERS[if_else(row == 0, nrow, row)],
        col = as.integer(col + first_col - 1))

}
