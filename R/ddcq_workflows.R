###############################################################################
#
# author: Christoph Kiefer
# email: christophak@bmb.sdu.dk
#
################################################################################
#'
#' Get delta Cq values
#'
#' @examples \dontrun{
#' analyse.cq(scheme, "./data-raw/example.txt",
#'   hkp = c("housekeeper_1", "housekeeper_2"), silent = TRUE)
#' }
#'
#' @export
analyse.dcq <- function(scheme, file, hkp, output = ".", silent = FALSE,
    decimal_mark = '.') {

    df <- import.LCcq(file, scheme, decimal_mark = decimal_mark)

    if(silent == FALSE) {
        for (i in 1:max(df$repl_biol)) {
            plot.cq(df, rep_biol = i)
            ggsave(paste0(output, "/cq_rep_", i, ".pdf"), height = 10, width = 16.18)
        }
    }

    df <- df %>%
        get.averageCq() %>%
        get.dCq(hkp = hkp) %>%
        get.averageDCq()

    if(silent == FALSE) {
        write_csv(df, paste0(output, "/dcq.csv"))

        plot.dCq(df)
        ggsave(paste0(output, "/dcq.pdf"), height = 10, width = 16.18)
    }

    return(df)
}

#'
#' Run the whole ddcq analysis
#'
#' @examples \dontrun{
#' analyse.ddcq(scheme, "./data-raw/example.txt",
#'   hkp = c("housekeeper_1", "housekeeper_2"), silent = TRUE)
#' }
#'
#' @export
analyse.ddcq <- function(scheme, file, hkp, output = ".", silent = FALSE,
    reference = scheme$cond[1], decimal_mark = '.') {

    df <- scheme %>%
        analyse.dcq(file, hkp, output, silent, decimal_mark = decimal_mark) %>%
        get.ddcq()

    if(silent == FALSE) {
        plot.ddCq(df)
        ggsave(paste0(output, "/ddcq.pdf"), height = 10, width = 16.18)

        write_csv(df, paste0(output, "/ddcq.csv"))
    }

    return(df)

}
