#' Filters out genes with low counts based on CPM threshold
#'
#' @param data counts-based dataframe of gene expression (genes x samples),
#' gene names in rownames
#' @param one_cpm_to_counts scale 1 cpm to default 10 counts
#' @param first_col number of first column with counts if other columns precede
#' columns of samples
#'
#' @return returns filtered dataframe
#'
#' @examples
filter_low_counts <- function(data, one_cpm_to_counts = 10, first_col = 1){
    require(edgeR)
    
    one_cpm_threshold <- 10/(mean(colSums(data[, first_col:ncol(data)])) / 1e6) #cutoff for mean cpm expression
    #threshold is scaled so that 1 cpm = 10 counts
    
    cpm_data <- cpm(data[, first_col:ncol(data)], log = FALSE)
    
    row_means <- rowMeans(cpm_data)
    low_counts_genes <- rownames(cpm_data[row_means < one_cpm_threshold, ])
    cpm_data <- cpm_data[rownames(cpm_data) %!in% low_counts_genes, ]
    
    data <- data[rownames(cpm_data), ]
    
    return(data)
}

