#' Summarize Count and Calculate Ratio, and Optionally Filter by Ratio
#'
#' This function takes a data frame and calculates the count of rows for each
#' user given group, then computes the ratio of the count to the total count across all groups.
#' The function also allows filtering the results based on a user-specified ratio threshold.
#'
#' @param data A data frame or tibble, typically grouped by one or more variables.
#' @param ... One or more column names (unquoted or quoted) to group by.
#' @param threshold Numeric value for filtering the ratio.
#'                        Default is 0.01
#'                        Only rows with `Ratio` greater than this value are returned.
#' @return A tibble with two columns:
#'   \item{Count}{The number of rows in each group.}
#'   \item{Sum}{The number of rows for the first grouping value.}
#'   \item{Ratio}{The ratio of the count to the total count of all groups.}
#' @import dplyr
#' @importFrom tibble tibble
#' @export
#' @examples
#' df <- data.frame(
#'   Category = c("A", "A", "B", "B", "C", "C"),
#'   Subcategory = c("X", "Y", "X", "Y", "X", "Y"),
#'   Value = c(1, 2, 3, 4, 5, 6)
#' )
#'
#' sum_rat(df, Category, Subcategory, threshold = 0.2)
#' sum_rat(df, "Category", "Subcategory", threshold = 0.2)
#'
sum_rat <- function(data, ..., threshold = 0.01) {
  Sum <- Ratio <- Count <- NULL
  groupings <- ensyms(...)

  # Ensure all groupings exist in the dataset
  missing <- setdiff(as.character(groupings), colnames(data))
  if (length(missing) >= 1) {
    stop(paste(missing, "is not a column name."))
  }

  sum_vals <- data %>%
    group_by(!!groupings[[1]]) %>%
    summarise(Sum = n())

  result <- data %>%
    group_by(!!!groupings) %>%
    summarise(Count = n()) %>% # Count the rows
    left_join(sum_vals, by = as.character(groupings[[1]])) %>% # join by the sum values of the first grouping
    mutate(Ratio = Count / Sum) %>%
    arrange(!!groupings[[1]], desc(Ratio)) # Order by first grouping, then descending Ratio

  # Filter by ratio if threshold is provided
  if (!is.null(threshold)) {
    result <- result %>% filter(Ratio > threshold)
  }

  return(result)
}


#' Create a Custom Color or Fill Scale for ggplot2
#'
#' This function converts a specified column in a data frame to a factor, assigns custom colors
#' to its unique levels, and returns either a `scale_colour_manual()` or `scale_fill_manual()`
#' object for use in ggplot2.
#'
#' @param data A data frame containing the column to be factored.
#' @param column_name A string specifying the column name to be factored.
#' @param custom_colors A vector of colors to be assigned to the factor levels.
#'                      The number of colors must be at least as many as the unique levels.
#' @param scale_type A string specifying the type of scale to return.
#'                   Accepts `"color"`, `"colour"`, or `"fill"`.
#'
#' @return A `scale_colour_manual()` or `scale_fill_manual()` object for ggplot2.
#' @importFrom ggplot2 scale_colour_manual scale_fill_manual
#' @export
#'
#' @examples
#' library(ggplot2)
#' df <- data.frame(Diff = c("Low", "Medium", "High"))
#' custom_colors <- c("#E5614CFF", "#8C57A2FF", "#358DB9FF")
#' col_scale <- create_col_scale(df, "Diff", custom_colors, "color")
#' ggplot(df, aes(x = Diff, y = 1, color = Diff)) +
#'   geom_point() +
#'   col_scale
#'
#' fill_scale <- create_col_scale(df, "Diff", custom_colors, "fill")
#' ggplot(df, aes(x = Diff, y = 1, fill = Diff)) +
#'   geom_bar(stat = "identity") +
#'   fill_scale
create_col_scale <- function(data, column_name, custom_colors, scale_type = "color") {
  data[[column_name]] <- factor(data[[column_name]], levels = unique(data[[column_name]]))
  factor_levels <- levels(data[[column_name]])

  if (length(custom_colors) < length(factor_levels)) {
    stop("Error: Not enough colors provided for the number of factor levels.")
  }

  names(custom_colors) <- factor_levels

  scale_type <- tolower(scale_type) # Normalize input

  if (scale_type %in% c("color", "colour")) {
    return(scale_colour_manual(name = column_name, values = custom_colors))
  } else if (scale_type == "fill") {
    return(scale_fill_manual(name = column_name, values = custom_colors))
  } else {
    stop('Error: Invalid scale_type. Use "color", "colour", or "fill".')
  }
}


#' Annotate Gene Table with Metadata
#'
#' This function retrieves gene annotations from a specified organism database based on gene IDs.
#'
#' @param gene_ids A vector with gene IDs
#' @param key The keytype (e.g. `ENSEMBL` or `SYMBOL`)
#' @param org_db The organism annotation database to use (e.g., `org.Hs.eg.db`, `org.Mm.eg.db`).
#' @param cols The column values to request from the org database
#' @return A data frame with gene annotations, including Ensembl ID, gene symbol, chromosomal location, gene name, and Entrez ID.
#' @export
annotate_genes <- function(gene_ids,
                           key = 'ENSEMBL',
                           org_db = org.Hs.eg.db::org.Hs.eg.db,
                           cols = c("ENSEMBL", "SYMBOL", "MAP", "GENENAME", "ENTREZID")) {
  AnnotationDbi::select(
    org_db,
    keys = gsub('\\.\\d+', '',gene_ids),
    columns = cols,
    keytype = key
  )
}



#' Print out template header for Rmarkdown
#'
#' This copies the header of a Rmarkdown to the system clipboard (for paste later)
#'
#' return Text
#' @importFrom clipr write_clip
#' @export
rmarkdown_header <- function(){

    paste0('---
title: "The title"
author: "David McGaughey"
date: "`r Sys.Date()`"
output:
 html_notebook:
  theme: flatly
  toc: true
  toc_float: true
  code_folding: hide
 html_document:
  theme: flatly
  toc: true
  toc_float: true
  code_folding: hide
---

',
getwd(),
'

```{r, include = FALSE}
knitr::opts_chunk$set(
  message = FALSE,  warning = FALSE,
  collapse = TRUE,
  fig.width = 12, fig.height = 8,
  comment = "#>",
        dpi=300
    )
```

```{r}

```

',
cat("Copied to clipboard")) |> write_clip()

}



#' Collapse Columns into Comma-Separated Strings
#'
#' This function takes a data frame / tibble and collapses each column into a
#' comma-separated unique string, omitting any NA values.
#'
#' @param data A data frame or tibble.
#' @param sep The separator to use when collapsing columns (default: ", ").
#' @return A data frame with each column collapsed into a single string.
#' @importFrom dplyr across everything summarise
#' @export
#'
#' @examples
#' df <- data.frame(
#'   col1 = c("a", "b", NA, "c"),
#'   col2 = c(1, NA, 3, 4),
#'   col3 = c(TRUE, FALSE, TRUE, NA)
#' )
#' collapse_columns(df)
#'
collapse_columns <- function(data, sep = ", ") {
  dplyr::summarise(data, dplyr::across(dplyr::everything(), ~ paste(unique(na.omit(.)), collapse = ", ")))
}




#' @title Calculate Gene by Group Mean Counts
#'
#' @description
#' This function takes a SummarizedExperiment object, a column name from the
#' colData(object) for grouping, and an optional assay name as input.
#' It then calculates the mean expression (often counts) for each gene, grouped
#' by the unique values in the specified grouping column.
#'
#' @param se_object A SummarizedExperiment object.
#' @param grouping_column A character string specifying the name of the column
#'                        in the colData of the SummarizedExperiment object
#'                        to use for grouping.
#' @param assay_name An optional character string specifying the name of the
#'                   assay to use for calculating means (e.g., "counts", "cpm",
#'                   "tpm", "rpkm"). If not provided, the first assay available
#'                   in the SummarizedExperiment object will be used. You
#'                   can also specify `deseq2_norm` to get the size factor
#'                   normalized counts.
#' @param log Default TRUE, will log2(assay + 1) scale the data
#'
#' @return A tibble where rows are genes and columns are the unique groups
#'         from the specified grouping_column, containing the mean values
#'         for each gene within each group from the specified assay.
#'
#' @details
#' The function first validates the input SummarizedExperiment object and
#' the existence of the grouping column. It then extracts the specified assay
#' data and the grouping factor. For each gene, it computes the mean of its
#' values across samples belonging to the same group.
#'
#' @examples
#' # This example requires the 'SummarizedExperiment' package
#' if (requireNamespace("SummarizedExperiment", quietly = TRUE) &&
#'     requireNamespace("Matrix", quietly = TRUE)) {
#'
#'   # Create a dummy SummarizedExperiment object for demonstration
#'   # Define assay data (e.g., counts, cpm)
#'   counts_data <- matrix(
#'     sample(1:100, 20 * 6, replace = TRUE),
#'     nrow = 20,
#'     ncol = 6,
#'     dimnames = list(paste0("Gene", 1:20), paste0("Sample", 1:6))
#'   )
#'   cpm_data <- matrix(
#'     round(runif(20 * 6, 0, 100), 2), # Dummy CPM values
#'     nrow = 20,
#'     ncol = 6,
#'     dimnames = list(paste0("Gene", 1:20), paste0("Sample", 1:6))
#'   )
#'
#'   # Define colData
#'   col_data <- S4Vectors::DataFrame(
#'     Group = factor(c("A", "A", "B", "B", "C", "C")),
#'     Batch = factor(c("X", "Y", "X", "Y", "X", "Y")),
#'     row.names = paste0("Sample", 1:6)
#'   )
#'
#'   # Create SummarizedExperiment object with multiple assays
#'   se <- SummarizedExperiment::SummarizedExperiment(
#'     assays = list(counts = counts_data, cpm = cpm_data),
#'     colData = col_data
#'   )
#'
#'   # Calculate mean counts by 'Group' using the 'counts' assay (explicitly)
#'   mean_counts_by_group <- calculate_gene_group_mean(se, "Group", assay_name = "counts")
#'   print(mean_counts_by_group[1:5, ])
#'
#'   # Calculate mean CPM by 'Batch' using the 'cpm' assay
#'   mean_cpm_by_batch <- calculate_gene_group_mean(se, "Batch", assay_name = "cpm")
#'   print(mean_cpm_by_batch[1:5, ])
#'
#'   # Calculate mean using the first assay if assay_name is not specified
#'   mean_default_assay <- calculate_gene_group_mean(se, "Group")
#'   print(mean_default_assay[1:5, ])
#' }
#'
#' @importFrom SummarizedExperiment assay colData assayNames
#' @importFrom S4Vectors DataFrame
#' @importFrom DESeq2 counts
#' @export
calculate_gene_group_mean <- function(se_object,
                                      grouping_column,
                                      assay_name = NULL,
                                      log = TRUE) {

  # 1. Input Validation
  if (!inherits(se_object, "SummarizedExperiment")) {
    stop("Input 'se_object' must be a SummarizedExperiment object.")
  }

  if (!grouping_column %in% names(SummarizedExperiment::colData(se_object))) {
    stop(paste0("Grouping column '", grouping_column, "' not found in colData."))
  }

  # 2. Extract assay data based on assay_name or default to first
  if (!is.null(assay_name)) {
    if (assay_name == 'deseq2_norm'){
      if (!inherits(se_object, "DESeqDataSet")) {
        stop("Input 'se_object' must be a DESeqDataSet to use deseq2_norm")
      }
      assay_data <- DESeq2::counts(se_object, normalized = TRUE)
    } else {
      if (!assay_name %in% SummarizedExperiment::assayNames(se_object)) {
        stop(paste0("Assay '", assay_name, "' not found in SummarizedExperiment object. Available assays: ",
                    paste(SummarizedExperiment::assayNames(se_object), collapse = ", ")))
      }
      assay_data <- SummarizedExperiment::assay(se_object, assay_name)
    }
  } else {
    # If assay_name is not specified, use the first assay
    if (length(SummarizedExperiment::assayNames(se_object)) == 0) {
      stop("No assays found in the SummarizedExperiment object.")
    }
    assay_data <- SummarizedExperiment::assay(se_object, 1) # Get the first assay
    message(paste0("No 'assay_name' specified. Using the first assay: '",
                   SummarizedExperiment::assayNames(se_object)[1], "'."))
  }

  # Ensure assay_data is numeric
  if (!is.numeric(assay_data)) {
    stop("Assay data must be numeric for mean calculation.")
  }

  # 3. Extract grouping information
  grouping_factor <- SummarizedExperiment::colData(se_object)[[grouping_column]]

  # Ensure grouping_factor is a factor for aggregate, or convert it
  if (!is.factor(grouping_factor)) {
    # also droplevels to avoid empty values
    grouping_factor <- droplevels(as.factor(grouping_factor))
  }

  # 4. Calculate means for each gene by group
  unique_groups <- levels(grouping_factor)
  mean_table_list <- list()

  for (group_name in unique_groups) {
    # Get indices of samples belonging to the current group
    group_indices <- which(grouping_factor == group_name)

    if (length(group_indices) > 0) {
      # Extract assay data for the current group
      group_assay_data <- assay_data[, group_indices, drop = FALSE]

      # Calculate row means (mean for each gene in this group)
      gene_means <- rowMeans(group_assay_data)
      mean_table_list[[group_name]] <- gene_means
    } else {
      # If a group has no samples, fill with NA for consistency
      mean_table_list[[group_name]] <- rep(NA, nrow(assay_data))
      names(mean_table_list[[group_name]]) <- rownames(assay_data)
    }
  }

  # 5. Combine results into a single matrix
  mean_values_table <- do.call(cbind, mean_table_list)

  # Ensure row names are gene names
  rownames(mean_values_table) <- rownames(assay_data)

  # 6. log2 scaling
  if (log){
    mean_values_table <- log2(mean_values_table + 1)
  }

  # Return the result
  return(mean_values_table)
}



#' Create a Volcano Plot
#'
#' This function generates a volcano plot from a data frame,
#' highlighting genes with a significant adjusted p-value and log2 fold change.
#' It can also label a specific set of user-provided genes or the top/bottom
#' most significant genes based on log fold change.
#'
#' @param df A data frame containing differential expression results.
#' @param title The title of the plot (default: "Volcano Plot").
#' @param pvalue The name of the column containing p-values (default: 'pvalue').
#' @param padj The name of the column containing adjusted p-values (default: 'padj').
#' @param logFC The name of the column containing log2 fold changes (default: 'log2FoldChange').
#' @param gene_list A character vector of genes to label on the plot. If not empty,
#' only these genes will be labeled. If empty, the top and bottom genes
#' will be labeled based on significance (default: '').
#' @param padj_cutoff A numeric value for the adjusted p-value cutoff (default: 0.01).
#' @param logFC_cutoff A numeric vector of length two for the log2 fold change cutoffs.
#' The first value is for the lower bound (downregulation), and the second is for the
#' upper bound (upregulation) (default: c(-1, 1)).
#' @param basemean_cutoff A numeric value for the minimum mean expression level (baseMean)
#' to be considered for labeling in the default mode (default: 500).
#' @param top_n_genes An integer for the number of top-upregulated genes to label
#' when `gene_list` is empty (default: 10).
#' @param bottom_n_genes An integer for the number of top-downregulated genes to label
#' when `gene_list` is empty (default: 10).
#'
#' @return A ggplot object representing the volcano plot.
#' @import ggplot2 cowplot ggrepel dplyr
#' @importFrom utils head tail
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming 'resLFC' is a differential expression results object
#'
#' # Use the default labeling (top/bottom significant genes)
#' volcano_maker(df = resLFC %>% as_tibble(rownames = 'Gene'))
#'
#' # Adjust all parameters for default labeling
#' volcano_maker(df = resLFC %>% as_tibble(rownames = 'Gene'),
#'               padj_cutoff = 0.05,
#'               logFC_cutoff = c(-2, 2),
#'               basemean_cutoff = 1000,
#'               top_n_genes = 5,
#'               bottom_n_genes = 5)
#'
#' # Use a specific gene list for labeling
#' my_genes <- c("GeneA", "GeneB", "GeneC")
#' volcano_maker(df = resLFC %>% as_tibble(rownames = 'Gene'), gene_list = my_genes)
#' }
volcano_maker <- function(df, title = "Volcano Plot", pvalue = 'pvalue', padj = 'padj',
                          logFC = 'log2FoldChange', gene_list = '', padj_cutoff = 0.05,
                          logFC_cutoff = c(-1, 1),
                          basemean_cutoff = 500, top_n_genes = 10, bottom_n_genes = 10) {
  padj_col <- logFC_col <- pvalue_col <- Gene <- Class <- baseMean <- NULL

  # Rename columns to standard names for internal consistency
  df <- df %>%
    dplyr::rename(
      pvalue_col = !!pvalue,
      padj_col = !!padj,
      logFC_col = !!logFC
    )

  df <- df[!is.na(df$pvalue_col), ]
  print(dim(df))

  df$Class <- 'Not Significant'
  df$Class[df$padj_col < padj_cutoff & (df$logFC_col < logFC_cutoff[1] |
                                          df$logFC_col > logFC_cutoff[2])] <- paste0("FDR < ", padj_cutoff)

  # Check if a Gene column exists
  if (!"Gene" %in% names(df)) {
    warning("No 'Gene' column found. Gene labels will be empty.")
    df$Gene <- ""
  }

  # Determine which genes to label based on gene_list
  if (length(gene_list) > 0 && !is.null(gene_list) && gene_list[1] != '') {
    # If gene_list is provided, filter the data for labeling to include only the user-specified genes
    label_data <- subset(df, df$Gene %in% gene_list)

  } else {
    # If gene_list is empty, use the previous logic to label top and bottom genes, now with adjustable parameters
    label_data <- bind_rows(
      subset(df, padj_col < padj_cutoff & logFC_col > logFC_cutoff[2] & baseMean > basemean_cutoff) %>%
        arrange(logFC_col) %>% utils::head(top_n_genes),

      subset(df, padj_col < padj_cutoff & logFC_col < logFC_cutoff[1] & baseMean > basemean_cutoff) %>%
        arrange(logFC_col) %>% utils::tail(bottom_n_genes)
    )
  }

  plot <- ggplot(data = df, aes(x = logFC_col, y = -log10(pvalue_col))) +
    geom_point(aes(colour = Class)) +
    scale_colour_manual(values = c("darkred", "grey")) +
    cowplot::theme_cowplot() +
    ggrepel::geom_label_repel(
      data = label_data,
      aes(label = as.character(Gene)),
      max.overlaps = Inf,
    ) +
    xlab('logFC') + ylab('-log10(p value)') +
    ggtitle(title) +
    cowplot::theme_cowplot() +
    geom_vline(aes(xintercept = logFC_cutoff[1]), linetype = "dotted") +
    geom_vline(aes(xintercept = logFC_cutoff[2]), linetype = "dotted")

  plot
}


