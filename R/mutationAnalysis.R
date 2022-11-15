#' Get count and percentage of gene's affected by mutation
#'
#' A function that analyzes the provided data by getting the
#' proportion and frequency of all gene mutations that
#' has had an effect on the sample donors.
#'
#' @param mutationData A data frame with columns icgc_donor_id,
#' and gene affected. The data frame should be in the same format as the icgc
#' databases DCC Data Release.
#'
#' @return Returns a data frame with results
#' \itemize{
#' \item gene_affected - Gene code for designated mutated gene in donors
#' \item mutation_count - The number of mutations observed in sample for
#' gene_affected
#' \item percentage_of_sample_with_mutated_gene - The percentage of the
#' frequency of observed mutated gene in sample
#' }
#'
#' @examples
#' # Example 1:
#' # Using icgc_data available inside package
#'
#' # Get genes mutation frequency, and percentage in sample
#' mutationPercentageResults <- mutationPercentage(icgc_data)
#' mutationPercentageResults
#'
#' @references
#' Hadley Wickham, Romain Francois, Lionel Henry and Kirill Muller
#' (2022). dplyr: A Grammar of Data Manipulation. R package version
#' 1.0.10. https://CRAN.R-project.org/package=dplyr
#'
#' Zhang J, Bajari R, Andric D, et al. The International
#' Cancer Genome Consortium Data Portal. Nat Biotechnol.
#' 2019;37(4):367‐369. doi:10.1038/s41587-019-0055-9
#'
#' @export
#' @import dplyr

mutationPercentage <- function(mutationData){

  #Performing checks on user input
  #Check if it is a data frame
  if(is.data.frame(mutationData) == FALSE){
    stop("Need to provide data frame as data")
  }

  #Check if it has column icgc_donor_id
  if(is.null(mutationData$icgc_donor_id)){
    stop("Data must have column icgc_donor_id")
  }

  #Check if it has column gene_affected
  if(is.null(mutationData$gene_affected)){
    stop("Data must have column gene_affected")
  }

  #Start Analysis


  inter <- dplyr::distinct(mutationData,mutationData$icgc_donor_id,
                           mutationData$gene_affected, .keep_all = TRUE)
  inter <- inter[,-c(length(inter),length(inter) - 1)]


  inter <- dplyr::add_count(inter, inter$gene_affected, name = "mutation_count")
  inter <- inter[,-(length(inter) - 1)]

  #Find number of samples
  num_samp <- dplyr::distinct(mutationData, mutationData$icgc_donor_id)
  number_of_sample <- nrow(num_samp)


  inter <- dplyr::distinct(inter, inter$gene_affected, .keep_all = TRUE)

  #percentage_of_sample_with_gene is the count
  #of a specific gene mutated/number of donors
  inter <- dplyr::mutate(inter, percentage_of_sample_with_mutated_gene =
                           (mutation_count/number_of_sample))

  #Set number of sample as a dataframe before inserting into result
  number_sample = data.frame(number_of_sample)
  colnames(number_sample) <- "number_of_sample"


  result <- inter[,c("gene_affected","mutation_count",
                     "percentage_of_sample_with_mutated_gene")]
  return(result)
}




#' Bar plot and pie chart for mutation type data samples
#'
#' A function that returns a list of two plots a bar plot and pie chart
#' for mutation type analysis
#'
#' @param mutationData A data frame with columns icgc_mutation_id,
#' cds_mutation, and consequence_type. The data frame should be in the same
#' format as the icgc databases DCC data release
#'
#' @return Returns a list of two plots.
#' \itemize{
#' \item mutation_types - A pie chart showing the count and mutation types
#' undergone in the sample
#' \item substitution_types - A bar plot showing the count and substitution
#' types undergone in the sample
#' }
#'
#' @examples
#' mutationTypePlotResults <- mutationTypePlot(icgc_data)
#' #To display each plot
#' mutationTypePlotResults["mutation_types"]
#' mutationTypePlotResults["substitution_types"]
#'
#' @references
#' Hadley Wickham, Romain Francois, Lionel Henry and Kirill Muller
#' (2022). dplyr: A Grammar of Data Manipulation. R package version
#' 1.0.10. https://CRAN.R-project.org/package=dplyr
#'
#' H. Wickham. ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New
#' York, 2016.
#'
#' Hadley Wickham (2022). stringr: Simple, Consistent Wrappers for Common
#' String Operations. R package version 1.4.1.
#' https://CRAN.R-project.org/package=stringr
#'
#' @export
#' @import dplyr
#' @import ggplot2
#' @importFrom stringr str_extract

mutationTypePlot <- function(mutationData){

  #Test if mutationData is in correct format
  #Check if it is a data frame
  if(is.data.frame(mutationData) == FALSE){
    stop("Need to provide data frame as data")
  }

  #Check if it has column cds_mutation
  if(is.null(mutationData$cds_mutation)){
    stop("Data must have column cds_mutation")
  }

  #Check if it has column icgc_mutation_id
  if(is.null(mutationData$icgc_mutation_id)){
    stop("Data must have column icgc_mutation_id")
  }

  #Check if it has column consequence_type
  if(is.null(mutationData$consequence_type)){
    stop("Data must have column consequency_type")
  }


  #Start Analysis

  #preallocate result object
  result <- NULL

  #leave only unique icgc_mutation_id
  inter <- dplyr::distinct(mutationData,
                           mutationData$icgc_mutation_id,.keep_all = TRUE)
  #Delete newly created column from previous code
  inter <- inter[,-(length(inter))]

  #Only filter for consequence_types that have cds code
  inter <- dplyr::filter(inter, inter$consequence_type == "stop_gained" |
  inter$consequence_type == "missense_variant"|
  inter$consequence_type == "
synonymous_variant")

  #Use str_extract function to extract nucleotide change string from cds code
  inter <- dplyr::mutate(inter, substitution_type = stringr::str_extract(
    inter$cds_mutation, "[A-Z]>[A-Z]"))

  #Get count of each string extracted from previous line
  inter <- dplyr::add_count(inter, count = substitution_type)

  #Only need the substitution types now filter
  inter <- dplyr::distinct(inter, substitution_type, .keep_all = TRUE)

  #Creating bar plot for substitution types
  bar_plot <- ggplot2::ggplot(inter, ggplot2::aes(x = substitution_type, y = n)) +
    ggplot2::geom_bar(stat = "identity", color = "black", fill ="white") +
    ggplot2::xlab("Substitution type") +
    ggplot2::ylab("Count") +
    ggplot2::labs(title = "Bar graph for substitution types")

  bar_plot <- bar_plot +
    ggplot2::geom_text(ggplot2::aes(label=n), vjust=2, color="black")


  #Formatting data into intermediate variable pie_inter to create pie chart
  pie_inter <- dplyr::add_count(mutationData, count = consequence_type)
  pie_inter <- dplyr::distinct(pie_inter, consequence_type, .keep_all = TRUE)
  pie_inter <- dplyr::mutate(pie_inter, concat_string = paste(consequence_type, n, sep = ", Count: "))

  #Pie chart for mutation types
  pie_chart <- ggplot2::ggplot(pie_inter, ggplot2::aes(x = "",
                                            y = n,fill = concat_string)) +
    ggplot2::geom_col(color = "black") +
    ggplot2::coord_polar(theta = "y") +
    ggplot2::labs(title = "Pie chart of Consequence type of mutation") +
    ggplot2::xlab("") +
    ggplot2::ylab("")

  #Combine pie chart and bar graph to be able to return on one object
  result = list(mutation_types = pie_chart, substitution_types = bar_plot)


  return(result)
}

#[END] Written by Jae Hyung Jung
