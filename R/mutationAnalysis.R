#' Description about this mutation
#'
#'

mutationPercentage <- function(mutationData){

  inter <- dplyr::distinct(mutationData,mutationData$icgc_donor_id, mutationData$gene_affected, .keep_all = TRUE)
  inter <- inter[,-c(length(inter),length(inter) - 1)]


  inter <- dplyr::add_count(inter, inter$gene_affected, name = "mutation_count")
  inter <- inter[,-(length(inter) - 1)]

  #Find number of samples
  num_samp <- dplyr::distinct(mutationData, mutationData$icgc_donor_id)
  number_of_sample <- nrow(num_samp)


  inter <- dplyr::distinct(inter, inter$gene_affected, .keep_all = TRUE)

  #percentage_of_sample_with_gene is the count of a specific gene mutated/number of donors
  inter <- dplyr::mutate(inter, percentage_of_sample_with_mutated_gene = (mutation_count/number_of_sample))

  #Set number of sample as a dataframe before inserting into result
  number_sample = data.frame(number_of_sample)
  colnames(number_sample) <- "number_of_sample"


  result <- c(inter[,c("gene_affected","mutation_count", "percentage_of_sample_with_mutated_gene")], number_sample)
  return(result)
}





mutationTypePlot <- function(mutationData){
  #leave only unique mutation id's
  inter <- dplyr::distinct(mutationData, mutationData$icgc_mutation_id,.keep_all = TRUE)
  inter <- inter[,-(length(inter))]

  inter <- dplyr::filter(inter, inter$consequence_type == "stop_gained" | inter$consequence_type == "missense_variant"| inter$consequence_type == "
synonymous_variant")
  inter <- dplyr::mutate(inter, substitution_type = stringr::str_extract(inter$cds_mutation, "[A-Z]>[A-Z]"))
  inter <- dplyr::add_count(inter, count = substitution_type)
  inter <- dplyr::distinct(inter, substitution_type, .keep_all = TRUE)

  bar_plot <- ggplot2::ggplot(inter, ggplot2::aes(x = substitution_type, y = n)) +
    ggplot2::geom_bar(stat = "identity", color = "black", fill ="white") +
    ggplot2::xlab("Substitution type") +
    ggplot2::ylab("Count")

  bar_plot <- bar_plot +
    ggplot2::geom_text(ggplot2::aes(label=n), vjust=2, color="black")


  pie_inter <- dplyr::add_count(mutationData, count = consequence_type)
  pie_inter <- dplyr::distinct(pie_inter, consequence_type, .keep_all = TRUE)
  pie_inter <- dplyr::mutate(pie_inter, concat_string = paste(consequence_type, n, sep = ", Count: "))

  #Pie chart for mutation types
  pie_chart <- ggplot2::ggplot(pie_inter, ggplot2::aes(x = "", y = n,fill = concat_string)) +
    ggplot2::geom_col(color = "black") +
    ggplot2::coord_polar(theta = "y") +
    #ggplot2::scale_fill_discrete(labels = pie_inter$concat_string)
    ggplot2::labs(title = "Pie chart of Consequence type of mutation") +
    ggplot2::xlab("") +
    ggplot2::ylab("")
  result = list(bar_plot, pie_chart)
  return(result)
}
