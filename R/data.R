#'raw Acute Myeloid Leukemia Target US open somatic mutation data from
#'ICGC DCC database
#'
#'Data of AML United States cancer patients updated from the ICGC DATA PORTAL.
#'File name is simple_somatic_mutation.open.AML-US.tsv.gz extracted from portal.
#'
#'@source \url{https://dcc.icgc.org/releases/current/Projects/AML-US}
#'
#'@format A data frame with columns:
#'\describe{
#'\item{icgc_mutation_id}{ unique mutation id to gene_affected
#'and transcript_affected}
#'\item{icgc_donor_id}{ unique donor id}
#'\item{project_code}{ code to denote id of cancer project}
#'\item{icgc_specimen_id}{ unique id for specimen handled}
#'\item{icgc_sample_id}{ unique id for sample}
#'\item{matched_icgc_sample_id}{ matched icgc sample id}
#'\item{submitted_sample_id}{ submitted sample id}
#'\item{submitted_matched_sample_id}{ submitted matched sample id}
#'\item{chromosome}{ the chromosome number the gene_affected is on}
#'\item{chromosome_start}{ chromosome start position}
#'\item{chromosome_end}{ chromosome end position}
#'\item{chromosome_strand}{ chromosome strand number}
#'\item{assembly_version}{ the human assembly_version}
#'\item{mutation_type}{ mutation type}
#'\item{reference_genome_allele}{ The reference genome allele for gene_affected}
#'\item{mutated_from_allele}{ The mutated from allele for gene_affected}
#'\item{mutated_to_allele}{ The mutated to allele for gene_affected}
#'\item{quality_score}{ quality score}
#'\item{probability}{ probability}
#'\item{total_read_count}{ total read count of sample}
#'\item{mutant_allele_read_count}{ mutation allele read count}
#'\item{verification_status}{ verification status of donor}
#'\item{verification_platform}{ verification method of donor's data}
#'\item{biological_validation_status}{ biological validation status}
#'\item{biological_validation_platform}{ biological validation platform}
#'\item{consequence_type}{ type of mutation consequence type on gene_affected}
#'\item{aa_mutation}{ The type of amino acid change that has occurred}
#'\item{cds_mutation}{ cds mutation code}
#'\item{gene_affected}{ Ensembl code for gene}
#'\item{transcript_affected}{ Ensembl code for transcript}
#'\item{gene_build_version}{ gene build version}
#'\item{platform}{ platform the sample was collected}
#'\item{experimental_protocol}{ experimental protocol url}
#'\item{sequencing_strategy}{ the sequencing strategy used}
#'\item{base_calling_algorithm}{ base calling algorithm url}
#'\item{alignment_algorithm}{ alignment algorithm url}
#'\item{variation_calling_algorithm}{ variation calling algorithm url}
#'\item{other_analysis_algorithm}{ other analysis algorithm used}
#'\item{seq_coverage}{ sequence coverage}
#'\item{raw_data_repository}{ the raw data repository for data}
#'\item{raw_data_accession}{ the raw data accession code}
#'\item{initial_data_release_date}{ inital data release date}
#'}
#'
#'@examples
#'\dontrun{
#' icgc_data
#'}
"icgc_data"

#'Rhabdoid Tumors Target US open somatic mutation data from
#'ICGC DCC database
#'
#'Data of Rhabdoid Tumors in United States cancer patients updated from the
#'ICGC DATA PORTAL. File name is
#'simple_somatic_mutation.open.RT-US.tsv.gz extracted from portal.
#'
#'@source \url{https://dcc.icgc.org/releases/current/Projects/RT-US}
#'
#'@format A data frame with columns:
#'\describe{
#'\item{icgc_mutation_id}{ unique mutation id to gene_affected
#'and transcript_affected}
#'\item{icgc_donor_id}{ unique donor id}
#'\item{project_code}{ code to denote id of cancer project}
#'\item{icgc_specimen_id}{ unique id for specimen handled}
#'\item{icgc_sample_id}{ unique id for sample}
#'\item{matched_icgc_sample_id}{ matched icgc sample id}
#'\item{submitted_sample_id}{ submitted sample id}
#'\item{submitted_matched_sample_id}{ submitted matched sample id}
#'\item{chromosome}{ the chromosome number the gene_affected is on}
#'\item{chromosome_start}{ chromosome start position}
#'\item{chromosome_end}{ chromosome end position}
#'\item{chromosome_strand}{ chromosome strand number}
#'\item{assembly_version}{ the human assembly_version}
#'\item{mutation_type}{ mutation type}
#'\item{reference_genome_allele}{ The reference genome allele for gene_affected}
#'\item{mutated_from_allele}{ The mutated from allele for gene_affected}
#'\item{mutated_to_allele}{ The mutated to allele for gene_affected}
#'\item{quality_score}{ quality score}
#'\item{probability}{ probability}
#'\item{total_read_count}{ total read count of sample}
#'\item{mutant_allele_read_count}{ mutation allele read count}
#'\item{verification_status}{ verification status of donor}
#'\item{verification_platform}{ verification method of donor's data}
#'\item{biological_validation_status}{ biological validation status}
#'\item{biological_validation_platform}{ biological validation platform}
#'\item{consequence_type}{ type of mutation consequence type on gene_affected}
#'\item{aa_mutation}{ The type of amino acid change that has occurred}
#'\item{cds_mutation}{ cds mutation code}
#'\item{gene_affected}{ Ensembl code for gene}
#'\item{transcript_affected}{ Ensembl code for transcript}
#'\item{gene_build_version}{ gene build version}
#'\item{platform}{ platform the sample was collected}
#'\item{experimental_protocol}{ experimental protocol url}
#'\item{sequencing_strategy}{ the sequencing strategy used}
#'\item{base_calling_algorithm}{ base calling algorithm url}
#'\item{alignment_algorithm}{ alignment algorithm url}
#'\item{variation_calling_algorithm}{ variation calling algorithm url}
#'\item{other_analysis_algorithm}{ other analysis algorithm used}
#'\item{seq_coverage}{ sequence coverage}
#'\item{raw_data_repository}{ the raw data repository for data}
#'\item{raw_data_accession}{ the raw data accession code}
#'\item{initial_data_release_date}{ inital data release date}
#'}
#'
#'@examples
#'\dontrun{
#' icgc_data_2
#'}
"icgc_data_2"


#[END] written by Jae Hyung Jung
