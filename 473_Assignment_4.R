library(stringr)
sequence <- readLines("C:/Users/chemi/OneDrive/Programming/chr1_GL383518v1_alt.fa")
sequence <- paste(sequence, collapse ="")
cat("10th Letter:", substr(sequence, 30, 30), "\n")
cat("10th Letter:", substr(sequence, 778, 778), "\n")
reverse_comp <- function(seq){
  complement <- c(A = "T", T = "A", C = "G", G = "C")
  seq_comp <- sapply(strsplit(seq, NULL)[[1]], function(base) complement[base])
  return(paste(rev(seq_comp), collapse = ""))
}
cat("79th Letter:", substr(rev_comp_seq, 99, 99))
cat("500th Letter:", substr(rev_comp_seq, 520, 520))
count_nuc <- function(sequence_chunk) {
  count_A <- sum(str_count(sequence_chunk, "A"))
  count_T <- sum(str_count(sequence_chunk, "T"))
  count_C <- sum(str_count(sequence_chunk, "C"))
  count_G <- sum(str_count(sequence_chunk, "G"))
  return(c(A = count_A, T = count_T, C = count_C, G = count_G))
}
kilobase_size <- 1000
kilo_counts <- list()
seq_length <- nchar(sequence)
num_kilobases <- ceiling(seq_length / kilobase_size)
for (i in 1:num_kilobases){
  start_pos <- (i - 1) * kilobase_size + 1
  end_pos <- min(i *kilobase_size, seq_length)
  kilobase_sequence <- substr(sequence, start_pos, end_pos)
  kilo_counts[[i]] <- count_nuc(kilobase_sequence)
}
kilo_counts[1:5]
kilobase_df <- do.call(rbind, kilo_counts)
first_kilo_counts <- kilo_counts[[1]]
first_kilo_df <- data.frame
first_kilo_df <- data.frame(A = first_kilo_counts["A"], T = first_kilo_counts["T"], C = first_kilo_counts["C"],G = first_kilo_counts["G"])
first_kilo_df
kilobase_df <- as.data.frame(kilobase_df)
colnames(kilobase_df) <- c("A", "T", "C", "G")
row_sums <- rowSums(kilobase_df)
kilobase_df$row_sum <- row_sums
kilobase_df
expected <- kilobase_size
deviations <- kilobase_df$row_sum != expected
if(any(deviations)){
  print("Kilobases with deviations from the expected sum:")
  print(kilobase_df[deviations, ])
} else{
  print("All kilobases have the expected sum")
}
#What is the expected sum for each list?
#The expected sum for each list should be 1000 since a kilbase should mean 1000 base pairs and the total should be up to 1000
#Are there any lists whose sums are not equal to the expected value?
#Most of the lists were not equal to the expected values.
#Provide a general explanation for the differences in your expected results and your observed results.
#Some of the lines could be misread by the program and don't fit with what is considered a standard base or the line is shorter than 1000 bases