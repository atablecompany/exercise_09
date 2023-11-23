Overlap = function(seq1, seq2){
  max_overlap <- 0
  len1 <- nchar(seq1)
  len2 <- nchar(seq2)
  for (i in 1:min(len1, len2)) {
    suffix <- substr(seq1, start = len1 - i + 1, stop = len1)
    prefix <- substr(seq2, start = 1, stop = i)
    if (suffix == prefix) {
      max_overlap <- i
    }
  }
  return(max_overlap)
}
seq2 = 'CATGC'
seq1 = 'CTAAGT'
print(Overlap(seq1,seq2))


OverlapMatrix = function(S){
  num_seqs <- length(S)
  overlap_matrix <- matrix(0, nrow = num_seqs, ncol = num_seqs)
  
  for (i in 1:num_seqs) {
    for (j in 1:num_seqs) {
      if (i != j) {
        overlap_matrix[i, j] <- Overlap(S[[i]], S[[j]])
      }
    }
  }
  return(overlap_matrix)
}
S = c('CATGC','CTAAGT','GCTA','TTCA','ATGCATC')
overlapMat = (OverlapMatrix(S))


GreedySuperstring = function(S){
  while (length(S) > 1){
    overlapMat = OverlapMatrix(S)
    seq1 = S[which(overlapMat == max(overlapMat), arr.ind = TRUE)][1]
    seq2 = S[which(overlapMat == max(overlapMat), arr.ind = TRUE)][2]
    seq = paste0(seq1,substr(seq2,max(overlapMat)+1,nchar(seq2)))
    S = S[ !S == seq1]
    S = S[ !S == seq2]
    S = append(S, seq)
  }
  return(S)
}
S = c('CATGC','CTAAGT','GCTA','TTCA','ATGCATC')
print(GreedySuperstring(S))
