cardi0_v <- scan("data/text/cardi.txt", what = "character", sep = "\n")
cardi_v <- tolower(paste(cardi0_v, collapse = " "))
cardi_word_l <- strsplit(text_v, "\\W")
cardi_word_v <- unlist(cardi_word_l)
not_blanks_v <- which(cardi_word_v != "")
cardi_word_v <- cardi_word_v[not_blanks_v]
(length(unique(cardi_word_v))/length(cardi_word_v))*100
not_so_unique_v <- c(length(cardi_word_v), length(unique(cardi_word_v)))
lbls <- c("all words (214899)", "unique words (18773)")
pie(not_so_unique_v, labels = lbls, main = "Pie chart of not-so-unique words in Invasion of Privacy")
cardi_freqs_t <- table(cardi_word_v) # frequencies table
sorted_cardi_freqs_t <- sort(cardi_freqs_t, decreasing = TRUE)
sorted_rel_freqs_cardi_t <- 100*(sorted_cardi_freqs_t/length(cardi_word_v))
plot(
  sorted_rel_freqs_cardi_t[1:10], type = "b",
  xlab = "Top Ten Words in Invasion of Privacy", 
  ylab = "Percentage of Full Text", 
  xaxt = "n"
)
axis(
  1, 1:10,
  labels = names(sorted_rel_freqs_cardi_t [1:10])
)

n_time_v <- seq(from = 1, to = length(cardi_word_v))
i_varient_v <- rep(NA, length(n_time_v))
i_hits <- grep(
  "my|me",
  cardi_word_v)
i_varient_v[i_hits] <- 1

plot(
  i_varient_v,
  main ="Dispersion Plot of 'i' variants in Invasion of Privacy",
  xlab = "th album",
  ylab = "i, me, my, mine",
  type = "h",
  ylim = c(0,1),
  yaxt = 'n'
)
install.packages("tidyr")
library("tidyr")
summarize(i_varient_v)
