library("metafor")
library("weightr")
library("RoBMA")
library("dplyr")

# Meta-analysis data available from: https://raw.githubusercontent.com/Motivation-and-Behaviour/screen_umbrella/v1.0.4/data/Reviews.csv
reviews <- read.csv("data/Reviews.csv", header=TRUE)

# clean_studies.csv obtained by adding:
# write.csv(data, "clean_studies.csv", row.names=FALSE)
# to:
# R/clean_studies.R
# https://github.com/Motivation-and-Behaviour/screen_umbrella/blob/v1.0.4/R/clean_studies.R
studies_clean <- read.csv("data/clean_studies.csv")

# Effect IDs identified manually by adding:
# high_certainty <- joined_df[which(joined_df[["certainty"]] == "meets criteria"),]
# write.csv(high_certainty, "high_certainty.csv", row.names=FALSE)
# to:
# https://github.com/Motivation-and-Behaviour/screen_umbrella/blob/v1.0.4/R/combine_effects.R
# And matching the Authors, years of publication, effect size and confidence intervals with the eight highlighted effects in Sanders et al.
effect_ids <- c("8556_119", "42168_002", "47429_001", "47429_003", "47429_002", "47783_001", "50271_001", "60496_003")

highlighted_primaries <- studies_clean[which(studies_clean[["effect_size_id"]] %in% effect_ids),]

primaries_with_reviews <- reviews %>%
  rename(review_id = "Review.ID" ) %>%
  left_join(highlighted_primaries, by = "review_id") %>%
  filter(!is.na(effect_size_id))

# Run same meta-analysis found here: https://github.com/Motivation-and-Behaviour/screen_umbrella/blob/v1.0.4/R/run_metaanalysis.R
# and attempt to replicate, correcting for publication bias with PET-PEESE.

table_1 <- data.frame()


for (eid in effect_ids) {

  df <- studies_clean[studies_clean$effect_size_id == eid,]

  # From https://github.com/Motivation-and-Behaviour/screen_umbrella/blob/7ad51ffa696ed0476a3902c75dd391299d9533a9/R/run_metaanalysis.R#L15
  meta_out <- rma(
    data = df,
    measure = "COR",
    ri = r_estimate,
    ni = study_n,
    slab = study_name
  )

  dfz <- combine_data(r = df$r_estimate, n = df$study_n, study_names = df$study_name, transformation = "fishers_z")

  naive_est <- z2r(meta_out$b)

  alpha = .10
  fit_PET <- lm(y ~ se, weights = 1/se^2, data = dfz)
  estimate <- z2r(summary(fit_PET)$coefficients["(Intercept)", "Estimate"])
  ci <- confint(fit_PET, '(Intercept)', level=0.95)
  PET_p_value <- summary(fit_PET)$coefficients["(Intercept)", "Pr(>|t|)"]

  # Bartoš et al. (https://osf.io/uhaew): "...if the test for effect
  # size [is] significant,... fit and interpret the PEESE model":
  if (PET_p_value < alpha) {
    fit_PEESE <- lm(y ~ I(se^2), weights = 1/se^2, data = dfz)

    # Overwrite estimate and ci.
    estimate <- z2r(summary(fit_PEESE)$coefficients["(Intercept)", "Estimate"])
    ci <- confint(fit_PEESE, '(Intercept)', level=0.95)
  }

  replicated <- ci[1] < meta_out$b & meta_out$b < ci[2]

  author <- max(primaries_with_reviews$First.Author[primaries_with_reviews$effect_size_id == eid])
  year <- max(primaries_with_reviews$Year.of.Publication[primaries_with_reviews$effect_size_id == eid])
  row <- data.frame(author, year, round(meta_out$b, 2), round(ci[1], 2), round(ci[2], 2), estimate, replicated, eid)
  names(row)<-c("Lead.author", "Date", "r", "Corrected.CI.low", "Corrected.CI.high", "Corrected.estimate", "Replicated", "ID")
  table_1 <- rbind(table_1, row)
}

print(table_1)

estimate.abs.mean <- round(mean(abs(table_1$r)), 2)
corrected_estimate.abs.mean <- round(mean(abs(table_1$Corrected.estimate)), 2)

print(estimate.abs.mean)
print(corrected_estimate.abs.mean)

