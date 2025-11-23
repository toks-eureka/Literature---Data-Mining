library(dplyr)
library(stringr)

# ============================
# 0. LOAD DATA
# ============================
df <- read.csv("C:/Users/Owner/Documents/books_1.Best_Books_Ever.csv")

# has_award = 1 if awards column contains any letter
df <- df %>%
  mutate(
    has_award = ifelse(grepl("[A-Za-z]", awards), 1, 0),
    has_award = factor(has_award, levels = c(0, 1))
  )

# ============================
# 1. MULTI-GENRE DUMMIES
# ============================
top_genres <- c(
  "fiction","adult","romance","fantasy","historical","contemporary",
  "literature","young","mystery","science","novels","paranormal",
  "nonfiction","thriller","audiobook","classics","adventure",
  "history","childrens","biography"
)

df2 <- df %>%
  mutate(genres_clean = str_to_lower(genres))

for (g in top_genres) {
  df2[[g]] <- as.integer(str_detect(df2$genres_clean, g))
}

# "other" dummy if none of the top genres matched (not used in model)
df2$other <- ifelse(rowSums(df2[top_genres]) == 0, 1, 0)

# ============================
# 2. BUILD MODEL DATASET
# ============================
predictors <- c("rating", "numRatings", "likedPercent", top_genres)

df_model <- df2 %>%
  select(has_award, all_of(predictors)) %>%
  # make numeric predictors numeric
  mutate(
    rating       = as.numeric(rating),
    numRatings   = as.numeric(numRatings),
    likedPercent = as.numeric(likedPercent)
  ) %>%
  # drop rows with any NA in variables we use
  tidyr::drop_na()

# quick sanity check
# str(df_model)
# table(df_model$has_award)

# ============================
# 3. TRAIN / VALID SPLIT (75/25)
# ============================
set.seed(123)
n <- nrow(df_model)
train_idx <- sample(seq_len(n), size = floor(0.75 * n))

train <- df_model[train_idx, ]
valid <- df_model[-train_idx, ]

# ============================
# 4. LOGISTIC REGRESSION
#    (other is implicit reference)
# ============================
dummy_vars <- top_genres

formula_logit <- as.formula(
  paste(
    # scale() does the normalization for numeric predictors
    "has_award ~ scale(rating) + scale(numRatings) + scale(likedPercent) +",
    paste(dummy_vars, collapse = " + ")
  )
)

model_logit <- glm(
  formula_logit,
  data   = train,
  family = binomial
)

summary(model_logit)

# ============================
# 5. VALIDATION PREDICTIONS &
#    CONFUSION MATRIX
# ============================
valid_pred <- valid %>%
  mutate(
    pred_prob  = predict(model_logit, newdata = valid, type = "response"),
    pred_class = ifelse(pred_prob >= 0.5, 1, 0),
    has_award_factor = factor(has_award, levels = c(0, 1)),
    pred_factor      = factor(pred_class, levels = c(0, 1))
  )

conf_mat <- table(
  Actual    = valid_pred$has_award_factor,
  Predicted = valid_pred$pred_factor
)

conf_mat

accuracy    <- sum(diag(conf_mat)) / sum(conf_mat)
sensitivity <- conf_mat["1", "1"] / sum(conf_mat["1", ])
specificity <- conf_mat["0", "0"] / sum(conf_mat["0", ])

accuracy
sensitivity
specificity
