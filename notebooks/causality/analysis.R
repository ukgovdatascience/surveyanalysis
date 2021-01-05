library(readr)
library(dplyr)

# ensure you run `direnv allow` in terminal
readRenviron(path = ".env")
dir_processed <- Sys.getenv(x = "DIR_DATA_PROCESSED")
df_names <- c("df_responses", "df_context", "df_questions")
df_files <- c("questionnaires_linked.csv", "context.csv", "questions.csv")

# load data into named list
list_dfs <- list()
for (i in seq_len(length.out = length(x = df_files))) {
  list_dfs[[i]] <- read_csv(file = paste0(dir_processed, "/", df_files[i]))
}
names(list_dfs) <- df_names

# Is there any way of understanding which factors
# (contextual survey responses or demographics)
# are most predictive of the wellbeing or learning scores
