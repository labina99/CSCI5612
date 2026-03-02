# ============================
# ARM on Numeric Data (R)
# Using interpretable cut() bins
# ============================

library(arules)
library(dplyr)
library(arulesViz)
library(igraph)
library(htmlwidgets)


# Load data
data_path <- "/Users/User/Desktop/dataset1_cleaned_numeric.csv" 
df <- read.csv(data_path, header = TRUE, stringsAsFactors = FALSE)

# Keep numeric columns only (safety)
df <- df %>% select(where(is.numeric))

# BINNING (interpretable categories)
# Note: include.lowest=TRUE prevents NA at boundaries

df$Hours_Studied_bin <- cut(
  df$Hours_Studied,
  breaks = quantile(df$Hours_Studied, probs = seq(0,1,0.25), na.rm=TRUE),
  include.lowest = TRUE,
  labels = c("study_Q1","study_Q2","study_Q3","study_Q4")
)
df$Attendance_bin <- cut(df$Attendance,
                         breaks = c(-Inf, 70, 85, 95, Inf),
                         labels = c("attendance_poor", "attendance_ok", "attendance_good", "attendance_excellent"),
                         include.lowest = TRUE)
df$Sleep_Hours_bin <- cut(df$Sleep_Hours,
                          breaks = c(-Inf, 6, 8, Inf),
                          labels = c("sleep_short", "sleep_normal", "sleep_long"),
                          include.lowest = TRUE)
df$Previous_Scores_bin <- cut(df$Previous_Scores,
                              breaks = c(-Inf, 60, 70, 85, Inf),
                              labels = c("prev_failing", "prev_passing", "prev_good", "prev_excellent"),
                              include.lowest = TRUE)
df$Tutoring_Sessions_bin <- cut(df$Tutoring_Sessions,
                                breaks = c(-Inf, 0, 2, Inf),
                                labels = c("tutor_none", "tutor_some", "tutor_frequent"),
                                include.lowest = TRUE)
df$Physical_Activity_bin <- cut(df$Physical_Activity,
                                breaks = c(-Inf, 2, 5, Inf),
                                labels = c("activity_low", "activity_moderate", "activity_high"),
                                include.lowest = TRUE)
df$Exam_Score_bin <- cut(
  df$Exam_Score,
  breaks = quantile(df$Exam_Score, probs = seq(0,1,0.25), na.rm=TRUE),
  include.lowest = TRUE,
  labels = c("exam_Q1","exam_Q2","exam_Q3","exam_Q4")  # Q1 = lowest 25% ; Q4 = highest 25%
)

# Build "binned only" dataset for transactions
binned <- df %>%
  select(ends_with("_bin"))

print(head(binned, 10))

table(df$Hours_Studied_bin)
table(df$Attendance_bin)
table(df$Sleep_Hours_bin)
table(df$Exam_Score_bin)



# Convert to transactions
trans <- as(binned, "transactions")
summary(trans)
inspect(head(trans, 10))

# Run Apriori
MIN_SUPPORT <- 0.10
MIN_CONF <- 0.50
MIN_LEN <- 2

rules <- apriori(trans,
                 parameter = list(supp = MIN_SUPPORT, conf = MIN_CONF, minlen = MIN_LEN))

rules <- rules[!is.redundant(rules)]
summary(rules)

# Top 15 by support / confidence / lift
top15_support <- head(sort(rules, by="support", decreasing=TRUE), 15)
top15_conf    <- head(sort(rules, by="confidence", decreasing=TRUE), 15)
top15_lift    <- head(sort(rules, by="lift", decreasing=TRUE), 15)

cat("\n=== TOP 15 SUPPORT ===\n")
inspect(top15_support)

cat("\n=== TOP 15 CONFIDENCE ===\n")
inspect(top15_conf)

cat("\n=== TOP 15 LIFT ===\n")
inspect(top15_lift)


# Network visualization (top 10 by lift)
#Visualize

p <- plot(rules, method = "graph", engine = "htmlwidget")
saveWidget(p, "/Users/User/Desktop/arm_rules.html")

dev.off()
