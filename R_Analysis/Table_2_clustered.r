# Replication Games
# Anticipatory Anxiety and Wishful Thinking, AER (2024)

library(haven)
library(texreg)
library(dplyr)
library(lmtest)
library(sandwich)
library(fixest)

# Read the Stata data file
data <- read_dta("Data/Data_Exp1234_clean.dta")

#----------------------------------------------------------------------
# TABLE 2
#----------------------------------------------------------------------

data_table2 <- data %>%
  group_by(accbonus, aligned, difficult, id, stake) %>%
  summarise(correct100 = mean(correct100),
            experiment = mean(experiment))

# Generate interactions and variable labels
data_table2 <- data_table2 %>%
  mutate(alignedXaccbonus = aligned * accbonus,
         alignedXstake = aligned * stake,
         alignedXdifficult = aligned * difficult)


# Linear regression models with cluser errors
correct1a_model <- feols(correct100 ~ aligned + accbonus + difficult, cluster = ~ id, data = subset(data_table2, experiment == 1))
correct1b_model <- feols(correct100 ~ aligned + accbonus + difficult + alignedXaccbonus + alignedXdifficult, cluster = ~ id, data = subset(data_table2, experiment == 1))
correct2a_model <- feols(correct100 ~ aligned + accbonus + difficult + stake, cluster = ~ id, data = subset(data_table2, experiment == 2))
correct2b_model <- feols(correct100 ~ aligned + accbonus + difficult + stake + alignedXaccbonus + alignedXdifficult + alignedXstake, cluster = ~ id, data = subset(data_table2, experiment == 2))
correct3a_model <- feols(correct100 ~ aligned + accbonus + difficult, cluster = ~ id, data = subset(data_table2, experiment == 3))
correct3b_model <- feols(correct100 ~ aligned + accbonus + difficult + alignedXaccbonus + alignedXdifficult, cluster = ~ id, data = subset(data_table2, experiment == 3))
correct4a_model <- feols(correct100 ~ aligned + accbonus + difficult, cluster = ~ id, data = subset(data_table2, experiment == 4))
correct4b_model <- feols(correct100 ~ aligned + accbonus + difficult + difficult + alignedXaccbonus + alignedXdifficult, cluster = ~ id, data = subset(data_table2, experiment == 4))


#### Original LM models

#correct1a_model <- lm(correct100 ~ aligned + accbonus + difficult, data = subset(data_table2, experiment == 1))
#correct1b_model <- lm(correct100 ~ aligned + accbonus + difficult + alignedXaccbonus + alignedXdifficult, data = subset(data_table2, experiment == 1))

#correct2a_model <- lm(correct100 ~ aligned + accbonus + difficult + stake, data = subset(data_table2, experiment == 2))
#correct2b_model <- lm(correct100 ~ aligned + accbonus + difficult + stake + alignedXaccbonus + alignedXdifficult + alignedXstake, data = subset(data_table2, experiment == 2))

#correct3a_model <- lm(correct100 ~ aligned + accbonus + difficult, data = subset(data_table2, experiment == 3))
#correct3b_model <- lm(correct100 ~ aligned + accbonus + difficult + alignedXaccbonus + alignedXdifficult, data = subset(data_table2, experiment == 3))

#correct4a_model <- lm(correct100 ~ aligned + accbonus + difficult, data = subset(data_table2, experiment == 4))
#correct4b_model <- lm(correct100 ~ aligned + accbonus + difficult + alignedXaccbonus + alignedXdifficult, data = subset(data_table2, experiment == 4))

# Define the model list
models <- list(correct1a_model, correct1b_model, 
               correct2a_model, correct2b_model, 
               correct3a_model, correct3b_model, 
               correct4a_model, correct4b_model)

# Set custom model names
model_names <- c("Experiment 1a", "Experiment 1b", 
                 "Experiment 2a", "Experiment 2b", 
                 "Experiment 3a", "Experiment 3b", 
                 "Experiment 4a", "Experiment 4b")

# Generate the regression table
texreg(
  models, 
  custom.model.names = model_names,
  custom.coef.names = c("Constant", "Shock/Loss pattern", "High accuracy bonus (HAB)",
                        "Difficult pattern (DP)", "Shock/Loss pattern $\\times$ HAB",
                        "Shock/Loss pattern $\\times$ DP", "Loss Size (LS)", 
                        "Shock/Loss pattern $\\times$ LS"),
  digits = 3,
  override.se = list( # Generating t-stats
    c(1-pt(abs(as.numeric(correct1a_model$coefficients / correct1a_model$se)), df=correct1a_model$nobs-1)),
    c(1-pt(abs(as.numeric(correct1b_model$coefficients / correct1b_model$se)), df=correct1b_model$nobs-1)),
    c(1-pt(abs(as.numeric(correct2a_model$coefficients / correct2a_model$se)), df=correct2a_model$nobs-1)),
    c(1-pt(abs(as.numeric(correct2b_model$coefficients / correct2b_model$se)), df=correct2b_model$nobs-1)),
    c(1-pt(abs(as.numeric(correct3a_model$coefficients / correct3a_model$se)), df=correct3a_model$nobs-1)),
    c(1-pt(abs(as.numeric(correct3b_model$coefficients / correct3b_model$se)), df=correct3b_model$nobs-1)),
    c(1-pt(abs(as.numeric(correct4a_model$coefficients / correct4a_model$se)), df=correct4a_model$nobs-1)),
    c(1-pt(abs(as.numeric(correct4b_model$coefficients / correct4b_model$se)), df=correct4b_model$nobs-1))
  ), 
  override.pvalues = list( # Generating t-values
    c(1-pt(abs(as.numeric(correct1a_model$coefficients / correct1a_model$se)), df=correct1a_model$nobs-1)),
    c(1-pt(abs(as.numeric(correct1b_model$coefficients / correct1b_model$se)), df=correct1b_model$nobs-1)),
    c(1-pt(abs(as.numeric(correct2a_model$coefficients / correct2a_model$se)), df=correct2a_model$nobs-1)),
    c(1-pt(abs(as.numeric(correct2b_model$coefficients / correct2b_model$se)), df=correct2b_model$nobs-1)),
    c(1-pt(abs(as.numeric(correct3a_model$coefficients / correct3a_model$se)), df=correct3a_model$nobs-1)),
    c(1-pt(abs(as.numeric(correct3b_model$coefficients / correct3b_model$se)), df=correct3b_model$nobs-1)),
    c(1-pt(abs(as.numeric(correct4a_model$coefficients / correct4a_model$se)), df=correct4a_model$nobs-1)),
    c(1-pt(abs(as.numeric(correct4b_model$coefficients / correct4b_model$se)), df=correct4b_model$nobs-1))
  ),
  file = "Tables/Table_2.tex"
)

# ROBUSTNESS CHECK: Consider trials 0,1,2 respectively

#----------------------------------------------------------------------
# FIGURE 2
#----------------------------------------------------------------------
