#----------------------------------------------------------------------
# Replication Games
# Anticipatory Anxiety and Wishful Thinking, AER (2024)
#----------------------------------------------------------------------

# Download libraries
library(haven)
library(texreg)
library(dplyr)
library(ggplot2)
library(fixest)

#----------------------------------------------------------------------
# NEW DATA: We explain how we obtain the new data file
# 
# Data_Exp1234_clean_new.dta is obtained by keeping the variable "session"
# in the file Data_Exp1234_raw.dta, and follows all instructions
# in the AER_cleaning.do file otherwise
#
# This results in an identical dataset to Data_Exp1234_clean.dta, except
# it includes a variable "session"
#----------------------------------------------------------------------

data <- read_dta("Data/Data_Exp1234_clean_new.dta")

#----------------------------------------------------------------------
# TABLE 2
#----------------------------------------------------------------------

data_table2 <- data %>%
  group_by(accbonus, aligned, difficult, id, stake, session) %>%
  summarise(correct100 = mean(correct100),
            experiment = mean(experiment))

# Generate interactions and variable labels
data_table2 <- data_table2 %>%
  mutate(alignedXaccbonus = aligned * accbonus,
         alignedXstake = aligned * stake,
         alignedXdifficult = aligned * difficult)

## Linear regression models with clusered errors 
# no dropping
Exp3a_no_drop <- feols(correct100 ~ aligned + accbonus + difficult, cluster = ~ id, data = subset(data_table2, experiment == 3))
Exp3b_no_drop <- feols(correct100 ~ aligned + accbonus + difficult + alignedXaccbonus + alignedXdifficult, cluster = ~ id, data = subset(data_table2, experiment == 3))

# dropping session 0
Exp3a_drop_0 <- feols(correct100 ~ aligned + accbonus + difficult, cluster = ~ id, data = subset(data_table2, experiment == 3 & session != 0))
Exp3b_drop_0 <- feols(correct100 ~ aligned + accbonus + difficult + alignedXaccbonus + alignedXdifficult, cluster = ~ id, data = subset(data_table2, experiment == 3 & session != 0))

# dropping session 1
Exp3a_drop_1 <- feols(correct100 ~ aligned + accbonus + difficult, cluster = ~ id, data = subset(data_table2, experiment == 3 & session != 1))
Exp3b_drop_1 <- feols(correct100 ~ aligned + accbonus + difficult + alignedXaccbonus + alignedXdifficult, cluster = ~ id, data = subset(data_table2, experiment == 3 & session != 1))

# dropping session 2
Exp3a_drop_2 <- feols(correct100 ~ aligned + accbonus + difficult, cluster = ~ id, data = subset(data_table2, experiment == 3 & session != 2))
Exp3b_drop_2 <- feols(correct100 ~ aligned + accbonus + difficult + alignedXaccbonus + alignedXdifficult, cluster = ~ id, data = subset(data_table2, experiment == 3 & session != 2))

# Define the model list
models <- list(Exp3a_no_drop, Exp3b_no_drop,
               Exp3a_drop_0, Exp3b_drop_0, 
               Exp3a_drop_1, Exp3b_drop_1, 
               Exp3a_drop_2, Exp3b_drop_2)

# Set custom model names
model_names <- c("Exp 3a (no drop)", "Exp 3b (no drop)",
                 "Exp 3a (drop 0)", "Exp 3b (drop 0)", 
                 "Exp 3a (drop 1)", "Exp 3b (drop 1)", 
                 "Exp 3a (drop 2)", "Exp 3b (drop 2)")

# Generate the regression table
texreg(
  models, 
  custom.model.names = model_names, 
  file = "Tables/Robust_Table_2_Exp_3.tex",
  custom.coef.names = c("Constant", "Shock/Loss pattern", "High accuracy bonus (HAB)",
                        "Difficult pattern (DP)", "Shock/Loss pattern $\\times$ HAB",
                        "Shock/Loss pattern $\\times$ DP"),
  override.se = list( # Replace SEs with T-values
    c(1-pt(abs(as.numeric(Exp3a_no_drop$coefficients / Exp3a_no_drop$se)), df=Exp3a_no_drop$nobs-1)),
    c(1-pt(abs(as.numeric(Exp3b_no_drop$coefficients / Exp3b_no_drop$se)), df=Exp3b_no_drop$nobs-1)),
    c(1-pt(abs(as.numeric(Exp3a_drop_0$coefficients / Exp3a_drop_0$se)), df=Exp3a_drop_0$nobs-1)),
    c(1-pt(abs(as.numeric(Exp3b_drop_0$coefficients / Exp3b_drop_0$se)), df=Exp3b_drop_0$nobs-1)),
    c(1-pt(abs(as.numeric(Exp3a_drop_1$coefficients / Exp3a_drop_1$se)), df=Exp3a_drop_1$nobs-1)),
    c(1-pt(abs(as.numeric(Exp3b_drop_1$coefficients / Exp3b_drop_1$se)), df=Exp3b_drop_1$nobs-1)),
    c(1-pt(abs(as.numeric(Exp3a_drop_2$coefficients / Exp3a_drop_2$se)), df=Exp3a_drop_2$nobs-1)),
    c(1-pt(abs(as.numeric(Exp3b_drop_2$coefficients / Exp3b_drop_2$se)), df=Exp3b_drop_2$nobs-1))
  ),
  override.pvalues = list( # Replace P-values with T-values
    c(1-pt(abs(as.numeric(Exp3a_no_drop$coefficients / Exp3a_no_drop$se)), df=Exp3a_no_drop$nobs-1)),
    c(1-pt(abs(as.numeric(Exp3b_no_drop$coefficients / Exp3b_no_drop$se)), df=Exp3b_no_drop$nobs-1)),
    c(1-pt(abs(as.numeric(Exp3a_drop_0$coefficients / Exp3a_drop_0$se)), df=Exp3a_drop_0$nobs-1)),
    c(1-pt(abs(as.numeric(Exp3b_drop_0$coefficients / Exp3b_drop_0$se)), df=Exp3b_drop_0$nobs-1)),
    c(1-pt(abs(as.numeric(Exp3a_drop_1$coefficients / Exp3a_drop_1$se)), df=Exp3a_drop_1$nobs-1)),
    c(1-pt(abs(as.numeric(Exp3b_drop_1$coefficients / Exp3b_drop_1$se)), df=Exp3b_drop_1$nobs-1)),
    c(1-pt(abs(as.numeric(Exp3a_drop_2$coefficients / Exp3a_drop_2$se)), df=Exp3a_drop_2$nobs-1)),
    c(1-pt(abs(as.numeric(Exp3b_drop_2$coefficients / Exp3b_drop_2$se)), df=Exp3b_drop_2$nobs-1))
  )
)



