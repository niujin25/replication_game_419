## Set directory (if needed) and load in libraries
## setwd("XXX")
library(haven)
library(data.table)
library(ggplot2)

## Upload data
exp1234 <- as.data.table(read_dta("data/data_Exp1234_clean.dta"))

## Isolate data relating to experiment 2
## Anxiety is only reported if 'stake' column is > 0
exp2 <- exp1234[experiment == 2 & stake > 0]
## Anxiety level is stored in column 'anxiety'
exp2_anxiety <- unique(exp2[,.(anxiety, stake, idtreatmentblock)])

## Compute density for low loss (50 pence)
density_low_stakes <- c( 
  nrow(exp2_anxiety[anxiety == 1 & stake == 1]),
  nrow(exp2_anxiety[anxiety == 2 & stake == 1]),
  nrow(exp2_anxiety[anxiety == 3 & stake == 1]),
  nrow(exp2_anxiety[anxiety == 4 & stake == 1]),
  nrow(exp2_anxiety[anxiety == 5 & stake == 1])
) / nrow(exp2_anxiety[stake == 1])

## Compute density for high loss (5 pounds)
density_high_stakes <- c( 
  nrow(exp2_anxiety[anxiety == 1 & stake == 2]),
  nrow(exp2_anxiety[anxiety == 2 & stake == 2]),
  nrow(exp2_anxiety[anxiety == 3 & stake == 2]),
  nrow(exp2_anxiety[anxiety == 4 & stake == 2]),
  nrow(exp2_anxiety[anxiety == 5 & stake == 2])
) / nrow(exp2_anxiety[stake == 2])
anxiety = 1:5

fig3 <- ggplot() +
  geom_bar(
    data = data.frame(
      x = anxiety,
      y = density_low_stakes
      ), 
    aes(x = anxiety, y = density_low_stakes, fill = "Low loss"), 
    stat = "identity", 
    position = "identity"
  ) +
  geom_bar(
    data = data.frame(
      x = anxiety,
      y = density_high_stakes
    ),  
    aes(x = anxiety, y = density_high_stakes, fill = "High loss"), 
    stat = "identity", 
    position = "identity",
    alpha = 0.5,
    width = 0.7
  ) +
  scale_fill_manual(values = c("Low loss" = "skyblue", "High loss" = "salmon")) +
  labs(x = "Category", y = "Value", fill = "Group") +
  ylab("Density") + 
  xlab("I felt anxious to lose money") +
  theme_minimal()

## Save figure 
fig3
ggsave("figures/Figure_3.pdf", plot = fig3)
