library(haven)
library(tidyverse)
library(ggplot2)
library(patchwork)

Exp1234_clean <- read_dta("Data_package/Data/Data_Exp1234_clean.dta")

######## Reproducing Figure 2 ########
######## Monetary Losses and Accuracy in Experiment 1 ########

# Load the necessary data for each panel

# Panel a)

Exp1_a <- Exp1234_clean |> filter(experiment == 1) |>
  group_by(aligned) |>
  summarise(mean_correct = mean(correct100),
            sd_oc = sd(correct100), n_oc = sum(!is.na(correct100))) |>
  mutate(upper_oc = mean_correct + qt(0.975, df = n_oc - 1) * (sd_oc / sqrt(n_oc)),
         lower_oc = mean_correct - qt(0.975, df = n_oc - 1) * (sd_oc / sqrt(n_oc))) |>
  mutate(x = 1)

plot_a <- ggplot(data = Exp1_a, aes(x = x, y = mean_correct, color = factor(aligned))) +
  geom_point(size = 3) +  # Scatter plot
  geom_errorbar(aes(ymin = lower_oc, ymax = upper_oc), width = 0.2) +  # Error bars
  scale_color_manual(values = c("black", "red"), 
                     name = NULL,
                     labels = c("No-shock pattern", "Shock pattern")) +  # Color scale
  scale_x_continuous(breaks = 1, labels = "Overall") +  # X-axis labels
  scale_y_continuous(limits = c(50, 85), breaks = seq(50, 85, 5)) +  # Y-axis limits and breaks
  labs(title = "Panel A. Aggregate", x = "", y = "Accuracy") +  # Titles and labels
  theme_minimal()

ggsave("Figures/figure2a.pdf", plot_a, width = 4, height = 6)

# Panel b)

Exp1_b <- Exp1234_clean |> filter(experiment == 1) |>
  group_by(aligned, difficult) |>
  summarise(mean_correct = mean(correct100),
            sd_oc = sd(correct100), n_oc = sum(!is.na(correct100))) |>
  mutate(upper_oc = mean_correct + qt(0.975, df = n_oc - 1) * (sd_oc / sqrt(n_oc)),
         lower_oc = mean_correct - qt(0.975, df = n_oc - 1) * (sd_oc / sqrt(n_oc))) |>
  mutate(x = difficult + 1, x = difficult + 1.04)

plot_b <- ggplot(data = Exp1_b, aes(x = x, y = mean_correct, color = factor(aligned))) +
  geom_point(size = 3) +  # Scatter plot
  geom_line(size = 1.5, linetype = "dotted") +
  geom_errorbar(aes(ymin = lower_oc, ymax = upper_oc), width = 0.2) +  # Error bars
  scale_color_manual(values = c("black", "red"), 
                     name = NULL,
                     labels = c("No-shock pattern", "Shock pattern")) +  # Color scale
  scale_x_continuous(breaks = c(1, 2, 3), labels = c("Easy", "Medium", "Hard")) +  # X-axis labels
  scale_y_continuous(limits = c(50, 85), breaks = seq(50, 85, 5)) +  # Y-axis limits and breaks
  labs(title = "Panel B. Pattern difficulty", x = "", y = "Accuracy") +  # Titles and labels
  theme_minimal()

ggsave("Figures/figure2b.pdf", plot_b, width = 5.5, height = 6)

# Panel c)

Exp1_c <- Exp1234_clean |> filter(experiment == 1) |>
  group_by(aligned, accbonus) |>
  summarise(mean_correct = mean(correct100),
            sd_oc = sd(correct100), n_oc = sum(!is.na(correct100))) |>
  mutate(upper_oc = mean_correct + qt(0.975, df = n_oc - 1) * (sd_oc / sqrt(n_oc)),
         lower_oc = mean_correct - qt(0.975, df = n_oc - 1) * (sd_oc / sqrt(n_oc))) |>
  mutate(x = accbonus + 1, x = accbonus + 1.04)

plot_c <- ggplot(data = Exp1_c, aes(x = x, y = mean_correct, color = factor(aligned))) +
  geom_point(size = 3) +  # Scatter plot
  geom_line(size = 1.5, linetype = "dotted") +
  geom_errorbar(aes(ymin = lower_oc, ymax = upper_oc), width = 0.2) +  # Error bars
  scale_color_manual(values = c("black", "red"), 
                     name = NULL,
                     labels = c("No-shock pattern", "Shock pattern")) +  # Color scale
  scale_x_continuous(breaks = c(1, 2), labels = c("Low", "High")) +  # X-axis labels
  scale_y_continuous(limits = c(50, 85), breaks = seq(50, 85, 5)) +  # Y-axis limits and breaks
  labs(title = "Panel C. Accuracy Bonus", x = "", y = "Accuracy") +  # Titles and labels
  theme_minimal()

ggsave("Figures/figure2c.pdf", plot_c, width = 5.5, height = 6)

