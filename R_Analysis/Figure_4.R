library(haven)
library(tidyverse)
library(ggplot2)
library(patchwork)

Exp1234_clean <- read_dta("Downloads/Data_package/Data/Data_Exp1234_clean.dta")

######## Reproducing Figure 4 ########
######## Monetary Losses and Accuracy in Experiment 2 ########

# Load the necessary data for each panel

Exp2_a <- Exp1234_clean |> filter(experiment == 2, stake != 0) |>
  group_by(aligned) |>
  summarise(mean_correct = mean(correct100),
         sd_oc = sd(correct100), n_oc = sum(!is.na(correct100)),
         upper_oc = mean_correct + qt(0.975, df = n_oc - 1) * (sd_oc / sqrt(n_oc)),
         lower_oc = mean_correct - qt(0.975, df = n_oc - 1) * (sd_oc / sqrt(n_oc))) |>
  mutate(x = 1)
  
# Panel a)
plot_a <- ggplot(data = Exp2_a, aes(x = x, y = mean_correct, color = factor(aligned))) +
  geom_point(size = 3) +  # Scatter plot
  geom_errorbar(aes(ymin = lower_oc, ymax = upper_oc), width = 0.2) +  # Error bars
  scale_color_manual(values = c("black", "red"), 
                     name = NULL,
                     labels = c("No-loss pattern", "Loss pattern")) +  # Color scale
  scale_x_continuous(breaks = 1, labels = "Overall") +  # X-axis labels
  scale_y_continuous(limits = c(50, 85), breaks = seq(50, 85, 5)) +  # Y-axis limits and breaks
  labs(title = "Panel A. Aggregate", x = "", y = "Accuracy") +  # Titles and labels
  theme_minimal()

ggsave("Figures/figure4a.pdf", plot_a, width = 4, height = 6)

# Panel b)

Exp2_b <- Exp1234_clean |> filter(experiment == 2, stake != 0) |>
  group_by(id, aligned, difficult) |> 
  group_by(difficult, aligned) |>
  summarise(mean_correct = mean(correct100),
            sd_oc = sd(correct100), n_oc = sum(!is.na(correct100)),
            upper_oc = mean_correct + qt(0.975, df = n_oc - 1) * (sd_oc / sqrt(n_oc)),
            lower_oc = mean_correct - qt(0.975, df = n_oc - 1) * (sd_oc / sqrt(n_oc))) |>
  mutate(x = difficult + 1)

plot_b <- ggplot(data = Exp2_b, aes(x = x, y = mean_correct, color = factor(aligned))) +
  geom_line(size = 1.5, linetype = "dotted") +
  geom_point(size = 3) +  # Scatter plot
  geom_errorbar(aes(ymin = lower_oc, ymax = upper_oc), width = 0.1) +  # Error bars
  scale_color_manual(values = c("black", "red"), 
                     name = NULL,
                     labels = c("No-loss pattern", "Loss pattern")) +  # Color scale
  scale_x_continuous(breaks = c(1, 2), labels = c("Easy", "Hard")) +  # X-axis labels
  scale_y_continuous(limits = c(45, 85), breaks = seq(45, 85, 5)) +  # Y-axis limits and breaks
  labs(title = "Panel B. Pattern Difficulty", x = "", y = "Accuracy") +  # Titles and labels
  theme_minimal()

ggsave("Figures/figure4b.pdf", plot_b, width = 5, height = 6)


# -----------------everything above here is correct----------------------------

# Panel c)
Exp2_c <- Exp1234_clean |> filter(experiment == 2, stake != 0) |>
  # group_by(id, aligned, accbonus) |> 
  group_by(accbonus, aligned) |>
  summarise(mean_correct = mean(correct100),
            sd_oc = sd(correct100), n_oc = sum(!is.na(correct100)),
            upper_oc = mean_correct + qt(0.975, df = n_oc - 1) * (sd_oc / sqrt(n_oc)),
            lower_oc = mean_correct - qt(0.975, df = n_oc - 1) * (sd_oc / sqrt(n_oc))) |>
  mutate(x = accbonus + 1)

plot_c <- ggplot(data = Exp2_c, aes(x = x, y = mean_correct, color = factor(aligned))) +
  geom_line(size = 1.5, linetype = "dotted") +
  geom_point(size = 3) +  # Scatter plot
  geom_errorbar(aes(ymin = lower_oc, ymax = upper_oc), width = 0.1) +  # Error bars
  scale_color_manual(values = c("black", "red"), 
                     name = NULL,
                     labels = c("No-loss pattern", "Loss pattern")) +  # Color scale
  scale_x_continuous(breaks = c(1, 2), labels = c("Low", "High")) +  # X-axis labels
  scale_y_continuous(limits = c(50, 85), breaks = seq(50, 85, 5)) +  # Y-axis limits and breaks
  labs(title = "Panel C. Accuracy bonus", x = "", y = "Accuracy") +  # Titles and labels
  theme_minimal()

ggsave("Figures/figure4c.pdf", plot_c, width = 5, height = 6)

# Panel d)
Exp2_d <- Exp1234_clean |> filter(experiment == 2, stake != 0) |>
  group_by(stake, aligned) |>
  summarise(mean_correct = mean(correct100),
            sd_oc = sd(correct100), n_oc = sum(!is.na(correct100)),
            upper_oc = mean_correct + qt(0.975, df = n_oc - 1) * (sd_oc / sqrt(n_oc)),
            lower_oc = mean_correct - qt(0.975, df = n_oc - 1) * (sd_oc / sqrt(n_oc))) |>
  mutate(x = stake, x1 = stake + 0.1)

plot_d <- ggplot(data = Exp2_d, aes(x = x, y = mean_correct, color = factor(aligned))) +
  geom_line(size = 1.5, linetype = "dotted") +
  geom_point(size = 3) +  # Scatter plot
  geom_errorbar(aes(ymin = lower_oc, ymax = upper_oc), width = 0.1) +  # Error bars
  scale_color_manual(values = c("black", "red"), 
                     name = NULL,
                     labels = c("No-loss pattern", "Loss pattern")) +  # Color scale
  scale_x_continuous(breaks = c(1, 2), labels = c("Low", "High")) +  # X-axis labels
  scale_y_continuous(limits = c(50, 85), breaks = seq(50, 85, 5)) +  # Y-axis limits and breaks
  labs(title = "Panel D. Loss size", x = "", y = "Accuracy") +  # Titles and labels
  theme_minimal()

ggsave("Figures/figure4d.pdf", plot_d, width = 5, height = 6)

# Combine panels into a single plot
combined_plot <- plot_a + plot_b + plot_c + plot_d + plot_layout(guides = "collect")
  
# Save the combined plot as a PDF
ggsave("Figures/Figure4.pdf", combined_plot, width = 10, height = 12)




