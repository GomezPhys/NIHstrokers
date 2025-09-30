### Required packages
library(readxl) ## To load excel sheet
library(dplyr) # Data grammar and manipulation
library(rstatix) # Shapiro Wilk and effect size
library(psych) #descriptives
library(kableExtra) #tables
library(lme4) #linear mixed effects models (LMM)
library(lmerTest) #anova like output for LMM
library(ggplot2) #data visualization
library(ggpubr)#data visualization
library(ggprism)##makes plots look like graphad
library(table1) #for descriptives


Df <- read_excel("~/strokersbfpmg.xlsx",
                 sheet = "combined")
View(Df)

Df$Intensity <- as.factor(Df$Intensity)
Df$Group <- as.factor(Df$Group)

## Order conditions
Df$Intensity <- ordered(Df$Intensity,
                        levels = c("Baseline", "Low",
                                   "Moderate","High"))
Df$Group <- ordered(Df$Group,
                    levels = c("Young", "Stroke", "Control"))

###### Linear Mixed models ESS Antegrade########
lmModel = lmer(ESS ~ Intensity*Group + (1|ID),
               data=Df, REML=FALSE)
summary(lmModel)

# mixed model
anova(lmModel)
#test of the random effects in the model
rand(lmModel)

Df <- Df %>%
  mutate(Intensity_Group = interaction(Intensity, Group))

# Post-hoc pairwise comparisons Holms-Bonferroni correction
pwc <- Df %>%
  pairwise_t_test(ESS ~ Intensity_Group, paired = F,
                  p.adjust.method	= "holm")
pwc %>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# Effect size Cohen's D with Hedge's g correction for small sample size
Df %>% cohens_d(ESS ~ Intensity_Group,
                paired = F, hedges.correction = TRUE)%>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")

#Plots
# Add position for p values in boxplot
pwc <- pwc %>% add_xy_position(x = "Intensity")
# Boxplot of ESS
Antegrade_ESS <- ggboxplot(Df, x = "Intensity", y = "ESS",
                                color = "Group", palette = get_palette("Set1", 4),
                                ylab = "ESS (dynes/cm2)") +
  stat_pvalue_manual(pwc,size = 4.5,hide.ns = TRUE) +
  theme_prism()

# Calculate 95% confidence intervals for Re_crit_B by Intensity and Group
ci_table <- Df %>%
  group_by(Intensity, Group) %>%
  summarise(
    n = sum(!is.na(Re_crit_B)),
    mean = mean(Re_crit_B, na.rm = TRUE),
    sd = sd(Re_crit_B, na.rm = TRUE),
    se = sd / sqrt(n),
    lower_ci = mean - 1.96 * se,
    upper_ci = mean + 1.96 * se,
    .groups = "drop"
  )

# View nicely formatted table (if using kableExtra)
library(kableExtra)
ci_table %>%
  kbl(caption = "95% CI for Re_crit_B by Intensity and Group") %>%
  kable_classic(full_width = F, html_font = "Cambria")


######RE trial####

plot_data <- Df %>%
  group_by(Intensity, Group) %>%
  summarise(
    mean_Re_B = mean(Re_B, na.rm = TRUE),
    n = sum(!is.na(Re_crit_B)),
    se = sd(Re_crit_B, na.rm = TRUE) / sqrt(n),
    lower_ci = mean(Re_crit_B, na.rm = TRUE) - 1.96 * se,
    upper_ci = mean(Re_crit_B, na.rm = TRUE) + 1.96 * se,
    .groups = "drop"
  )


ggplot(plot_data, aes(x = Intensity, y = mean_Re_B, group = Group)) +
  # Red error bars for CI (from Re_crit_B)
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci),
                color = "red", width = 0.2,
                position = position_dodge(width = 0.4)) +
  # Colored points for mean Re_B per group
  geom_point(aes(color = Group),
             size = 3, position = position_dodge(width = 0.4)) +
  labs(
    title = "Mean Re_B with 95% CI from Re_crit_B",
    y = "Re_B",
    x = "Intensity",
    color = "Group"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Cambria", size = 12),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )


ggplot(plot_data, aes(x = Intensity, y = mean_Re_B, fill = Group)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6, color = "black") +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci),
                color = "red", width = 0.2,
                position = position_dodge(width = 0.7)) +
  labs(
    title = "Mean Re_B with 95% CI from Re_crit_B",
    y = "Re_B",
    x = "Intensity",
    fill = "Group"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Cambria", size = 12),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )


ggplot(plot_data, aes(x = Intensity, y = mean_Re_B, group = Group)) +
  # Red 95% CI bars from Re_crit_B
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci),
                color = "red", width = 0.2,
                position = position_dodge(width = 0.4)) +

  # Lines connecting means per group
  geom_line(aes(color = Group), position = position_dodge(width = 0.4)) +

  # Points (with shape) for mean Re_B
  geom_point(aes(color = Group, shape = Group), size = 4,
             position = position_dodge(width = 0.4)) +

  labs(
    title = "Mean Re_B with 95% CI from Re_crit_B",
    y = "Re_B",
    x = "Intensity",
    color = "Group",
    shape = "Group"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Cambria", size = 12),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )



ggplot(ci_table, aes(x = Intensity, y = mean, color = Group, group = Group)) +
  geom_point(position = position_dodge(width = 0.4), size = 3) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci),
                position = position_dodge(width = 0.4),
                width = 0.2) +
  labs(y = "Re_crit_B (Mean ± 95% CI)",
       x = "Intensity") +
  theme_minimal()

ci_table <- Df %>%
  group_by(Intensity, Group) %>%
  summarise(
    n_Re_crit_B = sum(!is.na(Re_crit_B)),
    mean_Re_crit_B = mean(Re_crit_B, na.rm = TRUE),
    sd_Re_crit_B = sd(Re_crit_B, na.rm = TRUE),
    se_Re_crit_B = sd_Re_crit_B / sqrt(n_Re_crit_B),
    lower_ci_Re_crit_B = mean_Re_crit_B - 1.96 * se_Re_crit_B,
    upper_ci_Re_crit_B = mean_Re_crit_B + 1.96 * se_Re_crit_B,

    n_Re_B = sum(!is.na(Re_B)),
    mean_Re_B = mean(Re_B, na.rm = TRUE),
    sd_Re_B = sd(Re_B, na.rm = TRUE),
    se_Re_B = sd_Re_B / sqrt(n_Re_B),
    lower_ci_Re_B = mean_Re_B - 1.96 * se_Re_B,
    upper_ci_Re_B = mean_Re_B + 1.96 * se_Re_B,

    .groups = "drop"
  )
library(tidyr)

ci_table_long <- ci_table %>%
  pivot_longer(
    cols = c(mean_Re_B, lower_ci_Re_B, upper_ci_Re_B,
             mean_Re_crit_B, lower_ci_Re_crit_B, upper_ci_Re_crit_B),
    names_to = c(".value", "Variable"),
    names_pattern = "(.*)_(Re_B|Re_crit_B)"
  )


ggplot(ci_table_long, aes(x = Intensity, y = mean, color = Variable, shape = Group, group = interaction(Group, Variable))) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci),
                position = position_dodge(width = 0.5),
                width = 0.2) +
  labs(y = "Mean (± 95% CI)", x = "Intensity") +
  theme_minimal() +
  theme(legend.position = "right")

ggplot(ci_table_long, aes(x = interaction(Intensity, Group), y = mean, fill = Variable)) +
  geom_point(position = position_dodge(width = 0.7), size = 3, shape = 21, color = "black") +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci),
                width = 0.2,
                position = position_dodge(width = 0.7)) +
  labs(y = "Mean (± 95% CI)", x = "Intensity and Group") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank())




#####
Df$Group <- as.factor(Df$Group)
Df$Intensity <- as.factor(Df$Intensity)

# Create an interaction term in the data for visualization purposes
Df$Interaction <- interaction(Df$Intensity, Df$Group)

# Perform pairwise comparisons within each Group for Intensity
pwc <- Df %>%
  pairwise_t_test(ESS ~ Interaction, paired = FALSE, p.adjust.method = "holm")

# Add position for p-values in the plot
pwc <- pwc %>% add_xy_position(x = "Interaction")

# Create the boxplot
Antegrade_ESS <- ggboxplot(Df, x = "Intensity", y = "ESS",
                           color = "Group", palette = get_palette("Set1", length(unique(Df$Group))),
                           ylab = "ESS (dynes/cm2)") +
  stat_pvalue_manual(pwc, size = 4.5, hide.ns = TRUE) +
  theme_prism()

# Print the plot
print(Antegrade_ESS)
# Create an interaction term for comparisons
Df$Interaction <- interaction(Df$Intensity, Df$Group)

# Perform pairwise comparisons using the interaction term
pwc2 <- Df %>%
  pairwise_t_test(ESS ~ Interaction, paired = FALSE, p.adjust.method = "holm")

# Filter only significant comparisons (p.adj <= 0.05)
pwc2_filtered <- pwc2 %>% filter(p.adj <= 0.05)

# Add significance positions for plotting
pwc2_filtered <- pwc2_filtered %>% add_xy_position(x = "Interaction")

# Create the boxplot with significance annotations
Antegrade_ESS <- ggboxplot(Df, x = "Interaction", y = "ESS",
                           color = "Group", palette = get_palette("Set1", length(unique(Df$Group))),
                           ylab = "ESS (dynes/cm2)") +
  stat_pvalue_manual(pwc2_filtered, size = 3.5, hide.ns = TRUE, tip.length = 0.01) + # Annotate with significant comparisons
  theme_prism() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for better readability

# Print the plot
print(Antegrade_ESS)




# Ensure Group and Intensity are factors
Df$Group <- as.factor(Df$Group)
Df$Intensity <- as.factor(Df$Intensity)

# Create an interaction term in the data for comprehensive comparisons
Df$Interaction <- interaction(Df$Intensity, Df$Group)

# Perform pairwise comparisons for the interaction term
pwc <- Df %>%
  pairwise_t_test(ESS ~ Interaction, paired = FALSE, p.adjust.method = "holm")

# Filter significant comparisons
significant_pwc <- pwc %>% filter(p.adj <= 0.05)

# Display significant comparisons
significant_pwc %>%
  kbl(caption = "Significant Pairwise Comparisons for ESS") %>%
  kable_classic(full_width = FALSE, html_font = "Cambria")

# Calculate effect sizes for Intensity and Group interaction
effect_sizes <- Df %>%
  cohens_d(ESS ~ Interaction, paired = FALSE, hedges.correction = TRUE)

# Display effect sizes
effect_sizes %>%
  kbl(caption = "Effect Sizes for ESS by Interaction of Intensity and Group") %>%
  kable_classic(full_width = FALSE, html_font = "Cambria")

# Add position for p-values in the plot
pwc <- pwc %>% add_xy_position(x = "Interaction")



# Print the plot
print(Antegrade_ESS)



# Print the improved plot
print(Antegrade_ESS)


#####CC####

###### Linear Mixed models ESS Antegrade
lmModel2 = lmer(CC ~ Intensity_Group + (1|ID),
               data=Df, REML=FALSE)
summary(lmModel2)

# mixed model
anova(lmModel2)
#test of the random effects in the model
rand(lmModel2)

# Post-hoc pairwise comparisons Holms-Bonferroni correction
pwc2 <- Df %>%
  pairwise_t_test(CC ~ Intensity_Group, paired = F,
                  p.adjust.method	= "holm")
pwc2 %>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# Effect size Cohen's D with Hedge's g correction for small sample size
Df %>% cohens_d(CC ~ Intensity,
                paired = F, hedges.correction = TRUE)%>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")

#Plots
# Add position for p values in boxplot
pwc3 <- pwc2 %>% add_xy_position(x = "Intensity")
# Boxplot of ESS
AC <- ggboxplot(Df, x = "Intensity", y = "CC",
                           color = "Group", palette = get_palette("Set1", 4),
                           ylab = "Arterial Compliance (mm^2/mmHg)") +
  stat_pvalue_manual(pwc,size = 4.5,hide.ns = TRUE) +
  theme_prism()

#####
str(Df)
summary(Df$CC)
Df <- Df %>% filter(!is.na(CC))

# Fit linear mixed model with CC
lmModel2 <- lmer(CC ~ Intensity * Group + (1|ID), data = Df, REML = FALSE)
summary(lmModel2)

# Mixed model ANOVA
anova(lmModel2)

# Test of random effects in the model
rand(lmModel2)

Df$Interaction <- interaction(Df$Intensity, Df$Group)
# Post-hoc pairwise comparisons with Holm's Bonferroni correction
pwc2 <- Df %>%
  pairwise_t_test(CC ~ Interaction, paired = FALSE, p.adjust.method = "holm")


# Display pairwise comparisons table
pwc2 %>%
  kbl(caption = "Pairwise Comparisons for Arterial Compliance by Intensity and Group") %>%
  kable_classic(full_width = FALSE, html_font = "Cambria")
pwc2 %>%
  kbl(caption = "Pairwise Comparisons for Arterial Compliance by Interaction of Intensity and Group") %>%
  kable_classic(full_width = FALSE, html_font = "Cambria")

######

# Fit linear mixed model with Group as an additional fixed effect
lmModel2 <- lmer(CC ~ Intensity * Group + (1|ID), data = Df, REML = FALSE)
summary(lmModel2)

# ANOVA on the model
anova(lmModel2)

# Test the random effects in the model
rand(lmModel2)

pwc <- Df %>%
  pairwise_t_test(ESS ~ Intensity_Group, paired = FALSE, p.adjust.method = "holm")

# Post-hoc pairwise comparisons for both Intensity and Group with Holm's correction
pwc3 <- Df %>%
  pairwise_t_test(CC ~ Intensity_Group, paired = FALSE, p.adjust.method = "holm")

# Display pairwise comparisons table
pwc3 %>%
  kbl(caption = "Pairwise Comparisons for Arterial Compliance by Intensity and Group") %>%
  kable_classic(full_width = FALSE, html_font = "Cambria")

# Filter significant comparisons (p.adj <= 0.05)
pwc2_filtered <- pwc2 %>% filter(p.adj <= 0.05)

# Add positions for p-values in the plot
pwc2_filtered <- pwc2_filtered %>% add_xy_position(x = "interaction(Intensity, Group)")

# Calculate effect sizes for both Intensity and Group
effect_sizes <- Df %>%
  cohens_d(CC ~ interaction(Intensity, Group), paired = FALSE, hedges.correction = TRUE)

# Display effect sizes table
effect_sizes %>%
  kbl(caption = "Effect Sizes for Arterial Compliance by Intensity and Group") %>%
  kable_classic(full_width = FALSE, html_font = "Cambria")

# Create a boxplot with significance annotations
AC <- ggboxplot(Df, x = "Intensity", y = "CC",
                color = "Group", palette = get_palette("Set1", length(unique(Df$Group))),
                ylab = "Arterial Compliance (mm^2/mmHg)") +
  stat_pvalue_manual(pwc2_filtered, size = 4.5, hide.ns = TRUE, tip.length = 0.01) +
  theme_prism() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for better readability

# Print the plot
print(AC)

#####BSI#####
###### Linear Mixed models ESS Antegrade
lmModel3 = lmer(Beta_Stiffness ~ Intensity + (1|ID),
                data=Df, REML=FALSE)
summary(lmModel3)

# mixed model
anova(lmModel3)
#test of the random effects in the model
rand(lmModel3)

# Post-hoc pairwise comparisons Holms-Bonferroni correction
pwc <- Df %>%
  pairwise_t_test(Beta_Stiffness ~ Intensity_Group, paired = F,
                  p.adjust.method	= "holm")
pwc %>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# Effect size Cohen's D with Hedge's g correction for small sample size
Df %>% cohens_d(Beta_Stiffness ~ Intensity,
                paired = F, hedges.correction = TRUE)%>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")

#Plots
# Add position for p values in boxplot
pwc <- pwc %>% add_xy_position(x = "Intensity")
# Boxplot of ESS
Beta <- ggboxplot(Df, x = "Intensity", y = "Beta_Stiffness",
                           color = "Group", palette = get_palette("Set1", 4),
                           ylab = "BEta Stiffness Index") +
  stat_pvalue_manual(pwc,size = 4.5,hide.ns = TRUE) +
  theme_prism()



# Fit a linear mixed model with Beta_Stiffness
lmModel2 <- lmer(Beta_Stiffness ~ Intensity * Group + (1|ID), data = Df, REML = FALSE)
summary(lmModel2)

# ANOVA on the mixed model
anova(lmModel2)

# Test of random effects in the model
rand(lmModel2)

# Perform pairwise comparisons using the interaction column
pwc2 <- Df %>%
  pairwise_t_test(Beta_Stiffness ~ Interaction, paired = FALSE, p.adjust.method = "holm")

# Display pairwise comparisons table
pwc2 %>%
  kbl(caption = "Pairwise Comparisons for Beta Stiffness by Interaction of Intensity and Group") %>%
  kable_classic(full_width = FALSE, html_font = "Cambria")


#####Diameters

# Fit a linear mixed model with Beta_Stiffness
lmModel3 <- lmer(Systolic_diameter ~ Intensity * Group + (1|ID), data = Df, REML = FALSE)
summary(lmModel3)

# ANOVA on the mixed model
anova(lmModel3)

# Test of random effects in the model
rand(lmModel3)

# Perform pairwise comparisons using the interaction column
pwc2 <- Df %>%
  pairwise_t_test(Systolic_diameter ~ Intensity_Group, paired = FALSE, p.adjust.method = "holm")

# Display pairwise comparisons table
pwc2 %>%
  kbl(caption = "Systolic_diameter") %>%
  kable_classic(full_width = FALSE, html_font = "Cambria")

diastolic

lmModel4 <- lmer(Diastolic_Diameter ~ Intensity * Group + (1|ID), data = Df, REML = FALSE)
summary(lmModel4)

# ANOVA on the mixed model
anova(lmModel4)

# Test of random effects in the model
rand(lmModel4)

# Perform pairwise comparisons using the interaction column
pwc4 <- Df %>%
  pairwise_t_test(Diastolic_Diameter ~ Intensity_Group, paired = FALSE, p.adjust.method = "holm")

# Display pairwise comparisons table
pwc4 %>%
  kbl(caption = "Diastolic_Diameter") %>%
  kable_classic(full_width = FALSE, html_font = "Cambria")


#####Re
Re_plot <- ggplot(Df, aes(x = Intensity)) +
  # Plot Re_crit_B
  stat_summary(aes(y = Re_crit_B, color = "Re_crit_B"),
               fun = "mean", geom = "line", position = position_dodge(width = 0.2), size = 1) +
  stat_summary(aes(y = Re_crit_B, color = "Re_crit_B"),
               fun = "mean", geom = "point", position = position_dodge(width = 0.2), size = 2) +
  stat_summary(aes(y = Re_crit_B, color = "Re_crit_B"),
               fun.data = mean_sdl, fun.args = list(mult = 1),
               geom = "errorbar", width = 0.2, position = position_dodge(width = 0.2)) +

  # Plot Re_B
  stat_summary(aes(y = Re_B, color = "Re_B"),
               fun = "mean", geom = "line", position = position_dodge(width = 0.2), size = 1) +
  stat_summary(aes(y = Re_B, color = "Re_B"),
               fun = "mean", geom = "point", position = position_dodge(width = 0.2), size = 2) +
  stat_summary(aes(y = Re_B, color = "Re_B"),
               fun.data = mean_sdl, fun.args = list(mult = 1),
               geom = "errorbar", width = 0.2, position = position_dodge(width = 0.2)) +

  # Facet by Group
  facet_wrap(~ Group, scales = "free_y") +

  # Labels and customization
  labs(x = "Intensity", y = "Normalized Reynolds Number (AU)", color = "Measurement") +
  scale_color_manual(values = c("Re_crit_B" = "red", "Re_B" = "blue")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Create the plot for Re_crit_B and Re_B with means and error bars
Re_plot <- ggplot(Df, aes(x = Intensity)) +
  # Plot Re_crit_B
  stat_summary(aes(y = Re_crit_B, color = "Re_crit_B"),
               fun = "mean", geom = "line", position = position_dodge(width = 0.3), size = 1.2) +
  stat_summary(aes(y = Re_crit_B, color = "Re_crit_B"),
               fun = "mean", geom = "point", position = position_dodge(width = 0.3), size = 3) +
  stat_summary(aes(y = Re_crit_B, color = "Re_crit_B"),
               fun.data = mean_sdl, fun.args = list(mult = 1),
               geom = "errorbar", width = 0.2, position = position_dodge(width = 0.3)) +

  # Plot Re_B
  stat_summary(aes(y = Re_B, color = "Re_B"),
               fun = "mean", geom = "line", position = position_dodge(width = 0.3), size = 1.2) +
  stat_summary(aes(y = Re_B, color = "Re_B"),
               fun = "mean", geom = "point", position = position_dodge(width = 0.3), size = 3) +
  stat_summary(aes(y = Re_B, color = "Re_B"),
               fun.data = mean_sdl, fun.args = list(mult = 1),
               geom = "errorbar", width = 0.2, position = position_dodge(width = 0.3)) +

  # Facet by Group for separation
  facet_wrap(~ Group, scales = "free_y") +

  # Labels and customization
  labs(x = "Intensity", y = "Normalized Reynolds Number (AU)", color = "Measurement") +
  scale_color_manual(values = c("Re_crit_B" = "red", "Re_B" = "blue")) +
  theme_minimal(base_size = 14) +  # Increase base size for better readability
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),  # Remove minor grid lines for cleaner look
    panel.grid.major = element_line(color = "gray90"),  # Lighten grid lines
    legend.position = "top",  # Move legend to the top for better visibility
    legend.title = element_blank()  # Remove legend title for simplicity
  )

# Print the plot
print(Re_plot)


table1(~ ESS + Systolic_diameter + Diastolic_Diameter + Beta_Stiffness + CC | Group*Intensity,
       total=F,render.categorical="FREQ (PCTnoNA%)", na.rm = TRUE,data=Df,
       render.missing=NULL,topclass="Rtable1-grid Rtable1-shade Rtable1-times",
       overall=FALSE)



#####Diameters#####
lmModel2 = lmer(Systolic_diameter ~ Intensity_Group + (1|ID),
                data=Df, REML=FALSE)
summary(lmModel2)

# mixed model
anova(lmModel2)
#test of the random effects in the model
rand(lmModel2)

# Post-hoc pairwise comparisons Holms-Bonferroni correction
pwc5 <- Df %>%
  pairwise_t_test(Systolic_diameter ~ Intensity_Group, paired = F,
                  p.adjust.method	= "holm")
pwc5 %>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# Effect size Cohen's D with Hedge's g correction for small sample size
Df %>% cohens_d(Systolic_diameter ~ Intensity,
                paired = F, hedges.correction = TRUE)%>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")

#Plots
# Add position for p values in boxplot
pwc6 <- pwc5 %>% add_xy_position(x = "Intensity")
# Boxplot of ESS
SD <- ggboxplot(Df, x = "Intensity", y = "Systolic_diameter",
                color = "Group", palette = get_palette("Set1", 4),
                ylab = "Systolic Diameter (mm)") +
  stat_pvalue_manual(pwc6,size = 10,hide.ns = TRUE) +
  theme_prism()

diastolic

lmModel2 = lmer(Diastolic_Diameter ~ Intensity_Group + (1|ID),
                data=Df, REML=FALSE)
summary(lmModel2)

# mixed model
anova(lmModel2)
#test of the random effects in the model
rand(lmModel2)

# Post-hoc pairwise comparisons Holms-Bonferroni correction
pwc7 <- Df %>%
  pairwise_t_test(Diastolic_Diameter ~ Intensity_Group, paired = F,
                  p.adjust.method	= "holm")
pwc7 %>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# Effect size Cohen's D with Hedge's g correction for small sample size
Df %>% cohens_d(Diastolic_Diameter ~ Intensity,
                paired = F, hedges.correction = TRUE)%>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")

#Plots
# Add position for p values in boxplot
pwc8 <- pwc7 %>% add_xy_position(x = "Intensity")
# Boxplot of ESS
DA <- ggboxplot(Df, x = "Intensity", y = "Diastolic_Diameter",
                color = "Group", palette = get_palette("Set1", 4),
                ylab = "Diastolic Diameter (mm)") +
  stat_pvalue_manual(pwc8,size = 4.5,hide.ns = TRUE) +
  theme_prism()




