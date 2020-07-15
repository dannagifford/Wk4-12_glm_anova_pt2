library(tidyverse) # Load the tidyverse packages
library(afex) # ANOVA functions
library(emmeans) # Needed for pairwise comparisons

#ANCOVA
my_data <- read_csv("https://raw.githubusercontent.com/ajstewartlang/12_glm_anova_pt2/master/data/ancova_data.csv")

my_data <- my_data %>% 
  mutate(Condition = factor(Condition))

ggplot(my_data, aes(x = Gaming, y = Ability,  colour = Condition)) + 
  geom_point(size = 3, alpha = .9) +
  labs(x = "Gaming Frequency (hours per week)", 
       y = "Motor Ability") +
  theme_minimal() +
  theme(text = element_text(size = 11)) 
  
anova_model <-aov_4(Ability ~ Condition + (1 | Participant), data = my_data)
anova(anova_model)

emmeans(anova_model, pairwise ~ Condition)

my_data <- my_data %>%
  mutate(Gaming = scale(Gaming))

model_ancova <- aov_4(Ability ~ Gaming + Condition + (1 | Participant), data = my_data, factorize = FALSE)
anova(model_ancova)

emmeans(model_ancova, pairwise ~ Condition)

my_data %>%
  group_by(Condition) %>%
  summarise(mean_ability = mean(Ability))

# AN(C)OVA as a special case of regression
my_data <- read_csv("https://raw.githubusercontent.com/ajstewartlang/12_glm_anova_pt2/master/data/ancova_data.csv")

my_data <- my_data %>% 
  mutate(Condition = factor(Condition))

my_data %>%
  ggplot(aes(x = Condition, y = Ability, colour = Condition)) +
  geom_violin() +
  geom_jitter(width = .05, alpha = .8) +
  labs(x = "Condition", 
       y = "Motor Ability") +
  stat_summary(fun.data = mean_cl_boot, colour = "black") +
  guides(colour = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 12)) 

my_data <- my_data %>%
  mutate(Condition = fct_relevel(Condition, 
                                 levels = c("Water", "Double Espresso", "Single Espresso")))

contrasts(my_data$Condition)

model_lm <- lm(Ability ~ Condition, data = my_data)
model_lm

# ANCOVA example
my_data <- read_csv("https://raw.githubusercontent.com/ajstewartlang/12_glm_anova_pt2/master/data/ancova_data.csv")

my_data <- my_data %>% 
  mutate(Condition = factor(Condition)) %>%
  mutate(Condition = fct_relevel(Condition, 
                                 levels = c("Water", "Double Espresso", "Single Espresso")))

model_ancova <- lm(Ability ~ Gaming + Condition, data = my_data)
model_ancova


