library(VisualStats)

data(hand_washing, package = 'VisualStats')

VisualStats::describe_by(hand_washing$Bacterial_Counts, group = hand_washing$Method)

aov(Bacterial_Counts ~ Method, data = hand_washing) |> summary()
lm(Bacterial_Counts ~ Method, data = hand_washing) |> summary()

model.matrix(Bacterial_Counts ~ Method, data = hand_washing) |> head()

relevel(hand_washing$Method)

library(interactions)
lm_out <- lm(depression ~ anxiety * affect, data = depression)
interactions::interact_plot(lm_out, pred = anxiety, modx = affect)
interactions::interact_plot(lm_out, pred = affect, modx = anxiety)



data("poverty")
lm_out <- lm(poverty ~ female_house + white, data = poverty)
summary(lm_out)

anova_out <- anova(lm_out)
anova_out
ss_total <- sum((poverty$poverty - mean(poverty$poverty))^2)
ss_total

anova_out$`Sum Sq` / ss_total

lm(poverty ~ female_house, data = poverty) |> summary()
lm(poverty ~ white, data = poverty) |> summary()

