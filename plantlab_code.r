# ecological data on the pollination of flowering plants 

install.packages("readxl")
library(readxl)

# load data from excel
lamium <- read_excel("C:/RStudio/plantlab/poll_lim.xlsx")
summary(lamium)


# calculate ratio (actual number of seeds/max nr of seeds that this species can produce) to make plots easier to interpret
Ratio <- lamium$Success/4
Ratio

install.packages("multcomp")
library(multcomp)

# create a boxplot for visualization: 
pdf("Seed_ratio.pdf",
    width = 8, height = 7, # Width and height in inches
    bg = "white",          # Background color
    paper = "A4")
plot_ratio <- boxplot((Ratio[lamium$Test == "PS"]), (Ratio[lamium$Test == "N"]), 
                      names = c("Pollen Supplementation", "Natural"), 
                      main = "Seeds/Tot Seeds")
dev.off()

t.test(Ratio[lamium$Test == "PS"], Ratio[lamium$Test == "N"])
# p-value is significant with 0.02, therfore the alternative hypothesis is accepted, the two are signficantly different

# we create a general linear model to compare seed development success/failure
model <- glm(cbind(lamium$Success, Failure) ~ lamium$Test, family = binomial, data = lamium)
summary(model)

ods <- model$deviance/model$df.residual
ods # 3.23 this is to big so adjust family to "quasibinomial"
# if >1 than the model is overdispersed so we need to adjust

model2 <- glm(cbind(lamium$Success, Failure) ~ lamium$Test, family = quasibinomial, data = lamium)
ods2 <- model2$deviance/model2$df.residual
ods2 # still 2.796?? shouldn't it be smaller?


model3 <- glm(cbind(Seeds, Ovule_aborted) ~ Test, data = lamium, family = binomial)
summary(model3)




# use posthoc test to define between which groups differences are found, 
install.packages("emmeans")
library(emmeans)
pairs(emmeans(model2, ~ Test)) # since we only have 2 obviously the differences are found between these

