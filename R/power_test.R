# installing
install.packages("Superpower")
install.packages("pwr")

# loading
library(Superpower)
library(pwr)

# custom function
short_cut <- function(d, method = c("normal", "interaction")){
	method <- match.arg(method)
    if(method == "normal"){
    size <- 16*(1/d^2)}
    else{
        size <- 32*(1/d^2)
    }
    size
}
##################
# power caculation
##################

# 2 indepedent group situation (assumed)

# female control: mean = 100, sd = 30
# female experiment: mean = 120, sd = 30
# male control: mean = 100, sd = 30
# male experiment: mean = 130, sd = 30

# pooled sd = 30 # as all sd are 30 - otherwise you need to get weighed average via sd^2 (variance)

# cohen's d female
(120 - 100)/30

# conhen's d male
(130 - 100)/30

# cohen's d sex difference sex difference
((130 - 100) - (120 - 100))/30


# the first set (surprising large effects)

## male treatment effect
pwr.t.test(d = 1, power = 0.8)
short_cut(d = 1, method = "normal")

## female treatment effect
pwr.t.test(d = 0.67, power = 0.8)
short_cut(d = 0.67, method = "normal")

# sex difference = INCORRECT!!
pwr.t.test(d = 0.33, power = 0.8) # incorrect
short_cut(d = 0.33) # incorrect
short_cut(d = 0.33, method = "interaction")

# the second set (realstic samll effect)
# female control: mean = 100, sd = 30
# female experiment: mean = 103, sd = 30
# male control: mean = 100, sd = 30
# male experiment: mean = 105, sd = 30

# pooled sd = 30 # as all sd are 30 - otherwise you need to get weighed average via sd^2 (variance)

# cohen's d female
(103 - 100)/30

# conhen's d male
(105 - 100)/30

# cohen's d sex difference sex difference
((105 - 100) - (103 - 100))/30


# the first set (surprising large effects)

## male treatment effect
pwr.t.test(d = 0.167, power = 0.8)
short_cut(d = 0.167, method = "normal")

## female treatment effect
pwr.t.test(d = 0.1, power = 0.8)
short_cut(d = 0.1, method = "normal")

# sex difference = INCORRECT!!
pwr.t.test(d = 0.067, power = 0.8) # incorrect
short_cut(d = 0.067) # incorrect
short_cut(d = 0.067, method = "interaction")

#############
# interaction
#############

## scenario 1: large effects

design <- ANOVA_design(
  design = "2b*2b", # indepdent design
  n =286, 
  mu = c(130, 100, 120, 100), 
  sd = 30)

ANOVA_exact(design, alpha_level = 0.05)

ANOVA_power(design, alpha = 0.05,nsims = 1000, seed = 1234)

# assuming the siblings are very similar to each other - r = 0.5
design2 <- ANOVA_design(
  design = "2w*2w", # depedent design
  n =143, 
  r = 0.5,
  mu = c(130, 100, 120, 100), 
  sd = 30)

ANOVA_exact(design2, alpha_level = 0.05)

# assuming the siblings are very similar to each other - r = 0.25
design3 <- ANOVA_design(
  design = "2w*2w", # depedent design 
  n =215, 
  r = 0.25,
  mu = c(130, 100, 120, 100), 
  sd = 30)

ANOVA_exact(design3, alpha_level = 0.05)

## scenario 2: small effects 

design <- ANOVA_design(
  design = "2b*2b", 
  n =7128,  # indepedent design
  mu = c(105, 100, 103, 100), 
  sd = 30)

ANOVA_exact(design, alpha_level = 0.05)

# assuming the siblings are very similar to each other - r = 0.5
design2 <- ANOVA_design(
  design = "2w*2w", # depedent design
  n = 3564, 
  r = 0.5,
  mu = c(105, 100, 103, 100), 
  sd = 30)

ANOVA_exact(design2, alpha_level = 0.05)

# assuming the siblings are very similar to each other - r = 0.25
design3 <- ANOVA_design(
  design = "2w*2w", # depedent design 
  n = 5346, 
  r = 0.25,
  mu = c(105, 100, 103, 100), 
  sd = 30)
ANOVA_exact(design3, alpha_level = 0.05)
