library(tidyverse)
library(tinytex)
data <- read_csv("pk_data_R.csv")
View(data)

#summary(lm(I(log(Shot_prop))~I(log(conversion_rate))+I(log(.1+prop_nc_missed)),data=data))

data_lm <- read_csv("pk_data_shotlevel_chrono.csv")

cor(data_lm$conversion_rate_other,data_lm$prop_nc_missed_other)
ggplot(data=data_lm,aes(x=conversion_rate_other,y=prop_shots_other))+geom_smooth(method="lm",formula=y~I(x^2))
(pk_lm1 <- lm(prop_shots_other~prop_nc_missed_other+conversion_rate_other,data=data_lm))
#pk_lm2 <- lm(I(log(prop_shots_other))~I(log(1+prop_nc_missed_other))+I(log(conversion_rate_other)),data=data_lm)

(pk_lm2 <- lm(prop_shots_other~prop_nc_missed_other+conversion_rate_other+I(conversion_rate_other^2),data=data_lm))
(pk_lm3 <- lm(prop_shots_other~top+top_corner,data=data_lm))
(pk_lm4 <- lm(prop_shots_other~conversion_rate_other,data=data_lm))
(pk_lm5 <- lm(prop_shots_other~prop_nc_missed_other,data=data_lm))

summary(pk_lm1)
summary(pk_lm4)
summary(pk_lm5)

# library(rgl)
plot3d(data_lm$prop_nc_missed_other,data_lm$conversion_rate_other,data_lm$prop_shots_other,data=data_lm)

  # library(tidyverse)
# 
# library(plotly)
# library(plot3D)

# surf3D(as.matrix(lm_data$prop_nc_missed_other),as.matrix(lm_data$conversion_rate_other),as.matrix(lm_data$prop_shots_other))
# 
# fig <- plot_ly(data=lm_dat,x=~prop_nc_missed_other,y=~conversion_rate_other,z=~prop_shots_other) 
# 
# fig <- add_trace(p=fig,z=lm_dat$prop_shots_other,x=lm_dat$prop_nc_missed_other,y=lm_dat$conversion_rate_other,type="surface")
# fig
# 
# fig %>%  add_trace(p = iris_plot,
#                        z = petal_lm_surface,
#                        x = axis_x,
#                        y = axis_y,
#                        type = "surface")
# 
# fig %>% add_surface()
# fig
summary(data_lm$prop_shots_other)

summary(pk_lm1)
summary(pk_lm2)
summary(pk_lm3)
library(stargazer)
stargazer(pk_lm1,pk_lm2)

y <- data_lm$prop_shots_other
x <- data_lm %>% select(conversion_rate_other,prop_nc_missed_other) %>% data.matrix()
lambda_seq <- 10^seq(-6, 2, by = .3)
# Using glmnet function to build the ridge regression in r

library(glmnet)

lasso_pk <- cv.glmnet(x, y, alpha = 1, lambda = lambda_seq)
coef(lasso_pk,s=lasso_pk$lambda.min)
lasso_pk$lambda.min
CI <- boot.glmnet(x, y, lambda=lasso_pk$lambda.min)
plot(lasso_pk)


library(selectiveInference)
lambda = .01
beta = coef(lasso_pk, x=x, y=y, s=lambda/536, exact=TRUE)[-1]

# compute fixed lambda p-values and selection intervals
out = fixedLassoInf(x,y,beta,lambda,sigma=NULL)

