# uncomment to install the sdeTMB package from GitHub
# remotes::install_github(repo="phillipbvetter/sdeTMB", dependencies=TRUE)

# Libraries
library(sdeTMB)
library(ggplot2)
library(patchwork)
library(dplyr)

# Set File Location to Working Directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load Data
df = readRDS("example_data.rds")

# Create Model
model = sdeTMB$new()
model$set_modelname("sbh_model")
model$add_systems(dx ~ tau * (mu-x) * dt + sigma_x * dw1)
model$add_observations(Sbh ~ x)
model$add_observation_variances(Sbh ~ sigma_y^2)

model$add_algebraics(
  tau ~ 1/exp(logtheta),
  mu ~ invlogit(b0 + b1*Qf*Sf + b2*Qr)*3.5,
  sigma_x ~ exp(logsigma_x),
  sigma_y ~ exp(logsigma_y)
)
model$add_parameters(
  logtheta = log(c(1, 1/6, 24)),
  b0 = c(1e-5,-100,100),
  b1 = c(1e-5,-100,100),
  b2 = c(1e-5,-100,100),
  logsigma_x = log(c(1e-2, 1e-10, 1)),
  logsigma_y  = log(5e-2)
)
model$add_inputs(Sf, Qr, Qf)
model$set_initial_state(mean=df$Sbh[1], cov=5e-2^2*diag(1))

# Estimate Parameters
fit = model$estimate(data=df, use.hessian=TRUE)

# Full Predict (no state update)
pred = model$predict(data=df, k.ahead=1e6)

# PLOTTING
# PLOTTING
# PLOTTING

# load ggplot theme and colors
source("ggplot_settings.R")

# Prediction plot with 95% confidence interval
p1 = ggplot() + 
  geom_ribbon(data=pred$states, aes(x=t.j, ymin=x-2*sqrt(var.x),ymax=x+2*sqrt(var.x)), fill="grey",alpha=0.75) + 
  geom_line(data=pred$states, aes(x=t.j, y=x, color="Predictions")) + 
  geom_point(data=pred$observations,aes(x=t.j, y=Sbh.data, color="Observations"), size=0.5) +
  labs(color="",x="Time [Hours]", y="Sludge Blanket Height [Meters]") +
  mytheme

# Predicton plot for 2 and 4 hour predictions
pred2 = model$predict(data=df, k.ahead=6*4, return.k.ahead=c(6*2,6*4))
pred.2hours = pred2$states %>% filter(k.ahead==6*2)
pred.4hours = pred2$states %>% filter(k.ahead==6*4)
obs = pred2$observations %>% filter(k.ahead==6*4)
p2 = ggplot() +
  geom_line(data=pred.2hours, aes(x=t.j,y=x,color="2 Hour Prediction")) +
  geom_line(data=pred.4hours, aes(x=t.j,y=x,color="4 Hour Prediction")) +
  geom_point(data=obs, aes(x=t.j,y=Sbh.data,color="Observations"), size=0.5) +
  labs(color="",x="Time [Hours]", y="Sludge Blanket Height [Meters]") +
  mytheme

# Wrap plots
patchwork::wrap_plots(p1,p2,nrow=2)
