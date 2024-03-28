# Libraries
library(sdeTMB)

# Set File Location to Working Directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load Data


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

# Load Data
df = readRDS("example_data.rds")


model$set_initial_state(mean=df$Sbh[1], cov=5e-2^2*diag(1))
