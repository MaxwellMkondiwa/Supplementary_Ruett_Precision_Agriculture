library(dplyr)
# For Baseline Monitoring
data_G <- read.csv("mcSimulationResults.csv")
data_G <- dplyr::select(data_G, starts_with("NPV_G")) %>%
  stack(drop=FALSE)
data_G$values <- as.numeric(data_G$values)

# For Improved Monitoring
data_I <- read.csv("mcSimulationResults.csv")
data_I <- dplyr::select(data_I, starts_with("NPV_I")) %>%
  stack(drop=FALSE)
data_I$values <- as.numeric(data_I$values)

# For Sensor Monitoring
data_S <- read.csv("mcSimulationResults.csv")
data_S <- dplyr::select(data_S, starts_with("NPV_S")) %>%
  stack(drop=FALSE)
data_S$values <- as.numeric(data_S$values)

# Start risk premium calculation
risk_premium_G <-(1/2)*0.0001*var(data_G$values)

risk_premium_I <-(1/2)*0.0001*var(data_I$values)

risk_premium_S <-(1/2)*0.0001*var(data_S$values)

# Calculation of the semivariance
for(z in 1 : 10000){
  data_G$NewColumn[z] <- ifelse(min((data_G$values[z]- mean(data_G$values)),0) < 0,
                                (data_G$values[z]- mean(data_G$values))^2, 0)
}
Semivaricance_G <- mean(data_G$NewColumn)

for(z in 1 : 10000){
  data_I$NewColumn[z] <- ifelse(min((data_I$values[z]- mean(data_I$values)),0) < 0,
                                (data_I$values[z]- mean(data_I$values))^2, 0)
}
Semivaricance_I <- mean(data_I$NewColumn)

for(z in 1 : 10000){
  data_S$NewColumn[z] <- ifelse(min((data_S$values[z]- mean(data_S$values)),0) < 0,
                                (data_S$values[z]- mean(data_S$values))^2, 0)
}
Semivaricance_S <- mean(data_S$NewColumn)

# Start a loop to compute certainty equivalents for all risk aversion coefficients
for(r in c(-1e-01, -1e-02, -1e-03, -1e-04, -1e-05, -1e-06, 0,
           1e-06,  1e-05, 1e-04, 1e-03, 1e-02, 1e-01)){
  Certainty_equivalent_G_ <- mean(data_G$values) - Semivaricance_G * r
  assign(paste("CE_G_", r, sep = ""), Certainty_equivalent_G_)
  
  Certainty_equivalent_I_ <- mean(data_I$values) - Semivaricance_I * r
  assign(paste("CE_I_", r, sep = ""), Certainty_equivalent_I_)
  
  Certainty_equivalent_S_ <- mean(data_S$values) - Semivaricance_S * r
  assign(paste("CE_S_", r, sep = ""), Certainty_equivalent_S_)
}

Certainty_equivalent <- c(`CE_G_-0.1`, `CE_G_-0.01`, `CE_G_-0.001`, `CE_G_-1e-04`,
                          `CE_G_-1e-05`, `CE_G_-1e-06`,`CE_G_0`, `CE_G_1e-06`,
                          `CE_G_1e-05`, `CE_G_1e-04`, `CE_G_0.001`, `CE_G_0.01`,
                          `CE_G_0.1`, `CE_I_-0.1`, `CE_I_-0.01`, `CE_I_-0.001`, 
                          `CE_I_-1e-04`, `CE_I_-1e-05`, `CE_I_-1e-06`, `CE_I_0`,
                          `CE_I_1e-06`, `CE_I_1e-05`, `CE_I_1e-04`, `CE_I_0.001`,
                          `CE_I_0.01`, `CE_I_0.1`,`CE_S_-0.1`, `CE_S_-0.01`, `CE_S_-0.001`,
                          `CE_S_-1e-04`, `CE_S_-1e-05`, `CE_S_-1e-06`,`CE_S_0`, `CE_S_1e-06`,
                          `CE_S_1e-05`, `CE_S_1e-04`, `CE_S_0.001`, `CE_S_0.01`, `CE_S_0.1`)

monitoring_type <- c(rep("Baseline", 13),rep("Improved", 13),rep("Sensor", 13))

risk_aversion_coefficient <- c(-1e-01, -1e-02, -1e-03, -1e-04, -1e-05, -1e-06, 0, 1e-06,
                               1e-05, 1e-04, 1e-03, 1e-02, 1e-01)

options(scipen = 999)
risk_aversion_data <- data.frame(Certainty_equivalent, risk_aversion_coefficient, monitoring_type)