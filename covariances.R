getPearsonCor <- function(A, B){
  correlation = cor(A, B,
                    use = "complete.obs",
                    method = "pearson")
  return(correlation)
}

data <- train_data
# A Global_active_power
# B Global_reactive_power
# C voltage
# D global_intensity

corAB = getPearsonCor(data$Global_active_power,
                      data$Global_reactive_power)
corAC = getPearsonCor(data$Global_active_power,
                      data$Voltage)
corAD = getPearsonCor(data$Global_active_power,
                      data$Global_intensity)

corBC = getPearsonCor(data$Global_reactive_power,
                      data$Voltage)
corBD = getPearsonCor(data$Global_reactive_power,
                      data$Global_intensity)

corCD = getPearsonCor(data$Voltage,
                      data$Global_intensity)
