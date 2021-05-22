# The success of ornamental heather (Calluna vulgaris) production 
# is largely dependent on monitoring strategies within crop management.
# In the following simulation we compare three monitoring strategies in a 
# cost-benefit analysis: The currently applied monitoring strategy (Baseline); 
# More intensive monitoring (Improved); and the acquisition of a sensor for additional 
# verification of plant vitality based on hyperspectral data (Sensor).

library(decisionSupport)

input_table <- "Input_CBA.csv"
legend_file <- "Legend_CBA.csv"
MC_Results_folder <- "MC_Results_CBA"
EVPI_Results_folder <- "EVPI_Results_CBA"

# For internal checking model without running the function ####
make_variables <- function(est,n = 1)
{ x <- random(rho = est, n = n)
for(i in colnames(x)) assign(i, as.numeric(x[1,i]),envir = .GlobalEnv)}
make_variables(estimate_read_csv(input_table))
dir.create(EVPI_Results_folder)


source("./Simulation_Function.R") # The Simulation function for simulation 
# of 'Baseline', 'Improved' and 'Sensor' monitoring.

decisionSupport(inputFilePath = input_table, #input file with estimates
                outputPath = MC_Results_folder, #output folder
                welfareFunction = Simulation, 
                write_table = TRUE,
                numberOfModelRuns = 10000, 
                functionSyntax = "plainNames")

# Produce EVPI tables
MC_file <- read.csv(paste(MC_Results_folder,"/mcSimulationResults.csv",sep = ""))

# Create multi_EVPI
MC_file_without_cashflow <- select(MC_file, -c(1, starts_with("cashflow")))
multi_EVPI(mc = MC_file_without_cashflow,first_out_var = "NPV_G",
           write_table = TRUE,outfolder = EVPI_Results_folder)

# Check welfareDecisionSummary
welfare_summary <- read.csv(paste(MC_Results_folder,"/welfareDecisionSummary.csv",sep = ""))

# End of Decision Analysis Calculations ####