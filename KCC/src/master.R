
# Set PROJECT HOME FOLDER 
PROJECT_HOME <- file.path("C:", "Users", "604220", "Documents", "6.Projects", "KCC")
Sys.setenv(RSTUDIO_PANDOC="C:/Users/604220/Documents/RStudio/bin/pandoc")

# Load packages and custom functions
source(file.path(PROJECT_HOME, "src", "util.R"))

# Create log files
logger <- create.logger(logfile = file.path(PROJECT_HOME, "logs", 'error.log'), level = log4r:::INFO, logformat="%r [%t] %-5p %c %x - %m%n")
log4r::info(logger, "Master file started")



# Run scripts in order
source(file.path(PROJECT_HOME, "src", "data", "100_dataLoad.R"))

log4r::info(logger, "data loading finished")

source(file.path(PROJECT_HOME, "src", "data", "110_dataprep_CVdata.R"))
source(file.path(PROJECT_HOME, "src", "data", "115_dataprep_CV15data.R"))
source(file.path(PROJECT_HOME, "src", "data", "116_dataprep_CV15_WGdata.R"))
source(file.path(PROJECT_HOME, "src", "data", "120_dataprep_bankholidaydata.R"))
source(file.path(PROJECT_HOME, "src", "data", "130_dataprep_GSAdata.R"))

log4r::info(logger, "data preparation finished")

source(file.path(PROJECT_HOME, "src", "data", "140_datamerging.R"))

log4r::info(logger, "data merging finished")

# source(file.path(PROJECT_HOME, "src", "model", "200_train_test_partition.R"))
# source(file.path(PROJECT_HOME, "src", "model", "202_Model_training.R"))
# 
# log4r::info(logger, "model training finished")
# 
# source(file.path(PROJECT_HOME, "src", "data", "301_model_performance.R"))
# 
# log4r::info(logger, "model validation finished")
# 
# # Render markdown reports
# rmarkdown::render(file.path(PROJECT_HOME, "src", "visualisation", "diagnostics_dataquality.rmd"), output_format = "html_document")
# rmarkdown::render(file.path(PROJECT_HOME, "src", "visualisation", "Exploration_data.rmd"), output_format = "html_document")
# 
# rmarkdown::render(file.path(PROJECT_HOME, "reports", "Business_Understanding.rmd"), output_format = "html_document")
# rmarkdown::render(file.path(PROJECT_HOME, "reports", "Data_Understanding.rmd"), output_format = "html_document")
# rmarkdown::render(file.path(PROJECT_HOME, "reports", "Data_Preparation.rmd"), output_format = "html_document")
# rmarkdown::render(file.path(PROJECT_HOME, "reports", "Data_Modeling.rmd"), output_format = "html_document")
# rmarkdown::render(file.path(PROJECT_HOME, "reports", "Evaluation.rmd"), output_format = "html_document")
# rmarkdown::render(file.path(PROJECT_HOME, "reports", "Deployment.rmd"), output_format = "html_document")
# rmarkdown::render(file.path(PROJECT_HOME, "reports", "Executive_Summary.rmd"), output_format = "html_document")
log4r::info(logger, "Master file completed")
