# set locale
Sys.setlocale("LC_ALL", 'en_US.UTF-8')

source("path/to/R experiments/create-position-info-LAKE-assamese.R")
source("path/to/R experiments/compute-sigma-index-LAKE-assamese.R")
source("path/to/R experiments/Create-graph-LAKE-assamese.R")
source("path/to/R experiments/adjmat-to-edgelist-LAKE-assamese.R")
### Run Python script "path/to/Python scripts/InfluenceEvaluation.py". Use Python 2.x to execute.
### Go to the directory and run
### python InfluenceEvaluation.py "/path/to/file/Graphs/edgelists/"
source("path/to/R experiments/Word-score-with-PositionWeight-Assamese.R")

