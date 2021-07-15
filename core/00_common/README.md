# Common code

This folder contains code shared with every database.

Most importantly, it contains the `Mapping` base class, that must be subclassed
when writing the code to map your database to the data dictionnary.
This class is defined in the [mapping.R](./src/mapping.R) module.

## Adding a new database mapping

Create a new `mapping.R` file under `core/xx_your_database/src`. The raw data
must be located under `core/xx_your_database/data`.

``` R
# Create a MappingSein class based on Mapping
MappingSein <- setClass(
  "MappingSein",

  prototype=list(
    name_database = "sein",
    database = read.csv("/path/to/data.csv")
  ),

  contains = "Mapping"
)

# Override the methods to map the variables
setMethod(
  f="mapping_patient_id",
  signature="MappingSein",
  definition=function(self)
  {
    return(list(
      database = 1,
      numdos_curie = self@database$NUMDOS,
      cletri = "toto",
      side  = factor(self@database$cote, levels = c("G","D"), labels = c(1,2)),
      dat_birth = self@database$DATNAI,
      dat_bc_diagnosis = as.Date(ifelse(is.na(self@database$DATDIAG), self@database$datechir, self@database$DATDIAG), origin = "1970-01-01"),
      center_curie = factor(self@database$LIEUCHIRA, levels = c("I.C. Paris", "I.C. St Cloud","hors I.C."), labels = c(1,2,3)),
      center = ifelse(self@database$center_curie == 3,2,1),
      base_cletri = paste0(self@database$database, "_",self@database$cletri)
    ))
  }
)

```
