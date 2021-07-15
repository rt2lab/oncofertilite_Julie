#Define MappingImplants Class
library("readxl")

MappingImplants <- setClass(
  "MappingImplants",

  # Set the default values for the slots. (optional)
  prototype=list(
    name_database = "implants",
    database = read_excel(
      file.path(
        Sys.getenv("PROJECT_PATH"),
        "core/06_implants_seintinelles/data/SIM Donne&_769;es 20190821.xlsx"
      )
    )
  ),

  contains = "Mapping"
)

setMethod(f="mapping_patient_id",
          signature="MappingImplants",
          definition=function(self)
          {
            return(list(
                database <- 6,
                side <- self@database$q1_2a,  # 1=right, 2=left, 3=both
                dat_birth <- self@database$q1_1,
                dat_bc_diagnosis <- self@database$q1_2b
            ))
          }
)
