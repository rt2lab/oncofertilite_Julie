
var_selected		<-  c("age",	"menop.f","bmi","bmi.c2.f","tclin","tuicc2.f", "N.f","Index_mitotique","grade.c.f","ki67.c.f",
"typana_medul","subtype.f","roec.f","proc.f","her2.f","perc_stromal_lymphocytes","perc_TIL","typneo.f")
names_var_selected	<-  c("Age",	"Menopausal status","BMI (continuous)","BMI class", "Tumor stage","Tumor size","Clinical nodal status",
"Mitotic index","Tumor Grade","ki67","Histology","BC subtype","ER status","PR status","HER2 status","Pre-NAC str TILs","Pre-NAC  IT TILs","NAC regimen")

matching_name_file    <- data.frame(variable = var_selected, 
                                    name_variable = names_var_selected) ; head(matching_name_file)
# variable     name_variable
# 1      age               Age
# 2  menop.f Menopausal status
# 3      bmi  BMI (continuous)
# 4 bmi.c2.f         BMI class
# 5    tclin       Tumor stage
# 6 tuicc2.f        Tumor size

mydataset             <- neo_tabac  ; head(mydataset)
#         N.f Ninit Ninit.f cytn2.f cAJCC      bmi obese obese.f bmi.c     bmi.c.f bmi.c2      bmi.c2.f bmi.c3 bmi.c3.f grade
# 0788039       N0     1      N-    <NA>   IIA 24.09297     0 BMI<=30     1 19<=BMI<=25      1 BMI: 19 to 25      0 BMI < 25     3
# 0683466       N0     1      N-    <NA>   IIA 20.70312     0 BMI<=30     1 19<=BMI<=25      1 BMI: 19 to 25      0 BMI < 25     3
# 0682949 N1-N2-N3     2      N+    <NA>  IIIA 22.83288     0 BMI<=30     1 19<=BMI<=25      1 BMI: 19 to 25      0 BMI < 25     2
# 8884765       N0     1      N-    <NA>   IIA 19.37920     0 BMI<=30     1 19<=BMI<=25      1 BMI: 19 to 25      0 BMI < 25     3
# 0587902 N1-N2-N3     2      N+    <NA>   IIB 24.21875     0 BMI<=30     1 19<=BMI<=25      1 BMI: 19 to 25      0 BMI < 25     2
# 0886480       N0     1      N-    <NA>     I 20.79673     0 BMI<=30     1 19<=BMI<=25      1 BMI: 19 to 25      0 BMI < 25     3

# 1. Table1 by smoking status
Table1                <- CreateTableOne(var_selected , "tabac_3_classes",mydataset) 
table1_preformat      <- print(Table1, quote=TRUE, noSpaces=TRUE,showAllLevels = TRUE, pDigits=3, contDigits=1)
      # "Stratified by tabac_3_classes"
      # ""                                      "level"                  "current"     "ever"        "never"       "p"     "test"
      # "n"                                    ""                       "179"         "154"         "623"         ""      ""    
      # "age (mean (SD))"                      ""                       "46.5 (8.8)"  "46.3 (9.7)"  "48.8 (10.5)" "0.002" ""    
      # "menop.f (%)"                          "premenopausal"          "127 (72.2)"  "108 (70.1)"  "385 (61.9)"  "0.015" ""    
      # ""                                     "postmenopausal"         "49 (27.8)"   "46 (29.9)"   "237 (38.1)"  ""      ""    
      # "bmi (mean (SD))"                      ""                       "24.2 (4.8)"  "23.9 (4.3)"  "25.1 (4.7)"  "0.006" ""    

                      # => unfriendly format, not ready to csv, ugly variable names

source('~/RT2Lab/R/R functions/format_tableone.R', chdir = TRUE)
Table1_by_tabac <- Table1_format 
# names_variable_clean                  level     current        ever       never     p test
# 1                                             n         179         154         623           
# 2                    Age                         46.5 (8.8)  46.3 (9.7) 48.8 (10.5) 0.002     
# 3      Menopausal status          premenopausal  127 (72.2)  108 (70.1)  385 (61.9) 0.015     
# 4                                postmenopausal   49 (27.8)   46 (29.9)  237 (38.1)           
# 5       BMI (continuous)                         24.2 (4.8)  23.9 (4.3)  25.1 (4.7) 0.006     

                        # => friendly format, ready to csv, pretty variable names

# 2. Table 1 in whole population
Table1            <- CreateTableOne(var_selected, data=mydataset) 
table1_preformat  <- print(Table1, quote=TRUE, noSpaces=TRUE,showAllLevels = TRUE)
source('~/RT2Lab/R/R functions/format_tableone.R', chdir = TRUE)
Table1_WP <- Table1_format

# 3. Compil both tables 
Table1_WP_and_by_tabac <- full_join(Table1_WP,Table1_by_tabac, by = c("names_variable_clean","level"))

# 4. Add in legend vector_missing_data
vector_missing_data
# [1] "Missing data: Menopausal status, n=4; Tumor stage, n=1; Tumor size, n=1; Clinical nodal status, n=1; Mitotic index, n=338;
# Tumor Grade, n=28; ki67, n=565; Histology, n=10; PR status, n=23; Pre-NAC str TILs, n=324; Pre-NAC  IT TILs, n=324"
