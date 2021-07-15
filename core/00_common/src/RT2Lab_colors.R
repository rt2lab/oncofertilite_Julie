# Good practices

# 1.          Please share your nice tips and pantone
# .........................................................

# Put the reference if relevant
# ex : 
# tumortype with response
# http://colorpalettes.net/color-palette-3687/
# leveltumortype2 <- c("PT_with_pCR","PT_with_RD","RD")
# coltumortype2   <- c("PT_with_pCR"="#00BA38","PT_with_RD"="#619CFF", "RD"="#F8766D")

# 2.          Use relevant and intuitive colors 
# .........................................................

##  Yes : green and No : Red  => the inverse is not considered as relevant
## NB :  By convention if you use red green blue, BC subtypes are represented as follows : luminal : blue ; TNBC : red ; HER2 : green


# 3. Use generic vector in your code after sourcing this file, rather than defining colors within the plot
# ..................................................................................................................
# ex :    p <- ggplot(aes(x = tumortype2_side, y = nbmutation)) + geom_bar(aes(fill = tumortype2_side)) + scale_fill_manual(values = coltumortype2_side) 


##########################################################################################################################################################################
# Beyond the line, just express yourself  ! 
##########################################################################################################################################################################

library(RColorBrewer)
brewer.pal(n = 11, name = "Paired")

#       MANDATORY COLORS 
# *-*-*-*-*-*-*-*-*-*-*-*-*-*

# Color for comedication project
color_anat          <- c("Alimentary_metabo"   = "#0DB14B",
                         "Cardiovascular"      = "#CC004C",
                         "Hormonal_prep"       = "#F37021",
                         "Nervous_system"      = "#0089D0",
                         "Musculo_skeletal"    = "#FCB711",
                         "Respiratory"         = "#6460AA",
                         "Others"              = "#868686")
# If 6th needed, here are the original [1] "#868686" "#0089D0" "#B70F1D" "#0DB14B" "#FCB711" "#F37021"



# Vector colors ASHP
# Nice colors
A_col <- "firebrick3"
B_col <- "darkorange"
C_col <- "deepskyblue3"
D_col <- "#868686"
E_col <- "#004d66"

nice_colors_5  = c(A_col, B_col, C_col,D_col,E_col)
nice_colors_4  = c(A_col, B_col, C_col,D_col)
nice_colors_3  = c(A_col, B_col, C_col)
nice_colors_2  = c(B_col, C_col)

# Color gradient red found from   firebrick3 #CD2626
gradient_colors_5      <- c("#d82e2e", "#e16060","#eb9191","#f4c3c3","#fdf4f4")
gradient_colors_4      <- c("#d82e2e", "#e16060","#eb9191","#f4c3c3")
# https://www.color-hex.com/color-palette/1294
gradient_colors_4_blue <- c("#03396c", "#005b96","#6497b1","#b3cde0")
gradient_colors_4_blue_light <- c("#6497b1","#92dff3","#b7e9f7", "#dbf3fa")

# Gradient color severity (green orange red)
# https://colorpalettes.net/color-palette-3372/
# cf folder color_palette

#e68618
#9f3a06 
gradient_colors_severity_3  <- c("#577e26","#e68618","#9f3a06") 


# Palette JCO
library(ggsci)
pal_jco("default")(10)
# [1] "#0073C2FF" "#EFC000FF" "#868686FF" "#CD534CFF" "#7AA6DCFF" "#003C67FF" "#8F7700FF" "#3B3B3BFF" "#A73030FF" "#4A6990FF"

# Find defaults color ggplot2
gg_color_hue <- function(n) {
                              hues = seq(15, 375, length=n+1)
                              hcl(h=hues, l=65, c=100)[1:n]
                              }

gg_color_hue(2) # [1] "#F8766D" "#00BFC4"
gg_color_hue(3) # [1] "#F8766D" "#00BA38" "#619CFF"

colsubtype_ggplot     <- c("Luminal"="#619CFF","TNBC"="#F8766D", "HER2+"="#00BA38")
colsubtype_ggplot_min <- c("luminal"="#619CFF","TNBC"="#F8766D", "HER2+"="#00BA38") # The only difference is the case (Luminal and luminal)

colsubtype_cvd     <- c("Luminal"="#0072B2",
                        "TNBC"="#D55E00", 
                        "HER2+"="#009E73")


col_ypnuicc_3cl             <- c("0"="#577e26","[1-3]"="#e68618", "4 and more"="#9f3a06")
col_ypnuicc_3cl_cvd         <- c("0"="#F0E442","[1-3]"="#E69F00", "4 and more"="#56B4E9")

col_ypnuicc_3cl_palettePaired <- c("0"="#f6d51f","[1-3]"="#fa8925", "4 and more"="#fa5457")
colsubtype_palettePaired     <- c("Luminal"="#01b4bc",
                                  "TNBC"="#fa5457", 
                                  "HER2+"="#5fa55a")

#5fa55a
#01b4bc
#f6d51f
#fa8925
#fa5457 
