
correction_root_neorep <- function(x){
  
  # Error subtypes etc..
  # Pour n° 0991593 : surexpression HER 2 en ville et a donc eu en neoadj de l'herceptin mais à Curie, reliquat tumoral HER 2 negatif en FISH 
  # En fait elle est positive.
  x[which(x$numdos7=="0991593"),"surex"]  <- 1
  
  # 2. Errors dates
  # *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
  
  # Corrections dates ENORA
  
  # See file pb with data Isambert 
  # @ 05/11/2018
  x$datexam[x$numdos7=="0886480"] <- "2008-08-19"
  x$datexam[x$numdos7=="0786136"] <- "2007-07-06"
  x$datexam[x$numdos7=="0885886"] <- "2008-07-28"
  x$datexam[x$numdos7=="0882051"] <- "2007-11-13"
  x$datexam[x$numdos7=="0782111"] <- "2007-03-13"
  x$datexam[x$numdos7=="0701911"] <- "2007-03-14"
  
  
  #correction xes xates xe neoaxj
  x$datdiag[x$numdos7=="0582779"] <- "2005-04-26"
  x$datdiag[x$numdos7=="0584324"] <- "2005-06-07"
  x$datdiag[x$numdos7=="0587902"] <- "2005-12-12"
  x$datdiag[x$numdos7=="0601719"] <- "2006-03-06"
  x$datdiag[x$numdos7=="0605045"] <- "2006-07-05"
  x$datdiag[x$numdos7== "0680230"] <- "2006-01-09"
  x$datdiag[x$numdos7==  "0685127"] <- "2006-07-21"
  x$datdiag[x$numdos7==  "0687301"] <- "2006-10-30"
  x$datdiag[x$numdos7==  "0789699"] <- "2007-11-26"
  x$datdiag[x$numdos7==  "0885936"]<- "2008-07-24"
  x$datdiag[x$numdos7==  "0888383"] <-"2008-10-15"
  x$datdiag[x$numdos7==  "0988245"] <- "2009-07-28"
  x$datdiag[x$numdos7==  "0991593"]<- "2009-09-19"
  x$datdiag[x$numdos7==  "1090413"] <- "2010-10-08"
  x$datdiag[x$numdos7==  "1107922"] <- "2011-04-27"
  x$datdiag[x$numdos7==  "1117028"] <- "2011-10-17" 
  x$datdiag[x$numdos7 =="0505400" ] <- "2005-06-28"
  x$datdiag[x$numdos7 =="0586576" ] <- "2005-10-18"
  x$datdiag[x$numdos7 =="0587519"  ] <- "2005-11-21"
  x$datdiag[x$numdos7 =="0587716"  ] <- "2005-12-05"
  x$datdiag[x$numdos7 =="0608458"  ]  <- "2006-11-27"
  x$datdiag[x$numdos7 =="0681200"  ] <- "2006-01-30"
  x$datdiag[x$numdos7 =="0685524"  ]  <- "2006-07-25"
  x$datdiag[x$numdos7 =="0688224"  ] <- "2006-07-12"
  x$datdiag[x$numdos7 =="0782639" ] <-"2007-04-03"
  x$datdiag[x$numdos7 =="0806700" ] <- "2008-09-12"
  x$datdiag[x$numdos7 =="1091970" ] <- "2010-11-19"
  x$datdiag[x$numdos7 =="1092325" ]  <- "2010-12-15"
  x$datdiag[x$numdos7 =="1104888" ] <- "2011-02-24"
  x$datdiag[x$numdos7 =="1117429" ]  <- "2011-10-26"
  x$datdiag[x$numdos7 =="1202033"]  <- "2012-01-19"
  x$datdiag[x$numdos7 =="1204974"]  <- "2012-03-14"
  
  #date de fin de CNA après chir
  x$neofin[x$numdos7=="0586765"]<-"2006-04-27"
  x$neofin[x$numdos7=="0808737"]<-"2009-05-25"
  x$neofin[x$numdos7=="0685524"]<-"2007-01-26"
  x$neofin[x$numdos7=="1007373"]<-"2011-02-25"
  # + 1additional seen by isambert
  x$neofin[x$numdos7=="0403072"]<-"2004-09-20"
  
  # wrong datchir for 8383819  : not 2013-07-19 but "2011-07-19" 
  x[x$numdos7 =="8383819","datchir"]  <- "2011-07-19" 
  # Attention patiente 0486879 -> dader et etader faux !!
  #   Patiente morte en  2006-12-05
  # or dader = 2011-01-14 (correspond test génétique) et etader = 0 = vivant
  x[x$numdos7 =="0486879","dader"]    <- "2006-12-05" 
  x[x$numdos7 =="0486879","etader"]    <- 1 
  
  # PB de delays manquants pour 2 ptes
  
  # 3. Subtype errors 
  # *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
  
  # Une erreur de RP : 0488386
  x[x$numdos7 =="0488386","proc" ]    <- 0
  x[x$numdos7 =="0488386","proc.f" ]  <- "0- n\xe9gatif"
  
  # 4. Particular patients pathways 
  # *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
  
  # 2 patients without surgery  : "0581968" "0580973"
  # 6 patients with preop RT    : # 0486521 # 0582821 # 0584134 # 0984882 # 0989663 # 8383819
  
  nip_without_surgery  <- c("0581968","0580973")
  nip_neoRT            <- c("0486521","0582821","0584134","0984882","0989663","8383819")
  # save(nip_without_surgery,file="data/processed/nip_without_surgery.RData")
  # save(nip_neoRT,file="data/processed/nip_neoRT")
  
  # Progressive sous chimio?
  # Date de progression avant chir?
  
  # Several patients with progression under NAC
  # 582821
  # 584134
  # 984882
  # 989663
  # 685524
  
  # 5 . Others (not done yet)
  # *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
  
      # Semblent avoir ete deja corriges
  
      # RCB reviewed by marick and diane decroze
            # TILs["0486870", c("new_RCB","new_RCB_class")    ]  <- c("2","1.876") 
            # TILs["0789131", c("new_RCB","new_RCB_class")    ]  <- c("2","1.786") 
            # TILs["0887964", c("new_RCB","new_RCB_class")    ]  <- c("2","2.071") 
  
            
  # PB : 
  #  3.287: RCB-II
  # to correct
  
  # x_2019 %>% filter(RCB==3.287) %>% select(numdos7) # 0686280
  # x_2019[x_2019$numdos7=="0686280","RCB_class"] <- "RCB-III"
  # 
  # x_RCB$tmp <- "tmp"
  
  return(x)
}          
          
          
          