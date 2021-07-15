#Clear existing data and graphics
rm(list=ls())
graphics.off()
#Load Hmisc library
library(Hmisc)
#Read Data
PROJECT_PATH <- "/Users/bea/GitHUB/databases"
data=read.csv(file.path(PROJECT_PATH, "core/17_sein_tabac/data", 'Tabac_DATA_2021-01-10_2228.csv'))
#Setting Labels

label(data$record_id)="IPP"
label(data$date_consult)="Consultation day "
label(data$dat_birth)="Birth date "
label(data$habitus)="Habitus"
label(data$niv_etu)="Highest educationnal level"
label(data$csp)="Profession at diagnosis"
label(data$age_at_surg)="Age "
label(data$weight)="Weight "
label(data$size)="Size"
label(data$stat_mar)="Marital status"
label(data$tobac_mention)="Tobacco mentionned in the medical report "
label(data$tabac_life)="Tobacco (at least 100 cig in life)  "
label(data$smoking)="Currently smoking"
label(data$tobac_diag)="Tobacco at diagnosis "
label(data$age_tobac_start)="Age of tobacco beginning  "
label(data$tobac_years)="Number of year of smoking  "
label(data$nb_cig_life)="Average number of cig per day  "
label(data$age_tobac_stop)="Age at smoking cessation "
label(data$recent_smoker)="Smoking cessationg during the year before diagnosis  "
label(data$alcool)="Alcohol (nb glasses / week)  "
label(data$drogue)="Drugs (cannabis, others, etc) "
label(data$comm_drogue)="Commentaires drogues"
label(data$sport)="Physical activity"
label(data$comm_sport___1)="Duration of daily physical activity  (choice=< 30 min/d)"
label(data$comm_sport___2)="Duration of daily physical activity  (choice=> 30 min/d)"
label(data$in_situ_exclus)="Carcinome In situ exclusif"
label(data$statut_tn)="Statut TN clinique"
label(data$type_histo___0)="Type histologique (choice=CCI)"
label(data$type_histo___1)="Type histologique (choice=CLI)"
label(data$type_histo___2)="Type histologique (choice=CCIS)"
label(data$type_histo___3)="Type histologique (choice=CLIS)"
label(data$type_histo___4)="Type histologique (choice=Others)"
label(data$bifocal)="Multifocal"
label(data$rec_est___0)="Récepteur estrogène (choice=positif)"
label(data$rec_est___1)="Récepteur estrogène (choice=négatif)"
label(data$rec_est___2)="Récepteur estrogène (choice=na)"
label(data$recep_prog___0)="Récepteur progesterone (choice=positif)"
label(data$recep_prog___1)="Récepteur progesterone (choice=négatif)"
label(data$recep_prog___2)="Récepteur progesterone (choice=na)"
label(data$stat_her2___0)="Statut Her2 (choice=positif)"
label(data$stat_her2___1)="Statut Her2 (choice=négatif)"
label(data$stat_her2___2)="Statut Her2 (choice=na)"
label(data$grade___0)="Grade histologique (choice=1)"
label(data$grade___1)="Grade histologique (choice=2)"
label(data$grade___2)="Grade histologique (choice=3)"
label(data$grade___3)="Grade histologique (choice=na)"
label(data$embole___0)="Emboles (choice=oui)"
label(data$embole___1)="Emboles (choice=non)"
label(data$embole___2)="Emboles (choice=na)"
label(data$nb_gg)="Nombre ganglion positif"
label(data$ttt_kr___0)="Traitements recus (QCM)  (choice=chirurgie)"
label(data$ttt_kr___1)="Traitements recus (QCM)  (choice=chimiotherapie)"
label(data$ttt_kr___2)="Traitements recus (QCM)  (choice=radiotherapie)"
label(data$ttt_kr___3)="Traitements recus (QCM)  (choice=trastuzumab)"
label(data$ttt_kr___4)="Traitements recus (QCM)  (choice=hormonotherapie)"
label(data$ttt_kr___5)="Traitements recus (QCM)  (choice=reconstruction mammaire)"
label(data$hormono_neo)="Hormonothérapie néo-adjuvant"
label(data$date_hormono)="date début hormono néo adj"
label(data$seq_chimio)="Séquence chimiothérapie"
label(data$date_chimio)="Date début chimio néo adj"
label(data$taille_infiltrant)="Taille de la tumeur (mm)"
label(data$reponse_cna)="Réponse a la chimiothérapie neoadjuvant"
label(data$chir)="Chirurgie sein "
label(data$date_chir)="Date de chirugie"
label(data$reconst)="Reconstruction mammaire differee "
label(data$anes_tabac)="Smoking status asked by anesthesiologist  "
label(data$info_tabac_anes___0)="Info smoking anesth  (choice=info complications)"
label(data$info_tabac_anes___1)="Info smoking anesth  (choice=info cessation needed)"
label(data$info_tabac_anes___2)="Info smoking anesth  (choice=advises and orientation)"
label(data$chir_tabac)="Smoking status asked by surgeon  "
label(data$info_tabac_chir___0)="info smoking surgeon  (choice=info complications)"
label(data$info_tabac_chir___1)="info smoking surgeon  (choice=info cessation needed)"
label(data$info_tabac_chir___2)="info smoking surgeon  (choice=advises and orientation)"
label(data$rt_tabac)="Smoking status asked by radiotherapist  "
label(data$info_tabac_rt___0)="Info smoking RT   (choice=info complications)"
label(data$info_tabac_rt___1)="Info smoking RT   (choice=info cessation needed)"
label(data$info_tabac_rt___2)="Info smoking RT   (choice=advises and orientation)"
label(data$onco_tabac)="Smoking status asked by oncologist  "
label(data$info_tabac_onco___0)="Info smoking onco  (choice=info complications)"
label(data$info_tabac_onco___1)="Info smoking onco  (choice=info cessation needed)"
label(data$info_tabac_onco___2)="Info smoking onco  (choice=advises and orientation)"
label(data$moyen_sevrage___0)="Methods used for smoking cessation  (choice=tobacco consultation)"
label(data$moyen_sevrage___1)="Methods used for smoking cessation  (choice=psychological support)"
label(data$moyen_sevrage___2)="Methods used for smoking cessation  (choice=cognitive behavioral therapy)"
label(data$moyen_sevrage___3)="Methods used for smoking cessation  (choice=nicotine substitutes)"
label(data$moyen_sevrage___4)="Methods used for smoking cessation  (choice=pharmaco)"
label(data$moyen_sevrage___5)="Methods used for smoking cessation  (choice=hypnosis)"
label(data$moyen_sevrage___6)="Methods used for smoking cessation  (choice=acupuncture)"
label(data$tabac_surv)="Tobacco status "
label(data$duree_sevrage)="How many month without smoking since diagnosis  "
label(data$nb_cig_surv)="Average number of cig /d  "
label(data$methode_sevrage___0)="Methods used for smoking cessation  (choice=tobacco consultation)"
label(data$methode_sevrage___1)="Methods used for smoking cessation  (choice=psychological support)"
label(data$methode_sevrage___2)="Methods used for smoking cessation  (choice=cognitive behavioral therapy)"
label(data$methode_sevrage___3)="Methods used for smoking cessation  (choice=nicotine substitutes)"
label(data$methode_sevrage___4)="Methods used for smoking cessation  (choice=pharmaco)"
label(data$methode_sevrage___5)="Methods used for smoking cessation  (choice=hypnosis)"
label(data$methode_sevrage___6)="Methods used for smoking cessation  (choice=acupuncture)"
label(data$motivation_sevrage___0)="Motivation for smoking cessation  (choice=recurrence or other cancer fear)"
label(data$motivation_sevrage___1)="Motivation for smoking cessation  (choice=complications fear)"
label(data$motivation_sevrage___2)="Motivation for smoking cessation  (choice=breast reconstruction desire)"
label(data$motivation_sevrage___3)="Motivation for smoking cessation  (choice=other)"
label(data$autre_motivation_sevrage)="Other motivation for smoking cessation "
label(data$recidiv_cancer)="Recurrence after treatment "
label(data$poids_actuel)="Actual weight "
label(data$comment)="Comments"
label(data$questionnaire_tabac_complete)="Complete?"
#Setting Units


#Setting Factors(will create new variable for factors)
data$niv_etu.factor = factor(data$niv_etu,levels=c("1","2","3","4","5","6","7","8","9","BEI","10","11"))
data$csp.factor = factor(data$csp,levels=c("1","2","3","4","5","6","7","8","9"))
data$stat_mar.factor = factor(data$stat_mar,levels=c("1","2","3","4","5"))
data$tobac_mention.factor = factor(data$tobac_mention,levels=c("1","0"))
data$tabac_life.factor = factor(data$tabac_life,levels=c("1","0"))
data$smoking.factor = factor(data$smoking,levels=c("1","0"))
data$tobac_diag.factor = factor(data$tobac_diag,levels=c("1","0"))
data$recent_smoker.factor = factor(data$recent_smoker,levels=c("1","0"))
data$drogue.factor = factor(data$drogue,levels=c("0","1","2"))
data$sport.factor = factor(data$sport,levels=c("1","0"))
data$comm_sport___1.factor = factor(data$comm_sport___1,levels=c("0","1"))
data$comm_sport___2.factor = factor(data$comm_sport___2,levels=c("0","1"))
data$in_situ_exclus.factor = factor(data$in_situ_exclus,levels=c("1","0"))
data$type_histo___0.factor = factor(data$type_histo___0,levels=c("0","1"))
data$type_histo___1.factor = factor(data$type_histo___1,levels=c("0","1"))
data$type_histo___2.factor = factor(data$type_histo___2,levels=c("0","1"))
data$type_histo___3.factor = factor(data$type_histo___3,levels=c("0","1"))
data$type_histo___4.factor = factor(data$type_histo___4,levels=c("0","1"))
data$bifocal.factor = factor(data$bifocal,levels=c("1","0"))
data$rec_est___0.factor = factor(data$rec_est___0,levels=c("0","1"))
data$rec_est___1.factor = factor(data$rec_est___1,levels=c("0","1"))
data$rec_est___2.factor = factor(data$rec_est___2,levels=c("0","1"))
data$recep_prog___0.factor = factor(data$recep_prog___0,levels=c("0","1"))
data$recep_prog___1.factor = factor(data$recep_prog___1,levels=c("0","1"))
data$recep_prog___2.factor = factor(data$recep_prog___2,levels=c("0","1"))
data$stat_her2___0.factor = factor(data$stat_her2___0,levels=c("0","1"))
data$stat_her2___1.factor = factor(data$stat_her2___1,levels=c("0","1"))
data$stat_her2___2.factor = factor(data$stat_her2___2,levels=c("0","1"))
data$grade___0.factor = factor(data$grade___0,levels=c("0","1"))
data$grade___1.factor = factor(data$grade___1,levels=c("0","1"))
data$grade___2.factor = factor(data$grade___2,levels=c("0","1"))
data$grade___3.factor = factor(data$grade___3,levels=c("0","1"))
data$embole___0.factor = factor(data$embole___0,levels=c("0","1"))
data$embole___1.factor = factor(data$embole___1,levels=c("0","1"))
data$embole___2.factor = factor(data$embole___2,levels=c("0","1"))
data$ttt_kr___0.factor = factor(data$ttt_kr___0,levels=c("0","1"))
data$ttt_kr___1.factor = factor(data$ttt_kr___1,levels=c("0","1"))
data$ttt_kr___2.factor = factor(data$ttt_kr___2,levels=c("0","1"))
data$ttt_kr___3.factor = factor(data$ttt_kr___3,levels=c("0","1"))
data$ttt_kr___4.factor = factor(data$ttt_kr___4,levels=c("0","1"))
data$ttt_kr___5.factor = factor(data$ttt_kr___5,levels=c("0","1"))
data$hormono_neo.factor = factor(data$hormono_neo,levels=c("1","0"))
data$seq_chimio.factor = factor(data$seq_chimio,levels=c("0","1","2"))
data$reponse_cna.factor = factor(data$reponse_cna,levels=c("0","1","2"))
data$chir.factor = factor(data$chir,levels=c("0","1","2"))
data$reconst.factor = factor(data$reconst,levels=c("1","0"))
data$anes_tabac.factor = factor(data$anes_tabac,levels=c("1","0"))
data$info_tabac_anes___0.factor = factor(data$info_tabac_anes___0,levels=c("0","1"))
data$info_tabac_anes___1.factor = factor(data$info_tabac_anes___1,levels=c("0","1"))
data$info_tabac_anes___2.factor = factor(data$info_tabac_anes___2,levels=c("0","1"))
data$chir_tabac.factor = factor(data$chir_tabac,levels=c("1","0"))
data$info_tabac_chir___0.factor = factor(data$info_tabac_chir___0,levels=c("0","1"))
data$info_tabac_chir___1.factor = factor(data$info_tabac_chir___1,levels=c("0","1"))
data$info_tabac_chir___2.factor = factor(data$info_tabac_chir___2,levels=c("0","1"))
data$rt_tabac.factor = factor(data$rt_tabac,levels=c("1","0"))
data$info_tabac_rt___0.factor = factor(data$info_tabac_rt___0,levels=c("0","1"))
data$info_tabac_rt___1.factor = factor(data$info_tabac_rt___1,levels=c("0","1"))
data$info_tabac_rt___2.factor = factor(data$info_tabac_rt___2,levels=c("0","1"))
data$onco_tabac.factor = factor(data$onco_tabac,levels=c("1","0"))
data$info_tabac_onco___0.factor = factor(data$info_tabac_onco___0,levels=c("0","1"))
data$info_tabac_onco___1.factor = factor(data$info_tabac_onco___1,levels=c("0","1"))
data$info_tabac_onco___2.factor = factor(data$info_tabac_onco___2,levels=c("0","1"))
data$moyen_sevrage___0.factor = factor(data$moyen_sevrage___0,levels=c("0","1"))
data$moyen_sevrage___1.factor = factor(data$moyen_sevrage___1,levels=c("0","1"))
data$moyen_sevrage___2.factor = factor(data$moyen_sevrage___2,levels=c("0","1"))
data$moyen_sevrage___3.factor = factor(data$moyen_sevrage___3,levels=c("0","1"))
data$moyen_sevrage___4.factor = factor(data$moyen_sevrage___4,levels=c("0","1"))
data$moyen_sevrage___5.factor = factor(data$moyen_sevrage___5,levels=c("0","1"))
data$moyen_sevrage___6.factor = factor(data$moyen_sevrage___6,levels=c("0","1"))
data$tabac_surv.factor = factor(data$tabac_surv,levels=c("0","1","2","3"))
data$methode_sevrage___0.factor = factor(data$methode_sevrage___0,levels=c("0","1"))
data$methode_sevrage___1.factor = factor(data$methode_sevrage___1,levels=c("0","1"))
data$methode_sevrage___2.factor = factor(data$methode_sevrage___2,levels=c("0","1"))
data$methode_sevrage___3.factor = factor(data$methode_sevrage___3,levels=c("0","1"))
data$methode_sevrage___4.factor = factor(data$methode_sevrage___4,levels=c("0","1"))
data$methode_sevrage___5.factor = factor(data$methode_sevrage___5,levels=c("0","1"))
data$methode_sevrage___6.factor = factor(data$methode_sevrage___6,levels=c("0","1"))
data$motivation_sevrage___0.factor = factor(data$motivation_sevrage___0,levels=c("0","1"))
data$motivation_sevrage___1.factor = factor(data$motivation_sevrage___1,levels=c("0","1"))
data$motivation_sevrage___2.factor = factor(data$motivation_sevrage___2,levels=c("0","1"))
data$motivation_sevrage___3.factor = factor(data$motivation_sevrage___3,levels=c("0","1"))
data$recidiv_cancer.factor = factor(data$recidiv_cancer,levels=c("1","0"))
data$questionnaire_tabac_complete.factor = factor(data$questionnaire_tabac_complete,levels=c("0","1","2"))

levels(data$niv_etu.factor)=c("never been enrolled","elementary school","no degree but enrolled until after elementary school","CEP (certificat détudes primaires)","BEPC, brevet élémentaire, brevet des collèges","CAP, brevet de compagnon","BEP","Baccalauréat général, brevet supérieur","Baccalauréat technologique ou professionnel, brevet professionnel ou de technicien, BEA, BEC,","BEH, capacité en droit","Diplôme de 1er cycle universitaire, BTS, DUT, diplôme des professions sociales ou de la santé, dinfirmier(ère)","Diplôme de 2ème ou 3ème cycle universitaire (y compris médecine, pharmacie, dentaire), diplôme dingénieur, dune grande école, doctorat, etc.")
levels(data$csp.factor)=c("Agriculture, forestry, fishing","Energy Industry","Others industries","Construction","Trade","Education, health, social work","Other service","None","Retired")
levels(data$stat_mar.factor)=c("single","couple","married","divorced","widow")
levels(data$tobac_mention.factor)=c("Yes","No")
levels(data$tabac_life.factor)=c("Yes","No")
levels(data$smoking.factor)=c("Yes","No")
levels(data$tobac_diag.factor)=c("Yes","No")
levels(data$recent_smoker.factor)=c("Yes","No")
levels(data$drogue.factor)=c("currently","ever","never")
levels(data$sport.factor)=c("Yes","No")
levels(data$comm_sport___1.factor)=c("Unchecked","Checked")
levels(data$comm_sport___2.factor)=c("Unchecked","Checked")
levels(data$in_situ_exclus.factor)=c("Yes","No")
levels(data$type_histo___0.factor)=c("Unchecked","Checked")
levels(data$type_histo___1.factor)=c("Unchecked","Checked")
levels(data$type_histo___2.factor)=c("Unchecked","Checked")
levels(data$type_histo___3.factor)=c("Unchecked","Checked")
levels(data$type_histo___4.factor)=c("Unchecked","Checked")
levels(data$bifocal.factor)=c("Yes","No")
levels(data$rec_est___0.factor)=c("Unchecked","Checked")
levels(data$rec_est___1.factor)=c("Unchecked","Checked")
levels(data$rec_est___2.factor)=c("Unchecked","Checked")
levels(data$recep_prog___0.factor)=c("Unchecked","Checked")
levels(data$recep_prog___1.factor)=c("Unchecked","Checked")
levels(data$recep_prog___2.factor)=c("Unchecked","Checked")
levels(data$stat_her2___0.factor)=c("Unchecked","Checked")
levels(data$stat_her2___1.factor)=c("Unchecked","Checked")
levels(data$stat_her2___2.factor)=c("Unchecked","Checked")
levels(data$grade___0.factor)=c("Unchecked","Checked")
levels(data$grade___1.factor)=c("Unchecked","Checked")
levels(data$grade___2.factor)=c("Unchecked","Checked")
levels(data$grade___3.factor)=c("Unchecked","Checked")
levels(data$embole___0.factor)=c("Unchecked","Checked")
levels(data$embole___1.factor)=c("Unchecked","Checked")
levels(data$embole___2.factor)=c("Unchecked","Checked")
levels(data$ttt_kr___0.factor)=c("Unchecked","Checked")
levels(data$ttt_kr___1.factor)=c("Unchecked","Checked")
levels(data$ttt_kr___2.factor)=c("Unchecked","Checked")
levels(data$ttt_kr___3.factor)=c("Unchecked","Checked")
levels(data$ttt_kr___4.factor)=c("Unchecked","Checked")
levels(data$ttt_kr___5.factor)=c("Unchecked","Checked")
levels(data$hormono_neo.factor)=c("Yes","No")
levels(data$seq_chimio.factor)=c("néoadjuvant","adjuvant","néoadjuvant+adjuvant")
levels(data$reponse_cna.factor)=c("Réponse Complete histologique avec In situ","Réponse Complete sans In situ","Pas de réponse complète")
levels(data$chir.factor)=c("tumorectomie","mastectomie","MRMI")
levels(data$reconst.factor)=c("Yes","No")
levels(data$anes_tabac.factor)=c("Yes","No")
levels(data$info_tabac_anes___0.factor)=c("Unchecked","Checked")
levels(data$info_tabac_anes___1.factor)=c("Unchecked","Checked")
levels(data$info_tabac_anes___2.factor)=c("Unchecked","Checked")
levels(data$chir_tabac.factor)=c("Yes","No")
levels(data$info_tabac_chir___0.factor)=c("Unchecked","Checked")
levels(data$info_tabac_chir___1.factor)=c("Unchecked","Checked")
levels(data$info_tabac_chir___2.factor)=c("Unchecked","Checked")
levels(data$rt_tabac.factor)=c("Yes","No")
levels(data$info_tabac_rt___0.factor)=c("Unchecked","Checked")
levels(data$info_tabac_rt___1.factor)=c("Unchecked","Checked")
levels(data$info_tabac_rt___2.factor)=c("Unchecked","Checked")
levels(data$onco_tabac.factor)=c("Yes","No")
levels(data$info_tabac_onco___0.factor)=c("Unchecked","Checked")
levels(data$info_tabac_onco___1.factor)=c("Unchecked","Checked")
levels(data$info_tabac_onco___2.factor)=c("Unchecked","Checked")
levels(data$moyen_sevrage___0.factor)=c("Unchecked","Checked")
levels(data$moyen_sevrage___1.factor)=c("Unchecked","Checked")
levels(data$moyen_sevrage___2.factor)=c("Unchecked","Checked")
levels(data$moyen_sevrage___3.factor)=c("Unchecked","Checked")
levels(data$moyen_sevrage___4.factor)=c("Unchecked","Checked")
levels(data$moyen_sevrage___5.factor)=c("Unchecked","Checked")
levels(data$moyen_sevrage___6.factor)=c("Unchecked","Checked")
levels(data$tabac_surv.factor)=c("current","cessation before diagnosis","cessation at diagnosis","never")
levels(data$methode_sevrage___0.factor)=c("Unchecked","Checked")
levels(data$methode_sevrage___1.factor)=c("Unchecked","Checked")
levels(data$methode_sevrage___2.factor)=c("Unchecked","Checked")
levels(data$methode_sevrage___3.factor)=c("Unchecked","Checked")
levels(data$methode_sevrage___4.factor)=c("Unchecked","Checked")
levels(data$methode_sevrage___5.factor)=c("Unchecked","Checked")
levels(data$methode_sevrage___6.factor)=c("Unchecked","Checked")
levels(data$motivation_sevrage___0.factor)=c("Unchecked","Checked")
levels(data$motivation_sevrage___1.factor)=c("Unchecked","Checked")
levels(data$motivation_sevrage___2.factor)=c("Unchecked","Checked")
levels(data$motivation_sevrage___3.factor)=c("Unchecked","Checked")
levels(data$recidiv_cancer.factor)=c("Yes","No")
levels(data$questionnaire_tabac_complete.factor)=c("Incomplete","Unverified","Complete")

write_csv(data,"/Users/bea/GitHUB/databases/core/17_sein_tabac/data/Tabac_DATA_2021-01-10_2228_2.csv")
