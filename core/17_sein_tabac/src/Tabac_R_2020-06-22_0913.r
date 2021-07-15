#Clear existing data and graphics
rm(list=ls())
graphics.off()
#Load Hmisc library
library(Hmisc)
#Read Data
data=read.csv('/Users/nadir/Desktop/projects/postdoc/SEIN-TABAC/Tabac_DATA_2020-06-22_0913.csv', sep=';')
#Setting Labels

label(data$record_id)="IPP"
label(data$date_consult)="Date du jour de la consultation "
label(data$ddn)="Date de naissance "
label(data$habitus)="Habitus"
label(data$niv_etu)="Niveau détude (le plus haut)"
label(data$csp)="Profession au diagnostic "
label(data$age_a_la_chir)="Age "
label(data$poids)="Poids "
label(data$taille)="Taille "
label(data$stat_mar)="Statut marital"
label(data$tabac_doss)="Tabac mentionné dans le dossier"
label(data$tabac_life)="Tabac (au moins 100 cig dans sa vie) "
label(data$tabac_diag)="Tabac (au diagnostic) "
label(data$age_debut)="Age de debut "
label(data$anne_tabac)="Nb total années tabac "
label(data$nb_cig_life)="Nb moyen cig /j "
label(data$age_stop)="Age a  arrêt "
label(data$recent_smoker)="Arrêt dans année avant diagnostic "
label(data$alcool)="Alcool (nb verres / semaine) "
label(data$drogue)="Drogues (cannabis autres etc..) "
label(data$comm_drogue)="Commentaires drogues"
label(data$sport)="Activité physique "
label(data$comm_sport___1)="Durée activité physique (choice=< 30 min/jour)"
label(data$comm_sport___2)="Durée activité physique (choice=> 30 min/jour)"
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
label(data$anes_tabac)="Anest aborde question tabac "
label(data$info_tabac_anes___0)="Info tabac anest  (choice=info complications)"
label(data$info_tabac_anes___1)="Info tabac anest  (choice=info necessite arret)"
label(data$info_tabac_anes___2)="Info tabac anest  (choice=conseils et orientation)"
label(data$chir_tabac)="Chir aborde question tabac "
label(data$info_tabac_chir___0)="info tabac chir  (choice=info complications)"
label(data$info_tabac_chir___1)="info tabac chir  (choice=info necessite arret)"
label(data$info_tabac_chir___2)="info tabac chir  (choice=conseils et orientation)"
label(data$rt_tabac)="RT aborde question tabac "
label(data$info_tabac_rt___0)="Info tabac RT  (choice=info complications)"
label(data$info_tabac_rt___1)="Info tabac RT  (choice=info necessite arret)"
label(data$info_tabac_rt___2)="Info tabac RT  (choice=conseils et orientation)"
label(data$onco_tabac)="Oncologue aborde question tabac "
label(data$info_tabac_onco___0)="Info tabac onco  (choice=info complications)"
label(data$info_tabac_onco___1)="Info tabac onco  (choice=info necessite arret)"
label(data$info_tabac_onco___2)="Info tabac onco  (choice=conseils et orientation)"
label(data$moyen_sevrage___0)="Moyens sevrage tabac (choice=Cs tabacologie)"
label(data$moyen_sevrage___1)="Moyens sevrage tabac (choice=soutien psy)"
label(data$moyen_sevrage___2)="Moyens sevrage tabac (choice=TCC)"
label(data$moyen_sevrage___3)="Moyens sevrage tabac (choice=substituts nicotiniques)"
label(data$moyen_sevrage___4)="Moyens sevrage tabac (choice=pharmaco)"
label(data$moyen_sevrage___5)="Moyens sevrage tabac (choice=hypnose)"
label(data$moyen_sevrage___6)="Moyens sevrage tabac (choice=acupuncture)"
label(data$tabac_surv)="Statut tabac actuel "
label(data$duree_sevrage)="Année sevrage depuis diagnostic (mois) "
label(data$nb_cig_surv)="Nb moyen cig /j "
label(data$methode_sevrage___0)="Methode sevrage  (choice=Cs tabacologie)"
label(data$methode_sevrage___1)="Methode sevrage  (choice=soutien psy)"
label(data$methode_sevrage___2)="Methode sevrage  (choice=TCC)"
label(data$methode_sevrage___3)="Methode sevrage  (choice=substituts nicotiniques)"
label(data$methode_sevrage___4)="Methode sevrage  (choice=pharmaco)"
label(data$methode_sevrage___5)="Methode sevrage  (choice=hypnose)"
label(data$methode_sevrage___6)="Methode sevrage  (choice=acupuncture)"
label(data$motivation_sevrage___0)="Motivation sevrage  (choice=peur recidive ou autre cancer)"
label(data$motivation_sevrage___1)="Motivation sevrage  (choice=peur complications)"
label(data$motivation_sevrage___2)="Motivation sevrage  (choice=desir de reconstruction mammaire)"
label(data$motivation_sevrage___3)="Motivation sevrage  (choice=autre)"
label(data$autre_motivation_sevrage)="Autre motivation sevrage "
label(data$recidiv_cancer)="Récidive après traitement"
label(data$poids_actuel)="Poids actuel"
label(data$comment)="Commentaire"
label(data$questionnaire_tabac_complete)="Complete?"
#Setting Units


#Setting Factors(will create new variable for factors)
data$niv_etu.factor = factor(data$niv_etu,levels=c("1","2","3","4","5","6","7","8","9","BEI","10","11"))
data$csp.factor = factor(data$csp,levels=c("1","2","3","4","5","6","7","8","9"))
data$stat_mar.factor = factor(data$stat_mar,levels=c("1","2","3","4","5"))
data$tabac_doss.factor = factor(data$tabac_doss,levels=c("1","0"))
data$tabac_life.factor = factor(data$tabac_life,levels=c("1","0"))
data$tabac_diag.factor = factor(data$tabac_diag,levels=c("1","0"))
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

levels(data$niv_etu.factor)=c("Vous navez pas été scolarisé(e)","Aucun diplôme mais scolarité jusquen école  primaire ou au collège","Aucun diplôme mais scolarité au-delà du collège","CEP (certificat détudes primaires)","BEPC, brevet élémentaire, brevet des collèges","CAP, brevet de compagnon","BEP","Baccalauréat général, brevet supérieur","Baccalauréat technologique ou professionnel, brevet professionnel ou de technicien, BEA, BEC,","BEH, capacité en droit","Diplôme de 1er cycle universitaire, BTS, DUT, diplôme des professions sociales ou de la santé, dinfirmier(ère)","Diplôme de 2ème ou 3ème cycle universitaire (y compris médecine, pharmacie, dentaire), diplôme dingénieur, dune grande école, doctorat, etc.")
levels(data$csp.factor)=c("Agriculture, sylviculture, pêche","Industries énergétiques","Autres industries","Construction","Commerce et réparations","Education, santé, action sociale","Autre service ou autre activité du tertiaire","aucune","retraitée")
levels(data$stat_mar.factor)=c("célibataire","concubinage","mariée","divorcée","veuve")
levels(data$tabac_doss.factor)=c("Yes","No")
levels(data$tabac_life.factor)=c("Yes","No")
levels(data$tabac_diag.factor)=c("Yes","No")
levels(data$recent_smoker.factor)=c("Yes","No")
levels(data$drogue.factor)=c("en cours","sevre","jamais")
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
levels(data$tabac_surv.factor)=c("en cours","sevre avant diagnostic","sevre depuis diagnostic","jamais")
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

