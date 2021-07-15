preprocessBasetabac <- function(self){
  library(Hmisc)
  label(self@database$questionnaire_tabac_complete)="Complete?"
  label(self@database$niv_etu)="Niveau détude (le plus haut)"
  label(self@database$csp)="Profession au diagnostic "
  label(self@database$poids)="Poids "
  label(self@database$taille)="Taille "
  label(self@database$stat_mar)="Statut marital"
  label(self@database$tabac_doss)="Tabac mentionné dans le dossier"
  label(self@database$tabac_life)="Tabac (au moins 100 cig dans sa vie) "
  label(self@database$tabac_diag)="Tabac (au diagnostic) "
  label(self@database$age_debut)="Age de debut "
  label(self@database$anne_tabac)="Nb total années tabac "
  label(self@database$nb_cig_life)="Nb moyen cig /j "
  label(self@database$age_stop)="Age a  arrêt "
  label(self@database$recent_smoker)="Arrêt dans année avant diagnostic "
  label(self@database$alcool)="Alcool (nb verres / semaine) "
  label(self@database$drogue)="Drogues (cannabis autres etc..) "
  label(self@database$sport)="Activité physique "
  label(self@database$comm_sport___1)="Durée activité physique (choice=< 30 min/jour)"
  label(self@database$comm_sport___2)="Durée activité physique (choice=> 30 min/jour)"
  label(self@database$in_situ_exclus)="Carcinome In situ exclusif"
  label(self@database$type_histo___0)="Type histologique (choice=CCI)"
  label(self@database$type_histo___1)="Type histologique (choice=CLI)"
  label(self@database$type_histo___2)="Type histologique (choice=CCIS)"
  label(self@database$type_histo___3)="Type histologique (choice=CLIS)"
  label(self@database$type_histo___4)="Type histologique (choice=Others)"
  label(self@database$bifocal)="Multifocal"
  label(self@database$rec_est___0)="Récepteur estrogène (choice=positif)"
  label(self@database$rec_est___1)="Récepteur estrogène (choice=négatif)"
  label(self@database$rec_est___2)="Récepteur estrogène (choice=na)"
  label(self@database$recep_prog___0)="Récepteur progesterone (choice=positif)"
  label(self@database$recep_prog___1)="Récepteur progesterone (choice=négatif)"
  label(self@database$recep_prog___2)="Récepteur progesterone (choice=na)"
  label(self@database$stat_her2___0)="Statut Her2 (choice=positif)"
  label(self@database$stat_her2___1)="Statut Her2 (choice=négatif)"
  label(self@database$stat_her2___2)="Statut Her2 (choice=na)"
  label(self@database$grade___0)="Grade histologique (choice=1)"
  label(self@database$grade___1)="Grade histologique (choice=2)"
  label(self@database$grade___2)="Grade histologique (choice=3)"
  label(self@database$grade___3)="Grade histologique (choice=na)"
  label(self@database$embole___0)="Emboles (choice=oui)"
  label(self@database$embole___1)="Emboles (choice=non)"
  label(self@database$embole___2)="Emboles (choice=na)"
  label(self@database$nb_gg)="Nombre ganglion positif"
  label(self@database$ttt_kr___0)="Traitements recus (QCM)  (choice=chirurgie)"
  label(self@database$ttt_kr___1)="Traitements recus (QCM)  (choice=chimiotherapie)"
  label(self@database$ttt_kr___2)="Traitements recus (QCM)  (choice=radiotherapie)"
  label(self@database$ttt_kr___3)="Traitements recus (QCM)  (choice=trastuzumab)"
  label(self@database$ttt_kr___4)="Traitements recus (QCM)  (choice=hormonotherapie)"
  label(self@database$ttt_kr___5)="Traitements recus (QCM)  (choice=reconstruction mammaire)"
  label(self@database$hormono_neo)="Hormonothérapie néo-adjuvant"
  label(self@database$seq_chimio)="Séquence chimiothérapie"
  label(self@database$taille_infiltrant)="Taille de la tumeur (mm)"
  label(self@database$reponse_cna)="Réponse a la chimiothérapie neoadjuvant"
  label(self@database$chir)="Chirurgie sein "
  label(self@database$reconst)="Reconstruction mammaire differee "
  label(self@database$anes_tabac)="Anest aborde question tabac "
  label(self@database$info_tabac_anes___0)="Info tabac anest  (choice=info complications)"
  label(self@database$info_tabac_anes___1)="Info tabac anest  (choice=info necessite arret)"
  label(self@database$info_tabac_anes___2)="Info tabac anest  (choice=conseils et orientation)"
  label(self@database$chir_tabac)="Chir aborde question tabac "
  label(self@database$info_tabac_chir___0)="info tabac chir  (choice=info complications)"
  label(self@database$info_tabac_chir___1)="info tabac chir  (choice=info necessite arret)"
  label(self@database$info_tabac_chir___2)="info tabac chir  (choice=conseils et orientation)"
  label(self@database$rt_tabac)="RT aborde question tabac "
  label(self@database$info_tabac_rt___0)="Info tabac RT  (choice=info complications)"
  label(self@database$info_tabac_rt___1)="Info tabac RT  (choice=info necessite arret)"
  label(self@database$info_tabac_rt___2)="Info tabac RT  (choice=conseils et orientation)"
  label(self@database$onco_tabac)="Oncologue aborde question tabac "
  label(self@database$info_tabac_onco___0)="Info tabac onco  (choice=info complications)"
  label(self@database$info_tabac_onco___1)="Info tabac onco  (choice=info necessite arret)"
  label(self@database$info_tabac_onco___2)="Info tabac onco  (choice=conseils et orientation)"
  label(self@database$moyen_sevrage___0)="Moyens sevrage tabac (choice=Cs tabacologie)"
  label(self@database$moyen_sevrage___1)="Moyens sevrage tabac (choice=soutien psy)"
  label(self@database$moyen_sevrage___2)="Moyens sevrage tabac (choice=TCC)"
  label(self@database$moyen_sevrage___3)="Moyens sevrage tabac (choice=substituts nicotiniques)"
  label(self@database$moyen_sevrage___4)="Moyens sevrage tabac (choice=pharmaco)"
  label(self@database$moyen_sevrage___5)="Moyens sevrage tabac (choice=hypnose)"
  label(self@database$moyen_sevrage___6)="Moyens sevrage tabac (choice=acupuncture)"
  label(self@database$tabac_surv)="Statut tabac actuel "
  label(self@database$duree_sevrage)="Année sevrage depuis diagnostic (mois) "
  label(self@database$nb_cig_surv)="Nb moyen cig /j "
  label(self@database$methode_sevrage___0)="Methode sevrage  (choice=Cs tabacologie)"
  label(self@database$methode_sevrage___1)="Methode sevrage  (choice=soutien psy)"
  label(self@database$methode_sevrage___2)="Methode sevrage  (choice=TCC)"
  label(self@database$methode_sevrage___3)="Methode sevrage  (choice=substituts nicotiniques)"
  label(self@database$methode_sevrage___4)="Methode sevrage  (choice=pharmaco)"
  label(self@database$methode_sevrage___5)="Methode sevrage  (choice=hypnose)"
  label(self@database$methode_sevrage___6)="Methode sevrage  (choice=acupuncture)"
  label(self@database$motivation_sevrage___0)="Motivation sevrage  (choice=peur recidive ou autre cancer)"
  label(self@database$motivation_sevrage___1)="Motivation sevrage  (choice=peur complications)"
  label(self@database$motivation_sevrage___2)="Motivation sevrage  (choice=desir de reconstruction mammaire)"
  label(self@database$motivation_sevrage___3)="Motivation sevrage  (choice=autre)"
  label(self@database$autre_motivation_sevrage)="Autre motivation sevrage "
  label(self@database$recidiv_cancer)="Récidive après traitement"
  label(self@database$age_a_la_chir)="Age "
  label(self@database$poids_actuel)="Poids actuel"
  label(self@database$date_hormono)="date début hormono néo adj"
  label(self@database$date_chimio)="Date début chimio néo adj"
  #Setting Units
  
  
  #Setting Factors(will create new variable for factors)
  self@database$questionnaire_tabac_complete.factor = factor(self@database$questionnaire_tabac_complete,levels=c("0","1","2"))
  self@database$niv_etu.factor = factor(self@database$niv_etu,levels=c("1","2","3","4","5","6","7","8","9","BEI","10","11"))
  self@database$csp.factor = factor(self@database$csp,levels=c("1","2","3","4","5","6","7","8","9"))
  self@database$stat_mar.factor = factor(self@database$stat_mar,levels=c("1","2","3","4","5"))
  self@database$tabac_doss.factor = factor(self@database$tabac_doss,levels=c("1","0"))
  self@database$tabac_life.factor = factor(self@database$tabac_life,levels=c("1","0"))
  self@database$tabac_diag.factor = factor(self@database$tabac_diag,levels=c("1","0"))
  self@database$recent_smoker.factor = factor(self@database$recent_smoker,levels=c("1","0"))
  self@database$drogue.factor = factor(self@database$drogue,levels=c("0","1","2"))
  self@database$sport.factor = factor(self@database$sport,levels=c("1","0"))
  self@database$comm_sport___1.factor = factor(self@database$comm_sport___1,levels=c("0","1"))
  self@database$comm_sport___2.factor = factor(self@database$comm_sport___2,levels=c("0","1"))
  self@database$in_situ_exclus.factor = factor(self@database$in_situ_exclus,levels=c("1","0"))
  self@database$type_histo___0.factor = factor(self@database$type_histo___0,levels=c("0","1"))
  self@database$type_histo___1.factor = factor(self@database$type_histo___1,levels=c("0","1"))
  self@database$type_histo___2.factor = factor(self@database$type_histo___2,levels=c("0","1"))
  self@database$type_histo___3.factor = factor(self@database$type_histo___3,levels=c("0","1"))
  self@database$type_histo___4.factor = factor(self@database$type_histo___4,levels=c("0","1"))
  self@database$bifocal.factor = factor(self@database$bifocal,levels=c("1","0"))
  self@database$rec_est___0.factor = factor(self@database$rec_est___0,levels=c("0","1"))
  self@database$rec_est___1.factor = factor(self@database$rec_est___1,levels=c("0","1"))
  self@database$rec_est___2.factor = factor(self@database$rec_est___2,levels=c("0","1"))
  self@database$recep_prog___0.factor = factor(self@database$recep_prog___0,levels=c("0","1"))
  self@database$recep_prog___1.factor = factor(self@database$recep_prog___1,levels=c("0","1"))
  self@database$recep_prog___2.factor = factor(self@database$recep_prog___2,levels=c("0","1"))
  self@database$stat_her2___0.factor = factor(self@database$stat_her2___0,levels=c("0","1"))
  self@database$stat_her2___1.factor = factor(self@database$stat_her2___1,levels=c("0","1"))
  self@database$stat_her2___2.factor = factor(self@database$stat_her2___2,levels=c("0","1"))
  self@database$grade___0.factor = factor(self@database$grade___0,levels=c("0","1"))
  self@database$grade___1.factor = factor(self@database$grade___1,levels=c("0","1"))
  self@database$grade___2.factor = factor(self@database$grade___2,levels=c("0","1"))
  self@database$grade___3.factor = factor(self@database$grade___3,levels=c("0","1"))
  self@database$embole___0.factor = factor(self@database$embole___0,levels=c("0","1"))
  self@database$embole___1.factor = factor(self@database$embole___1,levels=c("0","1"))
  self@database$embole___2.factor = factor(self@database$embole___2,levels=c("0","1"))
  self@database$ttt_kr___0.factor = factor(self@database$ttt_kr___0,levels=c("0","1"))
  self@database$ttt_kr___1.factor = factor(self@database$ttt_kr___1,levels=c("0","1"))
  self@database$ttt_kr___2.factor = factor(self@database$ttt_kr___2,levels=c("0","1"))
  self@database$ttt_kr___3.factor = factor(self@database$ttt_kr___3,levels=c("0","1"))
  self@database$ttt_kr___4.factor = factor(self@database$ttt_kr___4,levels=c("0","1"))
  self@database$ttt_kr___5.factor = factor(self@database$ttt_kr___5,levels=c("0","1"))
  self@database$hormono_neo.factor = factor(self@database$hormono_neo,levels=c("1","0"))
  self@database$seq_chimio.factor = factor(self@database$seq_chimio,levels=c("0","1","2"))
  self@database$reponse_cna.factor = factor(self@database$reponse_cna,levels=c("0","1","2"))
  self@database$chir.factor = factor(self@database$chir,levels=c("0","1","2"))
  self@database$reconst.factor = factor(self@database$reconst,levels=c("1","0"))
  self@database$anes_tabac.factor = factor(self@database$anes_tabac,levels=c("1","0"))
  self@database$info_tabac_anes___0.factor = factor(self@database$info_tabac_anes___0,levels=c("0","1"))
  self@database$info_tabac_anes___1.factor = factor(self@database$info_tabac_anes___1,levels=c("0","1"))
  self@database$info_tabac_anes___2.factor = factor(self@database$info_tabac_anes___2,levels=c("0","1"))
  self@database$chir_tabac.factor = factor(self@database$chir_tabac,levels=c("1","0"))
  self@database$info_tabac_chir___0.factor = factor(self@database$info_tabac_chir___0,levels=c("0","1"))
  self@database$info_tabac_chir___1.factor = factor(self@database$info_tabac_chir___1,levels=c("0","1"))
  self@database$info_tabac_chir___2.factor = factor(self@database$info_tabac_chir___2,levels=c("0","1"))
  self@database$rt_tabac.factor = factor(self@database$rt_tabac,levels=c("1","0"))
  self@database$info_tabac_rt___0.factor = factor(self@database$info_tabac_rt___0,levels=c("0","1"))
  self@database$info_tabac_rt___1.factor = factor(self@database$info_tabac_rt___1,levels=c("0","1"))
  self@database$info_tabac_rt___2.factor = factor(self@database$info_tabac_rt___2,levels=c("0","1"))
  self@database$onco_tabac.factor = factor(self@database$onco_tabac,levels=c("1","0"))
  self@database$info_tabac_onco___0.factor = factor(self@database$info_tabac_onco___0,levels=c("0","1"))
  self@database$info_tabac_onco___1.factor = factor(self@database$info_tabac_onco___1,levels=c("0","1"))
  self@database$info_tabac_onco___2.factor = factor(self@database$info_tabac_onco___2,levels=c("0","1"))
  self@database$moyen_sevrage___0.factor = factor(self@database$moyen_sevrage___0,levels=c("0","1"))
  self@database$moyen_sevrage___1.factor = factor(self@database$moyen_sevrage___1,levels=c("0","1"))
  self@database$moyen_sevrage___2.factor = factor(self@database$moyen_sevrage___2,levels=c("0","1"))
  self@database$moyen_sevrage___3.factor = factor(self@database$moyen_sevrage___3,levels=c("0","1"))
  self@database$moyen_sevrage___4.factor = factor(self@database$moyen_sevrage___4,levels=c("0","1"))
  self@database$moyen_sevrage___5.factor = factor(self@database$moyen_sevrage___5,levels=c("0","1"))
  self@database$moyen_sevrage___6.factor = factor(self@database$moyen_sevrage___6,levels=c("0","1"))
  self@database$tabac_surv.factor = factor(self@database$tabac_surv,levels=c("0","1","2","3"))
  self@database$methode_sevrage___0.factor = factor(self@database$methode_sevrage___0,levels=c("0","1"))
  self@database$methode_sevrage___1.factor = factor(self@database$methode_sevrage___1,levels=c("0","1"))
  self@database$methode_sevrage___2.factor = factor(self@database$methode_sevrage___2,levels=c("0","1"))
  self@database$methode_sevrage___3.factor = factor(self@database$methode_sevrage___3,levels=c("0","1"))
  self@database$methode_sevrage___4.factor = factor(self@database$methode_sevrage___4,levels=c("0","1"))
  self@database$methode_sevrage___5.factor = factor(self@database$methode_sevrage___5,levels=c("0","1"))
  self@database$methode_sevrage___6.factor = factor(self@database$methode_sevrage___6,levels=c("0","1"))
  self@database$motivation_sevrage___0.factor = factor(self@database$motivation_sevrage___0,levels=c("0","1"))
  self@database$motivation_sevrage___1.factor = factor(self@database$motivation_sevrage___1,levels=c("0","1"))
  self@database$motivation_sevrage___2.factor = factor(self@database$motivation_sevrage___2,levels=c("0","1"))
  self@database$motivation_sevrage___3.factor = factor(self@database$motivation_sevrage___3,levels=c("0","1"))
  self@database$recidiv_cancer.factor = factor(self@database$recidiv_cancer,levels=c("1","0"))
  
  levels(self@database$questionnaire_tabac_complete.factor)=c("Incomplete","Unverified","Complete")
  levels(self@database$niv_etu.factor)=c("Vous navez pas été scolarisé(e)","Aucun diplôme mais scolarité jusquen école  primaire ou au collège","Aucun diplôme mais scolarité au-delà du collège","CEP (certificat détudes primaires)","BEPC, brevet élémentaire, brevet des collèges","CAP, brevet de compagnon","BEP","Baccalauréat général, brevet supérieur","Baccalauréat technologique ou professionnel, brevet professionnel ou de technicien, BEA, BEC,","BEH, capacité en droit","Diplôme de 1er cycle universitaire, BTS, DUT, diplôme des professions sociales ou de la santé, dinfirmier(ère)","Diplôme de 2ème ou 3ème cycle universitaire (y compris médecine, pharmacie, dentaire), diplôme dingénieur, dune grande école, doctorat, etc.")
  levels(self@database$csp.factor)=c("Agriculture, sylviculture, pêche","Industries énergétiques","Autres industries","Construction","Commerce et réparations","Education, santé, action sociale","Autre service ou autre activité du tertiaire","aucune","retraitée")
  levels(self@database$stat_mar.factor)=c("célibataire","concubinage","mariée","divorcée","veuve")
  levels(self@database$tabac_doss.factor)=c("Yes","No")
  levels(self@database$tabac_life.factor)=c("Yes","No")
  levels(self@database$tabac_diag.factor)=c("Yes","No")
  levels(self@database$recent_smoker.factor)=c("Yes","No")
  levels(self@database$drogue.factor)=c("en cours","sevre","jamais")
  levels(self@database$sport.factor)=c("Yes","No")
  levels(self@database$comm_sport___1.factor)=c("Unchecked","Checked")
  levels(self@database$comm_sport___2.factor)=c("Unchecked","Checked")
  levels(self@database$in_situ_exclus.factor)=c("Yes","No")
  levels(self@database$type_histo___0.factor)=c("Unchecked","Checked")
  levels(self@database$type_histo___1.factor)=c("Unchecked","Checked")
  levels(self@database$type_histo___2.factor)=c("Unchecked","Checked")
  levels(self@database$type_histo___3.factor)=c("Unchecked","Checked")
  levels(self@database$type_histo___4.factor)=c("Unchecked","Checked")
  levels(self@database$bifocal.factor)=c("Yes","No")
  levels(self@database$rec_est___0.factor)=c("Unchecked","Checked")
  levels(self@database$rec_est___1.factor)=c("Unchecked","Checked")
  levels(self@database$rec_est___2.factor)=c("Unchecked","Checked")
  levels(self@database$recep_prog___0.factor)=c("Unchecked","Checked")
  levels(self@database$recep_prog___1.factor)=c("Unchecked","Checked")
  levels(self@database$recep_prog___2.factor)=c("Unchecked","Checked")
  levels(self@database$stat_her2___0.factor)=c("Unchecked","Checked")
  levels(self@database$stat_her2___1.factor)=c("Unchecked","Checked")
  levels(self@database$stat_her2___2.factor)=c("Unchecked","Checked")
  levels(self@database$grade___0.factor)=c("Unchecked","Checked")
  levels(self@database$grade___1.factor)=c("Unchecked","Checked")
  levels(self@database$grade___2.factor)=c("Unchecked","Checked")
  levels(self@database$grade___3.factor)=c("Unchecked","Checked")
  levels(self@database$embole___0.factor)=c("Unchecked","Checked")
  levels(self@database$embole___1.factor)=c("Unchecked","Checked")
  levels(self@database$embole___2.factor)=c("Unchecked","Checked")
  levels(self@database$ttt_kr___0.factor)=c("Unchecked","Checked")
  levels(self@database$ttt_kr___1.factor)=c("Unchecked","Checked")
  levels(self@database$ttt_kr___2.factor)=c("Unchecked","Checked")
  levels(self@database$ttt_kr___3.factor)=c("Unchecked","Checked")
  levels(self@database$ttt_kr___4.factor)=c("Unchecked","Checked")
  levels(self@database$ttt_kr___5.factor)=c("Unchecked","Checked")
  levels(self@database$hormono_neo.factor)=c("Yes","No")
  levels(self@database$seq_chimio.factor)=c("néoadjuvant","adjuvant","néoadjuvant+adjuvant")
  levels(self@database$reponse_cna.factor)=c("Réponse Complete histologique avec In situ","Réponse Complete sans In situ","Pas de réponse complète")
  levels(self@database$chir.factor)=c("tumorectomie","mastectomie","MRMI")
  levels(self@database$reconst.factor)=c("Yes","No")
  levels(self@database$anes_tabac.factor)=c("Yes","No")
  levels(self@database$info_tabac_anes___0.factor)=c("Unchecked","Checked")
  levels(self@database$info_tabac_anes___1.factor)=c("Unchecked","Checked")
  levels(self@database$info_tabac_anes___2.factor)=c("Unchecked","Checked")
  levels(self@database$chir_tabac.factor)=c("Yes","No")
  levels(self@database$info_tabac_chir___0.factor)=c("Unchecked","Checked")
  levels(self@database$info_tabac_chir___1.factor)=c("Unchecked","Checked")
  levels(self@database$info_tabac_chir___2.factor)=c("Unchecked","Checked")
  levels(self@database$rt_tabac.factor)=c("Yes","No")
  levels(self@database$info_tabac_rt___0.factor)=c("Unchecked","Checked")
  levels(self@database$info_tabac_rt___1.factor)=c("Unchecked","Checked")
  levels(self@database$info_tabac_rt___2.factor)=c("Unchecked","Checked")
  levels(self@database$onco_tabac.factor)=c("Yes","No")
  levels(self@database$info_tabac_onco___0.factor)=c("Unchecked","Checked")
  levels(self@database$info_tabac_onco___1.factor)=c("Unchecked","Checked")
  levels(self@database$info_tabac_onco___2.factor)=c("Unchecked","Checked")
  levels(self@database$moyen_sevrage___0.factor)=c("Unchecked","Checked")
  levels(self@database$moyen_sevrage___1.factor)=c("Unchecked","Checked")
  levels(self@database$moyen_sevrage___2.factor)=c("Unchecked","Checked")
  levels(self@database$moyen_sevrage___3.factor)=c("Unchecked","Checked")
  levels(self@database$moyen_sevrage___4.factor)=c("Unchecked","Checked")
  levels(self@database$moyen_sevrage___5.factor)=c("Unchecked","Checked")
  levels(self@database$moyen_sevrage___6.factor)=c("Unchecked","Checked")
  levels(self@database$tabac_surv.factor)=c("en cours","sevre avant diagnostic","sevre depuis diagnostic","jamais")
  levels(self@database$methode_sevrage___0.factor)=c("Unchecked","Checked")
  levels(self@database$methode_sevrage___1.factor)=c("Unchecked","Checked")
  levels(self@database$methode_sevrage___2.factor)=c("Unchecked","Checked")
  levels(self@database$methode_sevrage___3.factor)=c("Unchecked","Checked")
  levels(self@database$methode_sevrage___4.factor)=c("Unchecked","Checked")
  levels(self@database$methode_sevrage___5.factor)=c("Unchecked","Checked")
  levels(self@database$methode_sevrage___6.factor)=c("Unchecked","Checked")
  levels(self@database$motivation_sevrage___0.factor)=c("Unchecked","Checked")
  levels(self@database$motivation_sevrage___1.factor)=c("Unchecked","Checked")
  levels(self@database$motivation_sevrage___2.factor)=c("Unchecked","Checked")
  levels(self@database$motivation_sevrage___3.factor)=c("Unchecked","Checked")
  levels(self@database$recidiv_cancer.factor)=c("Yes","No")
  return(self)
}
