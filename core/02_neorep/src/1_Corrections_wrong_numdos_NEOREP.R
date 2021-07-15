correction_wrong_numdos_neorep <- function(x){
  
    # 1. Numdos errors
    # *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
    
          #  4 number wrongs 	
                # "0805532" "0887512" "0582772" "0781902"
          #  4 number correct	
                # "0705532" "0887513" "0582779" "0781092"
    
    # 3 erreurs de dossier confirm?es par Aline
    x$numdos7[which(x$numdos7=="0887512")]			<- "0887513"
    x$numdos7[which(x$numdos7=="0582772")]			<- "0582779"
    x$numdos7[which(x$numdos7=="0805532")]			<- "0705532"
    # Une erreur relevee par AS; je corrige dans la base + demande ? Aline de verifier. "781092"  
    x$numdos7[which(x$numdos7=="0781902")]			<- "0781092"
    
    # 0707101 est en fait le dossier  0707501    
    x$numdos7[which(x$numdos7=="0707101")]			<- "0707501"
    
    return(x)
}
