# Nouvelle ligne
prepare_row_drive <- function(id_session,user,action,cible,PA,
                          timer,resultat,timer_ok,texte=""){
  tibble(id_session=id_session,user=user,action=action,cible=cible,PA=PA,
         timer=timer,resultat=resultat,timer_ok=timer_ok,texte=texte)
}

# Fonction de cr?ation BDD d?s
creation_bdd_des <- function(values){
  
  # Extraire la liste des r?sultats
  rate <- values$actions[values$actions$cible == 1,]$PA
  ok <- values$actions[values$actions$cible == 2,]$PA
  parfait <- values$actions[values$actions$cible == 3,]$PA
  
  rate <- rate[!is.na(rate)]
  ok <- ok[!is.na(ok)]
  parfait <- parfait[!is.na(parfait)]

  # Prendre le dernier r?sultat
  valeur_alea <- data.frame(resultat=NA,valeur=NA)
  valeur_alea[1,] <- c(1,rate[length(rate)])
  valeur_alea[2,] <- c(2,ok[length(ok)])
  valeur_alea[3,] <- c(3,parfait[length(parfait)])
  valeur_alea$valeur <- as.numeric(valeur_alea$valeur)
  
  return(valeur_alea)
}

# Fonction de recherche indices
recherche_indice <- function(id_drive,values,user_name,
                             enquete,PA,resultat=NA,duree=NA){
  n_new <- 0
  
  if (is.na(duree)){ 
    # Dur?e al?atoire de recherche
    alea_duree <- sample(c(2:10),1)
    if (nchar(enquete) == 1){
      # Enqu?te normale
      message_output <- paste("L'équipe de recherche est partie se renseigner. Vous devriez avoir une réponse dans ",alea_duree," minutes environ",sep="")
    }else{
      # Enqu?te sp?ciale
      showModal(modalDialog(
        h3('Enquête'),
        span(paste("L'équipe de recherche est partie se renseigner. Cette enquête sera particulièrement longue. Nous espérons avoir des réponses 1 heure avant la fin de votre soirée.",sep="")),
        footer = tagList(
          modalButton("OK")
        )
      ))
    }
  }else{
    alea_duree <- duree
    # Enqu?te admin
    message_output <- paste("Enquête lancée",sep="")
  }
  timer_ok <- Sys.time()+alea_duree*60
  
  if (is.na(resultat)){
    # Cr?ation de la BDD d?s
    valeur_alea <- creation_bdd_des(values) 
    
    # Lancement du d?
    liste_resultats <- rep(valeur_alea$resultat,valeur_alea$valeur)
    id_resultat <- sample(1:length(liste_resultats),1)
    resultat <- liste_resultats[id_resultat]
    # Echec critique
    if (resultat != 1){
      id_variation <- id_resultat + PA - 1
      resultat <- liste_resultats[min(id_resultat,length(liste_resultats))]
    }
  }
  
  # Savoir s'il y a une interception
  recup_indice <- values$actions[values$actions$cible == user_name & values$actions$action %in% c("interception","interception ok") & values$actions$timer < Sys.time(),]

  if (nrow(recup_indice) > 0){
    if (recup_indice[nrow(recup_indice),"action"] != "interception ok"){
    
      recup_indice <- recup_indice[nrow(recup_indice),]
      
      # La victime ne recoit que l'info enqu?te intercept?e
      # values$actions[nrow(values$actions)+1,] <-  list(user_name,"Enquete interceptee",enquete,PA,Sys.time(),variation,timer_ok)
      
      new_row <-  prepare_row_drive(values$id_session,user_name,"Enquete interceptee",
                    enquete,PA,Sys.time(),variation,timer_ok)
      
      sheet_append(id_drive, data = new_row)
      
      # L'interception actuelle n'est plus possible
      # values$actions[nrow(values$actions)+1,] <-  list(recup_indice$user,"interception ok",user_name,0,recup_indice$timer,variation,recup_indice$timer_ok)
      
      new_row <-  prepare_row_drive(values$id_session,recup_indice$user,"interception ok",
                    user_name,0,recup_indice$timer,resultat,recup_indice$timer_ok)
      
      sheet_append(id_drive, data = new_row)
      
      user_effectif <- recup_indice[,'user']
      PA_effectif <- 0
    }else{
      # Normal
      user_effectif <- user_name
      PA_effectif <- PA  
    }
  }else{		
    # Normal
    user_effectif <- user_name
    PA_effectif <- PA
  }
  
  texte_indice <- info_indices(values$actions) %>% 
    filter(indice == enquete & variation == resultat) %>% 
    pull(texte)

  # texte_indice <- info_indices[info_indices$indice == enquete & info_indices$variation == variation,'texte']
  
  # Chat GPT
  fl_chat_gpt <- values$actions[values$actions$action == "chat_gpt",]$resultat
  
  if (fl_chat_gpt[length(fl_chat_gpt)] == "Oui"){
    generic <- "Tu es une aide de jeu pour une soirée enquête. Il s'agit d'un scénario dans l'univers de INS/MV. Le joueur vient de demander une enquête à son équipe d'ange ou de démons, je vais te donner l'indice qu'il récupère, tu dois reformuler un peu le message, tout en gardant exactement les mêmes informations de scénario. Voici l'indice à reformuler : "
    
    set_chatlog(chatlog_id = ".__CURRENTCHAT__",initial_content = generic)
    # answer <- chat(texte_indice,model = "gpt-3.5-turbo",output = "response_object")
    answer <- chat(texte_indice,model = "gpt-4o",output = "response_object")
    texte_indice <- answer$choices$message$content
  }
  
  # Enregistrement du r?sultat
  new_row <-  prepare_row_drive(values$id_session,user_effectif,"enquete",enquete,
                PA_effectif,Sys.time(),resultat,timer_ok,texte_indice)

  sheet_append(id_drive, data = new_row)
  
  showModal(modalDialog(
    h3('Enquête'),
    span(message_output),
    footer = tagList(modalButton("OK"))
  ))
  
  return(message_output)
}

copie_indice <- function(values,user_name,cible,PA,variation=NA,duree=NA){
  
  # load('info_indices')
  
  timer_now <- Sys.time()
  
  new_row <- prepare_row_drive(values$id_session,user_name,"copie",cible,
               PA,timer_now,0,timer_now-6*60)
  
  sheet_append(id_drive, data = new_row)
  
  recup_indice <- values$actions[values$actions$user == cible & values$actions$action == "enquete" & timer_now <= values$actions$timer_ok+10*60,]
  
  if (nrow(recup_indice) > 0){
    
    recup_indice <- recup_indice[nrow(recup_indice),]
    new_row <-  prepare_row_drive(values$id_session,user_name,"enquete",
                                  recup_indice[,'cible'],0,timer_now,
                                  recup_indice[,'resultat'],timer_now,
                                  recup_indice[,'texte'])
    
    sheet_append(id_drive, data = new_row)
    
    message_output <- paste("Nous avons obtenu une copie de l'enquête suivante : ",info_indices[info_indices$indice==recup_indice[,'cible'] & info_indices$variation == 3,'titre'],sep='')
    
  }else{
    message_output <- "Nous n'avons rien pu copier comme enquête"
  }
  
  showModal(modalDialog(
    h3("Copie d'enquête"),
    span(message_output),
    footer = tagList(modalButton("OK"))
  ))
  
  return(message_output)
  
}

interception_indice <- function(values,user_name,cible,PA,variation=NA,duree=NA){
  timer_now <- Sys.time()
  
  if (sample(1:10,1) == 1){
    new_row <-  prepare_row_drive(values$id_session,user_name,"interception ratee",
                  cible,PA,timer_now,0,timer_now)
    sheet_append(id_drive, data = new_row)
    message_output <- "Malheureusement, nos équipes se sont faites directement captées par l'équipe concurrente. L'interception est un échec. Heureusement, votre identité est restée dissimulée."	
  }else{
    new_row <-  prepare_row_drive(values$id_session,user_name,"interception",
                  cible,PA,timer_now,0,timer_now+20*60)
    sheet_append(id_drive, data = new_row)
    
    message_output <- "Nous avons lancé l'interception du message, si votre cible enquête d'ici 15 minutes, vous recevrez directement le rapport d'enquête à sa place."
  }
  
  showModal(modalDialog(
    h3("Interception d'Enquête"),
    span(message_output),
    footer = tagList(modalButton("OK"))
  ))
  
  return(message_output)
}

info_indices <- function(actions){
  actions %>% 
    filter(user == "admin" & action ==  "init") %>% 
    group_by(cible,PA,resultat) %>% 
    filter(row_number() == n()) %>% 
    ungroup() %>% 
    select(titre = resultat,texte,variation=PA,indice=cible) %>% 
    arrange(indice,variation)
}

liste_indices <- function(user_name,values){
  
  # Liste des indices
  info_indices <- info_indices(values$actions) %>% 
    select(titre,texte,variation,indice)
  
  # Indice du User
  temp_indices <- values$actions %>% 
    filter(action ==  "init" & user == user_name) %>% 
    pull(cible) %>% unique()
    
  # temp_indices <- paste(values$actions[values$actions$user == user_name & values$actions$action ==  "init",]$cible)
    
  # V?rification des indices en cascade
  # liste_enquete_parfaite <- values$actions[values$actions$user == user_name & values$actions$resultat ==  3,]$cible
  
  # Vérification des indices en cascade
  temp_enquetes_parfaites <- values$actions %>% 
    filter(user == user_name & resultat ==  3) %>% 
    mutate(indice = paste0(cible,cible)) %>% 
    select(indice) %>% 
    left_join(info_indices %>% select(indice,variation)) %>% 
    filter(!is.na(variation)) %>% 
    pull(indice) %>% unique()
  
  # for (enquete in liste_enquete_parfaite){
  #   enquete_potentielle <- paste(enquete,enquete,sep='')
  #   if (nrow(info_indices[info_indices$indice == enquete_potentielle,]) > 0){
  #     temp_indices <- c(temp_indices,enquete_potentielle)
  #   }
  # }
  #   
  # temp_indices <- unique(temp_indices)
  
  liste_indices <- c(temp_indices,temp_enquetes_parfaites)
  
  print(info_indices)
  print(liste_indices)
  
  option_indices <- list()
  for (i in liste_indices){
    titre <- info_indices %>% 
      filter(indice==i & variation == 3) %>% 
      pull(titre)
    # titre <- info_indices[info_indices$indice==indice & info_indices$PA == 3,'titre']
    print(titre)
    option_indices[[titre]] <- i
  }
  
  return(option_indices)
}
