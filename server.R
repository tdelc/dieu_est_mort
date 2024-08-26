
#### Initialisation ####
library(shiny)
library(googlesheets4)
library(tidyverse)
library(DT) 
library(TheOpenAIR)

source("identification.R")
source("fonctions.R")

# Il faut créer son propre token pour ensuite le charger ici :
load("token_gs4")
gs4_auth(token=token_gs4)

id_drive <- "1lt9zMDbP5YtnNOMtialJzdFK-GtljU9qXqh2X0TvbNs"
# Il faut créer un google sheet avec les noms de colonnes suivants : id_session	user	action	cible	PA	timer	resultat	timer_ok	texte
# Il faut ensuite copier l'ID présent dans l'url sur l'objet id_chatgpt 

openai_api_key(id_chatgpt)

load('info_indices')
load('donnees_joueurs')
load('indices_dispo')

#### Serveur ####

shinyServer(function(input, output,session) {
  
  #### Données réactives ####
	
	USER <- reactiveValues(logged = FALSE)
	values <- reactiveValues(enquete_speciale="Non",url_active="",
	                         start=FALSE,serveur_admin = FALSE)
	
	#### Login initial ####
	
	observe({
	  
	  if (values$url_active != session$clientData$url_search){
	    
	    values$start <- FALSE
	    values$url_active <- session$clientData$url_search
	    
	    print(values$url_active)
	    
	    # Gestion PJ
	    if (str_detect(values$url_active,"\\?server=.*")){
	      values$id_session <- str_extract(values$url_active,"=.*")
	      values$id_session <- str_remove(values$id_session,"=")

	      values$serveur_admin <- FALSE
	      
	      values$actions <- googlesheets4::read_sheet(id_drive) %>% 
	        filter(id_session == values$id_session)

	      values$start <- nrow(values$actions)>0
	      
	      if (!values$start){
	        showModal(modalDialog(title = "Connexion","Le serveur n'existe pas"))
	      }
	    } else if (str_detect(values$url_active,"new_server")){
	      values$start <- TRUE
	      values$serveur_admin <- TRUE
	    } else {
	      if (!values$start){
	        showModal(modalDialog(title = "Connexion","L'URL ne fonctionne pas"))
	      }
	    }
	  }
	})
	
	#### Module création de serveur ####

	observeEvent(input$create_server, {
	  
	  server_name <- input$server_name
	  
	  if (server_name != ""){
	    
	    actions <- googlesheets4::read_sheet(id_drive) %>% 
	      filter(id_session == server_name)
	    print(actions)
	    print(nrow(actions)==0)
	    
	    if (nrow(actions)==0){
	      
	      # Création du dé et actions spéciales
	      actions <- prepare_row_drive(server_name,"admin","alea","1",1,NA,"",NA)
	      actions <- actions %>% 
	        add_row(prepare_row_drive(server_name,"admin","alea","2",4,NA,"",NA)) %>% 
	        add_row(prepare_row_drive(server_name,"admin","alea","3",1,NA,"",NA)) %>% 
	        add_row(prepare_row_drive(server_name,"admin","enq_spe","",0,NA,"Non",NA)) %>% 
	        add_row(prepare_row_drive(server_name,"admin","chat_gpt","",0,NA,"Non",NA))

	      # Mélange des indices de départ pour être sûr de pas faire apparaitre les indices importants en premier
	      indices_dispo <- indices_dispo[sample(nrow(indices_dispo)),]

	      for (i in 1:nrow(indices_dispo)){
	        actions <- actions %>% 
	          add_row(prepare_row_drive(server_name,indices_dispo$user[i],"init",
	                            indices_dispo$indice[i],0,NA,indices_dispo$titre[i],NA))
	      }
	      
	      # Ajout du texte
	      for (i in 1:nrow(info_indices)){
	        actions <- actions %>% 
	          add_row(prepare_row_drive(server_name,"admin","init",
	                                    info_indices$indice[i],
	                                    info_indices$variation[i],NA,
	                                    info_indices$titre[i],NA,
	                                    info_indices$texte[i]))
	      }
	      
	      sheet_append(id_drive, data = actions)
	      
	      output$info_server <- renderText({paste("Création du serveur opérationnel : ",input$server_name,sep="")})
	    }else{
	      output$info_server <- renderText({"Attention, ce nom de serveur existe déjà, choissisez en un autre."})
	    }
	  }
	})
	
	#### Module log début de session ####
	
	observe({
	  if (values$start){
	
    	output$info_log <- renderText({"Pour accéder à nos services, veuillez vous loguer"})
    
    	output$logged <- renderText({
    	  if (values$serveur_admin){
    	    'admin_serveur'
    	  } else if(USER$logged == TRUE){
    			if (USER$nom_user == 'admin'){
    				'admin'
    			}else{
    				"ok"
    			}
    		}else{
    			"not_ok"
    		}
    	})
    	outputOptions(output, "logged", suspendWhenHidden = FALSE)
    	
    	output$back <- renderImage({
    	    return(list(
    	      src = "images/back1.jpg",
    	      contentType = "image/jpg"
    	    ))
    	}, deleteFile = FALSE)
    
    	observeEvent(input$boutton_log, {
    		if (!is.null(input$username)) {
    			Username <- isolate(input$username)
    			Password <- isolate(input$password)
    			Id.username <- which(donnees_joueurs$user == Username)
    			Id.password <- which(donnees_joueurs$password == Password)
    			if (length(Id.username) > 0 & length(Id.password) > 0) {
    				if (Id.username %in% Id.password) {
    					# session$
    					USER$logged <- TRUE
    					USER$ligne_user <- Id.username
    					USER$nom_user <- Username
    					USER$PA_user <- donnees_joueurs[USER$ligne_user,'PA']
    					
    					# On recharge la DB
    					values$actions <- googlesheets4::read_sheet(id_drive) %>% 
    					  filter(id_session == values$id_session) %>% 
    					  mutate_if(is.list,as.character) %>% 
    					  mutate(timer = as.numeric(timer),
    					         timer_ok = as.numeric(timer_ok))
    					
    				}else{
    					output$info_log <- renderText({"Login ou mot de passe incorrect"})
    				}
    			}else{
    				output$info_log <- renderText({"Login ou mot de passe incorrect"})
    			}
    		}
    	})
    	
    	# Info sur le personnage, en barre latérale
    	observe({
    		if (USER$logged == TRUE) {
    			
    			# Nombre de PA actuel du joueur
    			PA_ini <- donnees_joueurs[USER$ligne_user,'PA']
    			
    			PA_actu <- values$actions %>% 
    			  filter(user == USER$nom_user) %>% summarise(sum(PA)) %>% pull()
    			
    			USER$PA_user <- as.numeric(PA_ini) - PA_actu
    			
    			# Panneau latéral du personnage
    			output$user_name <- renderText({USER$nom_user})
    			output$user_PA <- renderText({paste(as.character(USER$PA_user)," Point",ifelse(USER$PA_user >1,"s","")," de pouvoir",sep="")})
    		}
    	})
    	
    	# Info timer
    	
    	output$currentTime <- renderText({
    		invalidateLater(1000, session)
    		format(Sys.time(), "%H:%M:%S")
    	})
    	
    	###
    	#  Liens sur la page d'accueil
    	###
    	
    	observeEvent(input$link_enquete,{
    	  updateTabsetPanel(session,"panel_principal",selected = "Enquêter")
    	})
    	observeEvent(input$link_lecture,{
    	  updateTabsetPanel(session,"panel_principal",selected = "Lire les indices")
    	})
    	observeEvent(input$link_discussion,{
    	  updateTabsetPanel(session,"panel_principal",selected = "Discuter")
    	})
    	
    	
    	#### Module Enquête basique ####
    	
    	# Choix de l'enquête
    	
    	observe({
    		if (USER$logged == TRUE) {
    			option_indices <- liste_indices(USER$nom_user,values)
    			updateSelectInput(session, "choix_enquete",choices = option_indices)
    		}
    	})
    	
    	# PA et graphique de réussite
    	observe({
    		if (USER$logged == TRUE) {
    			
    			# Nombre de PA
    			updateNumericInput(session, "PA",max = USER$PA_user)
    			
    			# Tableau de probabilité
    			bonus_recherche <- input$PA-1
    			if (!is.numeric(input$PA)) bonus_recherche <- 0
    			 
    			# Création de la BDD dés
    			valeur_alea <- creation_bdd_des(values)

    			liste_alea <- rep(valeur_alea[,'resultat'],valeur_alea[,'valeur'])
    			
    			if (bonus_recherche > 0){
    				for (i in 1:bonus_recherche){
    					nb_resultats_reussi <- length(liste_alea[liste_alea==2])
    					liste_alea[liste_alea==2][sample(nb_resultats_reussi)[1]] <- 3
    				}
    			}
    			
    			prepa_tableau <- round(cbind(table(liste_alea))/length(liste_alea)*100,0)
    			rownames(prepa_tableau)[rownames(prepa_tableau) == 1] <- "Ratée"
    			rownames(prepa_tableau)[rownames(prepa_tableau) == 2] <- "Réussie"
    			rownames(prepa_tableau)[rownames(prepa_tableau) == 3] <- "Parfaite"
    			
    			texte_result <- paste(rownames(prepa_tableau)," - ",prepa_tableau," %",sep='')
    			
    			output$plot_proba <- renderPlot({
    				pie(prepa_tableau, labels = texte_result, 
    				    main="Probabilité de succès \nde l'enquête")
    			})
    		}
    	})
    	
    	# Lancement de l'enquête
    	observeEvent(input$boutton_enquete, {
    		if (!is.null(input$choix_enquete)){
    			if (input$choix_enquete != ""){
    				if (!is.numeric(input$PA) | input$PA == 0){
    					message_output <- "Petit futé.... Mais c'est râté !"
    				}else if (USER$PA_user < input$PA){
    					message_output <- "Vous n'avez plus assez de pouvoir pour effectuer cette mission"
    				}else{				
    					message_output <- recherche_indice(id_drive,values,USER$nom_user,
    					                                   input$choix_enquete,input$PA)
    				}
    			}
    		}else{
    			output$info <- renderText({"Veuillez sélectionner une enquête."})
    		}
    	})
    	
    	#### Module de recherche d'indices avancés ####
    
    	# Fonction pour pondérer les résultats
    	alea_pond <- function(mot,base){
    		nb_vec <- unlist(lapply(gregexpr(mot,base,ignore.case = TRUE),function(x) if (x[1] != -1) length(x) else 0))
    		result <- NULL
    		for(i in 1:length(nb_vec)){
    			if (nb_vec[i] != 0){
    				for (k in 1:nb_vec[i]){
    					result <- c(result,i)
    				}
    			}
    		}
    		result
    	}
    	
    	# Recherche d'indice avancée
    	observeEvent(input$boutton_enquete_advanced, {
    		
    		if (input$PA == 0){
    			message_output <- "Petit futé.... Mais c'est râté !"
    		}else if (USER$PA_user < input$PA){
    			message_output <- "Vous n'avez plus assez de pouvoir pour effectuer cette mission"
    		}else{
    		
    			recup1 <- NULL
    			recup1b <- NULL
    			recup2 <- NULL
    			recup2b <- NULL
    			recup3 <- NULL
    			recup3b <- NULL
    			
    			# indices non spéciaux
    			indices_basiques <- info_indices(values$actions) %>% 
    			  filter(nchar(indice) == 1 & variation == 2)

    			if (input$advanced_1 != ""){
    				recup1 <- alea_pond(input$advanced_1,indices_basiques$texte)
    				recup1b <- alea_pond(input$advanced_1,indices_basiques$titre)
    			}
    			if (input$advanced_2 != ""){
    				recup2 <- alea_pond(input$advanced_2,indices_basiques$texte)
    				recup2b <- alea_pond(input$advanced_2,indices_basiques$titre)
    			}
    			if (input$advanced_3 != ""){
    				recup3 <- alea_pond(input$advanced_3,indices_basiques$texte)
    				recup3b <- alea_pond(input$advanced_3,indices_basiques$titre)
    			}
    			
    			recup <- indices_basiques[c(recup1,recup2,recup3,recup1b,recup2b,recup3b),'indice']
    			
    			if (length(recup)>0){
    				message_output <- recherche_indice(id_drive,values,USER$nom_user,
    				                                   pull(sample(recup,1)),input$PA)
    				showModal(modalDialog(
    				  h3('Enquête'),
    				  span(message_output),
    				  footer = tagList(modalButton("OK"))
    				))
    			}else{
    				# Résultat aléatoire
    				enquete_alea <- pull(sample(indices_basiques[,'indice'],1))
    				message_output <- recherche_indice(id_drive,values,USER$nom_user,
    				                                   enquete_alea,input$PA,1)
    			}
    		}
    	})
    	
    	#### Module de Copie d'un indice ####
    	
    	observeEvent(input$boutton_enquete_copie, {
    		if (USER$PA_user < 2){
    			message_output <- "Vous n'avez plus assez de pouvoir pour effectuer cette mission"
    		}else{
    		  message_output <- copie_indice(values,USER$nom_user,input$choix_user_copie,2)
    		}
    	})
    	
    	#### Intercepter une enquête ####
    	
    	observeEvent(input$boutton_enquete_interception, {
    		if (USER$PA_user < 2){
    			message_output <- "Vous n'avez plus assez de pouvoir pour effectuer cette mission"
    		}else{
    		  message_output <- interception_indice(values,USER$nom_user,input$choix_user_interception,2)
    		}
    	})
    	
    	#### Module lecture des indices ####

    	observe({
    		if (USER$logged == TRUE) {
    		
    			# Vérification de l'activation des enquêtes spéciales
    			nb_car_enq <- 1
    			enq_spe <- values$actions[values$actions$action == "enq_spe",]$resultat
    			
    			if (length(enq_spe) == 0) enq_spe <- "Non"
    			if (enq_spe[length(enq_spe)] == "Oui") nb_car_enq <- 2
    	
    			# Création du tableau 
    			tableau_indices <- values$actions %>% 
    			  filter(user == USER$nom_user,
    			         action == "enquete"|action=="Enquete interceptee",
    			         timer_ok < Sys.time(),
    			         nchar(cible) <= nb_car_enq) 
    			if (nrow(tableau_indices) > 0 ){

    				tableau_indices <- tableau_indices %>% 
    				  rename(indice=cible) %>% 
    				  left_join(info_indices(values$actions) %>% 
    				              filter(variation == 3) %>% 
    				              select(titre,indice)) %>% 
    				  mutate(texte = ifelse(action == "Enquete interceptee","Malheureusement, une équipe surnaturelle (nous ne savons dire s'il s'agissait de démons ou d'anges) nous est tombée dessus, et nous a intercepté le rapport de l'enquête avant que nous puissons vous le transmettre. Nous vous promettons que cela n'arrivera plus jamais.",texte),
    				         resultat = case_when(
    				           resultat == 1 ~ "Enquête ratée",
    				           resultat == 2 ~ "Enquête reussie",
    				           resultat == 3 ~ "Enquête parfaite",
    				         )
    				        )
    				
    				tableau_indices <- tableau_indices %>% 
    				  arrange(desc(timer_ok)) %>% 
    				  select(titre,PA,resultat,texte)
    				
    				tableau_indices$texte <- gsub(pattern = "\n", replacement = "<br/>", x = tableau_indices$texte)
    
    				colnames(tableau_indices) <- c("Enquête","PA utilisés","Résultat","Rapport d'enquête")
    				
    				output$tableau_indices = DT::renderDataTable(
    				  tableau_indices, rownames = FALSE,escape=FALSE,
    					options = list(
    					ordering = FALSE,
    					info = FALSE
    				)
    				)
    				
    				output$tableau_indices_lite = DT::renderDataTable(
    				  tableau_indices[1,], rownames = FALSE,escape=FALSE,
    					options = list(
    						paging = FALSE,
    						searching = FALSE,
    						ordering = FALSE,
    						info = FALSE
    					)
    				)
    			}
    		}
    	})
    				
    	#### Module Chat ####
    	
    	observe({
    		if (USER$logged == TRUE) {
    			if (input$ano_chat){
    				output$info_chat <- renderText("(cela vous coûtera 1 point de pouvoir)")
    			}else{
    				output$info_chat <- renderText("")
    			}
    		}
    	})
    
    	observe({
    		if (USER$logged == TRUE) {
    			tableau_chat <- values$actions %>% 
    			  filter(cible == USER$nom_user,
    			         action == "chat",
    			         timer_ok < Sys.time()) %>% 
    			  select(user,cible,PA,resultat,timer_ok,resultat)
    			
    			if (nrow(tableau_chat) > 0)
    				tableau_chat[tableau_chat$PA == 1,'user'] <- "Anonyme"
    			
    			tableau_chat <- tableau_chat %>% 
    			  arrange(desc(timer_ok)) %>% 
    			  select(user,timer_ok,resultat)
    
    			tableau_chat$timer_ok <- format(as.POSIXct(tableau_chat$timer_ok, origin = "1970-01-01", tz = "Europe/Paris"),format="%H:%M:%S")
    			
    			tableau_chat$resultat <- gsub(pattern = "\n", replacement = "<br/>", x = tableau_chat$resultat)
    			
    			colnames(tableau_chat) <- c("Expéditeur","Date et heure d'envoi","Message")
    		
    			if (nrow(tableau_chat)>0){				
    				output$tableau_chat = DT::renderDataTable(
    				  tableau_chat,
    					rownames = FALSE,escape=FALSE,
    					options = list(
    						paging = FALSE,
    						searching = FALSE,
    						ordering = FALSE,
    						info = FALSE
    					)
    				)
    				output$tableau_chat_lite = DT::renderDataTable(
    				  tableau_chat[1,],
    					rownames = FALSE,escape=FALSE,
    					options = list(
    						paging = FALSE,
    						searching = FALSE,
    						ordering = FALSE,
    						info = FALSE
    					)
    				)
    			}
    		}
    	})
    	
    	observeEvent(input$boutton_chat, {
    
    		ano <- 0
    		if (input$ano_chat) ano <- 1
    		
    		if (ano == 1 & USER$PA_user < 1){
    			message_output <- "Vous n'avez plus assez de pouvoir pour effectuer cette mission"
    		}else{
    		  new_row <-  prepare_row_drive(values$id_session,USER$nom_user,"chat",input$choix_user_chat,
    		    ano,Sys.time(),input$message_chat,Sys.time()+input$timer_chat*60)
    		  
    		  sheet_append(id_drive, data = new_row)
    		
    			message_output <- paste("Le message est envoyé. ",input$choix_user_chat," le recevra dans ",input$timer_chat," minute(s)",sep='')
    		}
    		
    		showModal(modalDialog(
    		  h3('Messagerie'),
    		  span(message_output),
    		  footer = tagList(modalButton("OK"))
    		))
    		
    		updateCheckboxInput(session,"ano_chat",value = FALSE)
    	})
    	
    	#### Module d'administration de la murder ####
    	
    		##### Module de vérification des actions ####
    
    		observe({
    			if (USER$logged == TRUE) {
    			if (USER$nom_user == "admin") {
    			  
    			  verif <- values$actions %>% 
    			    filter(action != "init", user != "admin") %>% 
    			    arrange(desc(timer_ok))
    			  
    				verif$timer_ok <- format(as.POSIXct(verif$timer_ok, origin = "1970-01-01", tz = "Europe/Paris"),format="%H:%M:%S")
    				verif$timer <- format(as.POSIXct(verif$timer, origin = "1970-01-01", tz = "Europe/Paris"),format="%H:%M:%S")
    				
    				output$table_admin = DT::renderDataTable(
    				  verif, rownames = FALSE
    				)
    			}}
    		})
    		
    		##### Module de vérification des indices obtenus ####
    		
    		observe({
    		  if (USER$logged == TRUE) {
    		    if (USER$nom_user == "admin") {
    		      
    		      # on prend la liste des indices possibles
    		      liste_indices <- info_indices(values$actions) %>% 
    		        select(indice,titre,variation,texte)
    		      liste_indices$ok <- 0
    		      liste_indices$qui <- ""
    		      for (i in 1:nrow(liste_indices)){
    		        indice <- liste_indices[i,"indice"]
    		        resultat <- liste_indices[i,"variation"]
    		        sub_bdd <- values$actions %>% filter(cible==indice)
    		        
    		        if (resultat %in% sub_bdd$resultat){
    		          liste_indices[liste_indices$indice == indice & liste_indices$variation == resultat,"ok"] <- 1
    		          liste_indices[liste_indices$indice == indice & liste_indices$variation == resultat,"qui"] <- paste(sub_bdd[sub_bdd$resultat == resultat,"user"],collapse = ",")
    		          liste_indices[liste_indices$indice == indice & liste_indices$variation != resultat,"ok"] <- 0
    		        }
    		      }
    		        
    	        liste_indices <- liste_indices[liste_indices$ok == 1,c("indice","titre","variation","qui","texte")]
    	        
    	        output$table_indices_admin = DT::renderDataTable(
    	          liste_indices, rownames = FALSE
    	        )
    		        
    		    }}
    		})
    		
    		##### Module de vérification des infos PJ ####
    		
    		observe({
    			if (USER$logged == TRUE) {
    			if (USER$nom_user == "admin") {
    			
    				# Nombre de PA de chaque joueur
    				donnees_joueurs_update <- donnees_joueurs
    				donnees_joueurs_update$PA_actu <- 0
    				
    				for (i in 1:nrow(donnees_joueurs_update)){
    					if (nrow(values$actions[values$actions$user == donnees_joueurs_update[i,'user'],]) > 0){
    						donnees_joueurs_update[i,'PA_actu'] <- as.numeric(donnees_joueurs_update[i,'PA']) - sum(values$actions[values$actions$user == donnees_joueurs_update[i,'user'],'PA'])
    					}else{
    						donnees_joueurs_update[i,'PA_actu'] <- donnees_joueurs_update[i,'PA']
    					}
    				}
    			
    				output$table_admin_PJ = DT::renderDataTable(
    					donnees_joueurs_update, rownames = FALSE
    				)
    			
    			}}
    		})
    		
    		##### Module de modification des indices ####
    		
    		output$table_admin_indices <- renderDT({
    		  
    		  datatable(info_indices(values$actions) %>% 
    		              select(indice,titre,variation,texte)
    		              , editable = TRUE)
    		})
    		
    		observeEvent(input$table_admin_indices_cell_edit, {
    		  
    		  print(info_indices(values$actions))
    		  
    		  temp_indices <- info_indices(values$actions) %>% 
    		    select(indice,titre,variation,texte)

    		  row  <- input$table_admin_indices_cell_edit$row
    		  
    		  indice <- pull(temp_indices[row,1])
    		  titre <- pull(temp_indices[row,2])
    		  variation <- pull(temp_indices[row,3])
    		  texte <- input$table_admin_indices_cell_edit$value
    		  
    		  new_row <- prepare_row_drive(values$id_session,"admin","init",
    		                               indice,variation,Sys.time(),
    		                               titre,Sys.time(),texte)
    		  
    		  sheet_append(id_drive,data = new_row)
    		  
    		})
    		
    		# Téléchargement de tous les indices
    		output$downloadIndices <- downloadHandler(
    		  filename = function() {
    		    paste('indices-dieu_', Sys.Date(), '.csv', sep='')
    		  },
    		  content = function(con) {
    		    write_excel_csv2(info_indices(values$actions), con)
    		  }
    		)
    		
    		observeEvent(input$loadIndices, {
    		  
    		  file <- input$loadIndices
    		  ext <- tools::file_ext(file$datapath)
    		  
    		  req(file)
    		  validate(need(ext == "csv", "Veuillez charger un csv"))
    		  
    		  input <- read_csv2(file$datapath)
    		  
    		  new_row <- prepare_row_drive(values$id_session,"admin","init",
    		                               input$indice,input$variation,
    		                               Sys.time(),input$titre,
    		                               Sys.time(),input$texte)
    		  
    		  sheet_append(id_drive,data = new_row)
    		  
    		  # On recharge la DB
    		  values$actions <- googlesheets4::read_sheet(id_drive) %>% 
    		    filter(id_session == values$id_session) %>% 
    		    mutate_if(is.list,as.character) %>% 
    		    mutate(timer = as.numeric(timer),
    		           timer_ok = as.numeric(timer_ok))
    		  
    		})
    		
    		##### Module de modification des PA ####
    
    		observeEvent(input$boutton_admin_PA, {
    			timer_now <- Sys.time()
    			if (input$choix_user_PA == "All"){
    			  new_rows <- NULL
    				for (pj in donnees_joueurs$user){
    					if (pj == donnees_joueurs$user[1])
    					  new_row <- prepare_row_drive(values$id_session,pj,"admin_PA","",
    					                                -input$PA_admin,timer_now,0,timer_now)
    					else
    					  new_row <- new_row %>% 
    					    add_row(prepare_row_drive(values$id_session,pj,"admin_PA","",
    					              -input$PA_admin,timer_now,0,timer_now))
    				}
    			  sheet_append(id_drive, data = new_row)
    			  message_output <- paste(input$PA_admin," PA attribué à tous",sep="")

    			}else{
    				new_row <-  prepare_row_drive(values$id_session,input$choix_user_PA,
    				              "admin_PA","",-input$PA_admin,timer_now,0,timer_now)
    				sheet_append(id_drive, data = new_row)
    				
    				message_output <- paste(input$PA_admin," PA attribué à ",input$choix_user_PA,sep="")
    			}
    			output$info <- renderText({message_output})
    			
    		})
    		
    		##### Module d'ajout des indices ####
    		
    		observe({
    			if (USER$logged == TRUE) {
    			if (USER$nom_user == "admin") {
    			
    				option_variations <- c("Enquête ratée" = 1,"Enquête réussie" = 2,"Enquête parfaite" = 3)
    				updateSelectInput(session,"choix_variation",choices = option_variations)
    				
    				liste_indices <- info_indices(values$actions) %>% 
    				  filter(variation == 3) %>% 
    				  select(titre,texte,variation,indice)
    				
    				option_indices <- list()
    				for (i in liste_indices$indice){
    				  titre <- liste_indices %>% filter(indice==i) %>% pull(titre)
    				  option_indices[[titre]] <- i
    				}
    		
    				updateSelectInput(session, "choix_enquete_admin",
    				  choices = option_indices
    				)
    			
    			}}
    		})
    		
    		
    		observe({
    			if (!is.null(input$choix_enquete_admin)){
    			if (input$choix_enquete_admin != "dynamique"){
    			  
    			  ligne_indice <- info_indices(values$actions) %>% 
    			    filter(indice == input$choix_enquete_admin)
    			  
    			  output$admin_indice_lettre <- renderText({input$choix_enquete_admin})
    			  output$admin_indice_1 <- renderText({ligne_indice %>% 
    			      filter(variation == 1) %>% pull(texte)})
    			  output$admin_indice_2 <- renderText({ligne_indice %>% 
    			      filter(variation == 2) %>% pull(texte)})
    			  output$admin_indice_3 <- renderText({ligne_indice %>% 
    			      filter(variation == 3) %>% pull(texte)})

    			}}
    		})
    		
    		observeEvent(input$boutton_admin_indice, {
    		  
    			message_output <- try(recherche_indice(id_drive,values,
    			                                       input$choix_user_indice,
    			                                       input$choix_enquete_admin,
    			                                       0,input$choix_variation,0
    			                                       ),silent=TRUE)
    			if (class(message_output) != "try-error")
    				output$info <- renderText({paste(input$choix_enquete_admin," attribué à ",input$choix_user_indice,sep="")})
    		})
    	
    		##### Module de modification du dé ####
    
    		observe({
    			if (USER$logged == TRUE) {
    			if (USER$nom_user == "admin") {
    			
    			  # Création de la BDD dés
    			  valeur_alea <- creation_bdd_des(values)
    			  
    				liste_alea <- rep(valeur_alea[,'resultat'],valeur_alea[,'valeur'])
    				tableau_proba <- cbind(table(liste_alea))

    				nb_echec <- try(tableau_proba[1,],silent=TRUE)
    				nb_reussite <- try(tableau_proba[2,],silent=TRUE)
    				nb_reussite_parfaite <- try(tableau_proba[3,],silent=TRUE)
    				if (class(nb_echec) == "try-error") nb_echec <- 0
    				if (class(nb_reussite) == "try-error") nb_reussite <- 0
    				if (class(nb_reussite_parfaite) == "try-error") nb_reussite_parfaite <- 0
    
    				updateNumericInput(session, "Nb_echec",
    					value = as.character(nb_echec)
    				)
    				updateNumericInput(session, "Nb_reussite",
    					value = as.character(nb_reussite)
    				)
    				updateNumericInput(session, "Nb_reussite_parfaite",
    					value = as.character(nb_reussite_parfaite)
    				)
    				
    				prepa_tableau <- round(cbind(table(liste_alea))/length(liste_alea)*100,0)
    				rownames(prepa_tableau)[rownames(prepa_tableau) == 1] <- "Ratée"
    				rownames(prepa_tableau)[rownames(prepa_tableau) == 2] <- "Réussie"
    				rownames(prepa_tableau)[rownames(prepa_tableau) == 3] <- "Parfaite"
    				texte_result <- paste(rownames(prepa_tableau)," - ",prepa_tableau," %",sep='')
    				
    				texte_result <- paste(rownames(prepa_tableau)," - ",prepa_tableau," %",sep='')
    				output$plot_proba_admin <- renderPlot({
    					pie(tableau_proba, labels = texte_result, main="Probabilité de succès \nde l'enquête")
    				})
    			
    			}}
    		})
    		
    		observeEvent(input$boutton_admin_des, {
    		  
    		  timer_ok <- Sys.time()
    		  
    		  new_row <- prepare_row_drive(values$id_session,"admin","alea",1,
    		                               input$Nb_echec,timer_ok,"",timer_ok) %>% 
    		    add_row(prepare_row_drive(values$id_session,"admin","alea",2,
    		                              input$Nb_reussite,timer_ok,"",timer_ok)) %>% 
    		    add_row(prepare_row_drive(values$id_session,"admin","alea",3,
    		                              input$Nb_reussite_parfaite,timer_ok,"",timer_ok)) 
    		  
    			sheet_append(id_drive, data = new_row)
    		})
    		
    		##### Module des actions spéciales ####
    		
    		observeEvent(input$enquete_speciale_on, {
    			output$admin_enquete_speciale <- renderText("Enquête spéciale activée")
    			new_row <- prepare_row_drive(values$id_session,"admin","enq_spe",NA,0,
    			              Sys.time(),"Oui",Sys.time())
    			sheet_append(id_drive, data = new_row)
    		})
    		observeEvent(input$enquete_speciale_off, {
    			output$admin_enquete_speciale <- renderText("Enquête spéciale desactivée")
    			new_row <- prepare_row_drive(values$id_session,"admin","enq_spe",NA,0,
    			              Sys.time(),"Non",Sys.time())
    			sheet_append(id_drive, data = new_row)
    		})
    		
    		observeEvent(input$enquete_chat_gpt_on, {
    		  output$admin_enquete_speciale <- renderText("Modification par Chat GPT activée")
    		  new_row <- prepare_row_drive(values$id_session,"admin","chat_gpt",NA,0,
    		                               Sys.time(),"Oui",Sys.time())
    		  sheet_append(id_drive, data = new_row)
    		})
    		observeEvent(input$enquete_chat_gpt_off, {
    		  output$admin_enquete_speciale <- renderText("Modification par Chat GPT desactivée")
    		  new_row <- prepare_row_drive(values$id_session,"admin","chat_gpt",NA,0,
    		                               Sys.time(),"Non",Sys.time())
    		  sheet_append(id_drive, data = new_row)
    		})
	  }
	})
})