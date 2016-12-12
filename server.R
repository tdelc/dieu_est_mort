
library(shiny)
library(datasets) 
library(doBy)
library(lipsum)

# if (!require("DT")) install.packages('DT')
library(DT) 

# Liste des indices et des indices de départ
source('BDD_base.R')

# indices_dispo
# info_indices

# Modif ici pour la murder
info_indices[info_indices$variation == 1,"variation"] <- "Enquête râtée"
info_indices[info_indices$variation == 2,"variation"] <- "Enquête réussie"
info_indices[info_indices$variation == 3,"variation"] <- "Enquête parfaite"

# Mélange des indices de départ pour être sûr de pas faire apparaitre les indices importants en premier
indices_dispo <- indices_dispo[sample(nrow(indices_dispo)),]

# Pour test
for (indice in unique(info_indices[,'indice'])){
	info_indices[info_indices$indice == indice,"titre"] <- lipsums[sample(1000)][1]
}
for (i in 1:nrow(info_indices)){
	info_indices[i,"texte"] <- paste(lipsum[sample(100)][2:sample(5)[1]],collapse='. ')
}
info_indices <- info_indices[nchar(info_indices$indice) == 1,]

donnees_joueurs <- data.frame(user=NA,password=NA,PA=NA)
donnees_joueurs[1,] <- c("Baal","Baal",6)
donnees_joueurs[2,] <- c("Dominique","Dominique",5)
donnees_joueurs[3,] <- c("Yves","Yves",5)
donnees_joueurs[4,] <- c("Valefor","Valefor",6)
donnees_joueurs[5,] <- c("Andréalphus","Andréalphus",5)
donnees_joueurs[6,] <- c("Jésus","Jésus",5)
donnees_joueurs[7,] <- c("Blandine","Blandine",5)
donnees_joueurs[8,] <- c("Malphas","Malphas",6)
donnees_joueurs[9,] <- c("Crocell","Crocell",5)
donnees_joueurs[10,] <- c("admin","admin",5)

# Tableau récap de toute la séance
actions <- data.frame(user=NA,action=NA,cible=NA,PA=0,timer=NA,resultat=NA,timer_ok=NA)
actions <- actions[-1,]

# Valeur du dé, modifiable pour l'admin
valeur_alea <- data.frame(resultat=NA,valeur=NA)
valeur_alea[1,] <- c("Enquête râtée",1)
valeur_alea[2,] <- c("Enquête réussie",4)
valeur_alea[3,] <- c("Enquête parfaite",1)
valeur_alea$valeur <- as.numeric(valeur_alea$valeur) 

# Fonction de recherche indices
recherche_indice <- function(user_name,user_PA,enquete,PA,tableau_des,variation=NA,duree=NA){
	if (PA > user_PA){
		message_output <- "Vous n'avez plus assez de pouvoir pour effectuer cette mission"
	}else{
		
		if (is.na(duree)){ 
			# Durée aléatoire de recherche
			alea_duree <- sample(c(2:10),1)
			# DEBUG !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
			alea_duree <- 0
			if (nchar(enquete) == 1){
				# Enquête normale
				message_output <- paste("L'équipe de recherche est partie se renseigner. Vous devriez avoir une réponse dans ",alea_duree," minutes environ",sep="")
			}else{
				# Enquête spéciale
				message_output <- paste("L'équipe de recherche est partie se renseigner. Cette enquête sera particulièrement longue. Nous espèrons avoir des réponses 1 heure avant la fin de votre soirée.",sep="")
			}
		}else{
			alea_duree <- duree
		}
		timer_ok <- Sys.time()+alea_duree*60
		
		if (is.na(variation)){
			# Lancement du dé
			liste_resultats <- rep(tableau_des[,'resultat'],tableau_des[,'valeur'])
			id_variation <- sample(1:length(liste_resultats),1)
			variation <- liste_resultats[id_variation]
			# Echec critique
			if (variation != "Enquête râtée"){
				id_variation <- id_variation + PA - 1
				variation <- liste_resultats[min(id_variation,length(liste_resultats))]
			}
		}
		
		# Savoir s'il y a une interception
		recup_indice <- values$actions[values$actions$cible == user_name & values$actions$action == "interception" & values$actions$timer < Sys.time(),]		
		
		if (nrow(recup_indice) > 0){
		
			recup_indice <- recup_indice[nrow(recup_indice),]
			
			# La victime ne recoit que l'info enquête interceptée
			values$actions[nrow(values$actions)+1,] <-  list(user_name,"enquete interceptée",enquete,PA,Sys.time(),variation,timer_ok)
			
			# L'interception actuelle n'est plus possible
			values$actions[values$actions$cible == user_name & values$actions$action == "interception" & values$actions$timer < Sys.time(),'action'] <- "interception ok"
			
			user_effectif <- recup_indice[,'user']
			PA_effectif <- 0
		}else{		
			# Normal
			user_effectif <- user_name
			PA_effectif <- PA
		}

		# Enregistrement du résultat
		values$actions[nrow(values$actions)+1,] <-  list(user_effectif,"enquete",enquete,PA_effectif,Sys.time(),variation,timer_ok)

		# Vérification des indices en cascade
		if (variation == "Enquête parfaite"){
			enquete_potentielle <- paste(enquete,enquete,sep='')
			if (nrow(info_indices[info_indices$indice == enquete_potentielle,]) > 0){
				values$indices_dispo[nrow(values$indices_dispo)+1,] <- list(user_effectif,enquete_potentielle)
			}
		}	
		# if (enquete == "B" & variation == "Enquête parfaite"){
			# values$indices_dispo[nrow(values$indices_dispo)+1,] <- list(user_effectif,"DD")
		# }
		# if (enquete == "F" & variation == "Enquête parfaite"){
			# values$indices_dispo[nrow(values$indices_dispo)+1,] <- list(user_effectif,"FF")
		# }
		# if (enquete == "I" & variation == "Enquête parfaite"){
			# values$indices_dispo[nrow(values$indices_dispo)+1,] <- list(user_effectif,"II")
		# }
	}
	return(message_output)
}

values <- reactiveValues(actions = actions,indices_dispo=indices_dispo,valeur_alea=valeur_alea,enquete_speciale="Non")

shinyServer(function(input, output,session) {
	
	USER <- reactiveValues(logged = FALSE)
	
	output$info_log <- renderText({"Pour accéder à nos services, veuillez vous loguer"})

	output$logged <- renderText({
		if(USER$logged == TRUE){
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
	
	observeEvent(input$boutton_log, {
		if (!is.null(input$username)) {
			Username <- isolate(input$username)
			Password <- isolate(input$password)
			Id.username <- which(donnees_joueurs$user == Username)
			Id.password <- which(donnees_joueurs$password == Password)
			if (length(Id.username) > 0 & length(Id.password) > 0) {
				if (Id.username == Id.password) {
					# session$
					USER$logged <- TRUE
					USER$ligne_user <- Id.username
					USER$nom_user <- Username
					USER$PA_user <- donnees_joueurs[USER$ligne_user,'PA']
				}else{
					output$info_log <- renderText({"Login ou mot de passe incorrect"})
				}
			}else{
				output$info_log <- renderText({"Informations insuffisantes"})
			}
		}
	})
	
	# Recherche d'indice classique
	observeEvent(input$boutton_enquete, {
		if (!is.null(input$choix_enquete) & input$choix_enquete != ""){
			message_output <- recherche_indice(USER$nom_user,USER$PA_user,input$choix_enquete,input$PA,values$valeur_alea)
			output$info <- renderText({message_output})
		}else{
			output$info <- renderText({"Veuillez sélectionner une enquête."})
		}
	})
	
	# Recherche d'indice avancée
	observeEvent(input$boutton_enquete_advanced, {
		
		recup1 <- NULL
		recup1b <- NULL
		recup2 <- NULL
		recup2b <- NULL
		recup3 <- NULL
		recup3b <- NULL
		
		# indices non spéciaux
		indices_basiques <- info_indices[nchar(info_indices$indice) == 1,]
		if (input$advanced_1 != ""){
			recup1 <- grep(input$advanced_1,indices_basiques$texte,ignore.case = TRUE)
			recup1b <- grep(input$advanced_1,indices_basiques$titre,ignore.case = TRUE)
		}
		if (input$advanced_2 != ""){
			recup2 <- grep(input$advanced_2,indices_basiques$texte,ignore.case = TRUE)
			recup2b <- grep(input$advanced_2,indices_basiques$titre,ignore.case = TRUE)
		}
		if (input$advanced_3 != ""){
			recup3 <- grep(input$advanced_3,indices_basiques$texte,ignore.case = TRUE)
			recup3b <- grep(input$advanced_3,indices_basiques$titre,ignore.case = TRUE)
		}
		
		# recup <- info_indices[c(recup1,recup2,recup3),'indice']
		recup <- indices_basiques[c(recup1,recup2,recup3,recup1b,recup2b,recup3b),'indice']
		
		if (length(recup)>0){
			message_output <- recherche_indice(USER$nom_user,USER$PA_user,sample(recup,1),input$PA,values$valeur_alea)
			output$info <- renderText({message_output})
		}else{
			# Résultat aléatoire
			enquete_alea <- sample(indices_basiques[,'indice'],1)
			message_output <- recherche_indice(USER$nom_user,USER$PA_user,enquete_alea,input$PA,values$valeur_alea,"Enquête râtée")
			output$info <- renderText({message_output})
		}
		
	})
	
	# Copie d'un indice
		
	observeEvent(input$boutton_enquete_copie, {
		
		if (2 > USER$PA_user){
			message_output <- "Vous n'avez plus assez de pouvoir pour effectuer cette mission"
		}else{
			timer_now <- Sys.time()
			
			values$actions[nrow(values$actions)+1,] <-  list(USER$nom_user,"copie",input$choix_user_copie,2,timer_now,0,timer_now-6*60)
			
			recup_indice <- values$actions[values$actions$user == input$choix_user_copie & values$actions$action == "enquete" & timer_now <= values$actions$timer_ok+6*60,]
			
			if (nrow(recup_indice) > 0){
			
				recup_indice <- recup_indice[nrow(recup_indice),]
				values$actions[nrow(values$actions)+1,] <-  list(USER$nom_user,"enquete",recup_indice[,'cible'],0,timer_now,recup_indice[,'resultat'],timer_now)
				
				message_output <- paste("Nous avons obtenu une copie de l'enquête suivante : ",info_indices[info_indices$indice==recup_indice[,'cible'] & info_indices$variation == "Enquête parfaite",'titre'],sep='')
			}else{
				message_output <- "Nous n'avons rien pu copier comme enquête"
			}
		}
		output$info <- renderText({message_output})
	})
	
	# Intercepter une enquête
	observeEvent(input$boutton_enquete_interception, {
		
		if (2 > USER$PA_user){
			message_output <- "Vous n'avez plus assez de pouvoir pour effectuer cette mission"
		}else{
		
			timer_now <- Sys.time()
			
			if (sample(1:6,1) == 1){
				values$actions[nrow(values$actions)+1,] <-  list(USER$nom_user,"interception ratée",input$choix_user_interception,2,timer_now,0,timer_now)
				message_output <- "Malheureusement, nos équipes se sont faites directement captées par l'équipe concurrente. L'interception sera un échec."	
			}else{
				values$actions[nrow(values$actions)+1,] <-  list(USER$nom_user,"interception",input$choix_user_interception,2,timer_now,0,timer_now+20*60)
				
				message_output <- "Nous avons lancé l'interception du message, si celle-ci est un succès, vous recevrez directement le rapport d'enquête"
			}
		}
		output$info <- renderText({message_output})
	})
	
	# Chat
	observeEvent(input$boutton_chat, {

		ano <- 0
		if (input$ano_chat) ano <- 1
		
		if (ano == 1 & 1 > USER$PA_user){
			message_output <- "Vous n'avez plus assez de pouvoir pour effectuer cette mission"
		}else{
		
			values$actions[nrow(values$actions)+1,] <-  list(USER$nom_user,"chat",input$choix_user_chat,ano,Sys.time(),input$message_chat,Sys.time()+input$timer_chat*60)
			
			updateCheckboxInput(session,"ano_chat",value = FALSE)
		
			message_output <- paste("Le message est envoyé. ",input$choix_user_chat," le recevra dans ",input$timer_chat," minute(s)",sep='')
		}
		output$info <- renderText({message_output})
	})
	
	# Admin
	observeEvent(input$boutton_admin_PA, {
		timer_now <- Sys.time()
		if (input$choix_user_PA == "All"){
			for (pj in donnees_joueurs$user){
				values$actions[nrow(values$actions)+1,] <-  list(pj,"admin_PA","",-input$PA_admin,timer_now,0,timer_now)
			}
			output$info <- renderText({paste(input$PA_admin," PA attribué à tous",sep="")})
		}else{
			values$actions[nrow(values$actions)+1,] <-  list(input$choix_user_PA,"admin_PA","",-input$PA_admin,timer_now,0,timer_now)
			output$info <- renderText({paste(input$PA_admin," PA attribué à ",input$choix_user_PA,sep="")})
		}
	})

	observeEvent(input$boutton_admin_indice, {
		message_output <- recherche_indice(input$choix_user_indice,10,input$choix_enquete_admin,0,values$valeur_alea,input$choix_variation,0)
		output$info <- renderText({paste(input$choix_enquete_admin," attribué à ",input$choix_user_indice,sep="")})
	})
	
	observeEvent(input$boutton_admin_des, {
	
		values$valeur_alea[values$valeur_alea$resultat=="Enquête râtée",'valeur'] <- input$Nb_echec
		values$valeur_alea[values$valeur_alea$resultat=="Enquête réussie",'valeur'] <- input$Nb_reussite
		values$valeur_alea[values$valeur_alea$resultat=="Enquête parfaite",'valeur'] <- input$Nb_reussite_parfaite
		
	})
				
	observe({
				
		write.csv(values$actions, paste("sauv_actions_","16122016",".csv",sep=""))
		
	})
	observe({
	
		if (USER$logged == TRUE) {
			
			# Récupération d'info sur le personnage
				# Liste des indices 
				liste_indices <- paste(values$indices_dispo[values$indices_dispo$user == USER$nom_user,'indice'])
				# Nombre de PA actuel du joueur
				PA_actu <- donnees_joueurs[USER$ligne_user,'PA']
				
				if (nrow(values$actions[values$actions$user == USER$nom_user,]) > 0){
					PA_actu <- as.numeric(PA_actu) - sum(values$actions[values$actions$user == USER$nom_user,'PA'])
				}
				USER$PA_user <- PA_actu
			
			# Panneau latéral du personnage
			output$user_name <- renderText({input$username})
			output$user_PA <- renderText({as.character(PA_actu)})
		
			# updateTextInput(session, "action1", label = donnees_joueurs[USER$ligne_user,'action'])
			
			# Onglet enquête
				
				# Create a list of new options, where the name of the items is something
				# like 'option label x 1', and the values are 'option-x-1'.
				option_indices <- list()
				for (indice in liste_indices){
					option_indices[[info_indices[info_indices$indice==indice & info_indices$variation == "Enquête parfaite",'titre']]] <- indice
					option_indices[[""]] <- ""
				}
				
				# Choix de l'enquête
				updateSelectInput(session, "choix_enquete",
				  choices = option_indices,
				  # selected = tail(x, 1)
				)
				
				# Nombre de PA
				updateNumericInput(session, "PA",
					max = PA_actu
				)
				
				# Tableau de probabilité
				bonus_recherche <- input$PA-1
				
				liste_resultats <- rep(values$valeur_alea[,'resultat'],values$valeur_alea[,'valeur'])
				
				if (bonus_recherche > 0){
					for (i in 1:bonus_recherche){
						nb_resultats_reussi <- length(liste_resultats[liste_resultats=="Enquête réussie"])
						liste_resultats[liste_resultats=="Enquête réussie"][sample(nb_resultats_reussi)[1]] <- "Enquête parfaite"
					}
				}
				
				prepa_tableau <- round(cbind(table(liste_resultats))/length(liste_resultats)*100,0)
				texte_result <- paste(rownames(prepa_tableau)," - ",prepa_tableau," %",sep='')
				
				output$plot_proba <- renderPlot({
					pie(prepa_tableau, labels = texte_result, main="Probabilité de succès \nde la mission")
				})

			# Onglet indices
			
				# Vérification de l'activation des enquêtes spéciales
				nb_car_enq <- 1
				if (values$enquete_speciale == "Oui")
					nb_car_enq <- 2
									
				tableau_indices <- subset(values$actions,user == USER$nom_user & (action == "enquete"|action=="enquete interceptée") & timer_ok < Sys.time() & nchar(cible) <= nb_car_enq)
				tableau_indices$titre <- NULL
				tableau_indices$texte <- NULL
				if (nrow(tableau_indices) > 0 ){
					for (i in 1:nrow(tableau_indices)){
						tableau_indices$titre[i] <- info_indices[info_indices$indice == tableau_indices$cible[i] & info_indices$variation == tableau_indices$resultat[i],'titre']
						
						if (tableau_indices$action[i] == "enquete interceptée"){
							tableau_indices$texte[i] <- "Malheureusement, une équipe surnaturelle (nous ne savons dire s'il s'agissait de démons ou d'anges) nous sont tombés dessus, et nous ont intercepté le rapport de l'enquête avant que nous puissons vous le transmettre. Nous vous promettons que cela n'arrivera plus jamais."
						}else{
							tableau_indices$texte[i] <- info_indices[info_indices$indice == tableau_indices$cible[i] & info_indices$variation == tableau_indices$resultat[i],'texte']
						}
					}
				
					tableau_indices <- subset(orderBy(~-timer_ok,tableau_indices),select=c(titre,PA,texte))

					# tableau_indices$texte <- gsub(pattern = "\n", replacement = "<br/>", x = tableau_indices$texte)

					tableau_indices$texte <- gsub(pattern = "\n", replacement = "<br/>", x = tableau_indices$texte)
					
					colnames(tableau_indices) <- c("Enquête demandée","Points d'action utilisé(s)","Résultat de l'enquête")
					
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
				
				# datatable(head(mtcars), rownames = FALSE)  # no row names
				
			# Onglet Chat
				tableau_chat <- subset(values$actions,cible == USER$nom_user & action == "chat" &
						timer_ok < Sys.time(),select=c(user,cible,PA,resultat,timer_ok,resultat))
				
				if (nrow(tableau_chat) > 0)
					tableau_chat[tableau_chat$PA == 1,'user'] <- "Anonyme"
				
				tableau_chat <- subset(orderBy(~-timer_ok,tableau_chat),select=c(user,timer_ok,resultat))

				# tableau_indices$texte <- gsub(pattern = "\n", replacement = "<br/>", x = tableau_indices$texte)

				tableau_chat$timer_ok <- format(as.POSIXct(tableau_chat$timer_ok, origin = "1970-01-01", tz = "Europe/Paris"),format="%H:%M:%S")
				
				colnames(tableau_chat) <- c("Expéditeur","Date et heure d'envoi","Message")
			
				tableau_chat$resultat <- gsub(pattern = "\n", replacement = "<br/>", x = tableau_chat$resultat)
			
				if (input$ano_chat){
					output$info_chat <- renderText("(coûte 1 PA)")
				}else{
					output$info_chat <- renderText("")
				}
				
				if (nrow(tableau_chat)){				
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
				
			if (USER$nom_user == "admin") {
			
				verif <- orderBy(~-timer_ok,values$actions)
				verif$timer_ok <- format(as.POSIXct(verif$timer_ok, origin = "1970-01-01", tz = "Europe/Paris"),format="%H:%M:%S")
				verif$timer <- format(as.POSIXct(verif$timer, origin = "1970-01-01", tz = "Europe/Paris"),format="%H:%M:%S")
				
				output$table_admin = DT::renderDataTable(
				  verif
				)
				
				# Nombre de PA de chaque joueur
				# PA_PJ <- data.frames(donnees_joueurs$user)
				# PA_PJ$PA <- 0
				# for (i in 1:nrow(PA_PJ)){
					# PA_actu <- donnees_joueurs[USER$ligne_user,'PA']
				
				# }
				
				liste_indices <- info_indices[info_indices$variation == "Enquête parfaite",'indice']
				option_indices <- list()
				for (indice in liste_indices){
					option_indices[[info_indices[info_indices$indice==indice & info_indices$variation == "Enquête parfaite",'titre']]] <- indice
				}
				
				if (input$enquete_speciale == "Oui")
					values$enquete_speciale <- "Oui"
				if (input$enquete_speciale == "Non")
					values$enquete_speciale <- "Non"
						
				updateSelectInput(session, "choix_enquete_admin",
				  choices = option_indices,
				  # selected = tail(x, 1)
				)
				
				liste_resultats <- rep(values$valeur_alea[,'resultat'],values$valeur_alea[,'valeur'])

				tableau_proba <- cbind(table(liste_resultats))
				nb_echec <- try(tableau_proba['Enquête râtée',],silent=TRUE)
				nb_reussite <- try(tableau_proba['Enquête réussie',],silent=TRUE)
				nb_reussite_parfaite <- try(tableau_proba['Enquête parfaite',],silent=TRUE)
				if (class(nb_echec) == "try-error") nb_echec <- 0
				if (class(nb_reussite) == "try-error") nb_reussite <- 0
				if (class(nb_reussite_parfaite) == "try-error") nb_reussite_parfaite <- 0

				updateNumericInput(session, "Nb_echec",
					value = as.character(nb_echec),
				)
				updateNumericInput(session, "Nb_reussite",
					value = as.character(nb_reussite),
				)
				updateNumericInput(session, "Nb_reussite_parfaite",
					value = as.character(nb_reussite_parfaite),
				)
				
				texte_result <- paste(rownames(prepa_tableau)," - ",prepa_tableau," %",sep='')
				output$plot_proba_admin <- renderPlot({
					pie(tableau_proba, labels = texte_result, main="Probabilité de succès \nde la mission")
				})
			}
		}
	})
})
