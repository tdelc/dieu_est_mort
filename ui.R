library(shiny)
library(DT)
library(shinythemes)

options(DT.options = list(
	language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json')
))

shinyUI(fluidPage(theme = shinytheme("paper"),
	
	conditionalPanel(
		condition = "output.logged != 'not_ok' & output.logged != 'ok' & output.logged != 'admin'",
		titlePanel("Connexion avec votre équipe d'enquêteurs en cours....")
	),
	conditionalPanel(
		condition = "output.logged == 'ok'",
		titlePanel("Lien avec votre équipe d'enquêteurs")
	),
  conditionalPanel(
    condition = "output.logged == 'admin'",
    titlePanel("Administration de la murder")
  ),


	sidebarLayout(
	  
	  conditionalPanel(
	    condition = "output.logged == 'ok' | output.logged == 'admin'",
	    sidebarPanel(width = 2,
	                 conditionalPanel(
	                   condition = "output.logged == 'ok'",
	                   h3(textOutput("currentTime")),
	                   h3(textOutput("user_name")),
	                   h4(textOutput("user_PA")),
	                   helpText("(Vous regagnerez quatre points de pouvoir par heure)"),
	                   div(h5(textOutput("info")), style = "color:red")
	                 ),
	                 h4(HTML("<A HREF=\"javascript:history.go(0)\">Quitter l'interface</A>"))
	    )
	  ),
		mainPanel(width = 10,
		          
      conditionalPanel(
        condition = "output.logged == 'admin_serveur'",
        textInput("server_name", "Nom du serveur", ""),
        actionButton("create_server", "Créer le serveur"),
        br(),
        textOutput("info_server")
      ),
			conditionalPanel(
				condition = "output.logged == 'not_ok'",
				column(width = 8,align="right",
				       imageOutput("back")
				),
				column(width = 2,
				       br(),br(),
				       div(h5(textOutput("info_log")), style = "color:red"),
				       br(),
				       textInput("username", "Identifiant", ""),
				       passwordInput("password", "Mot de passe", ""),
				       actionButton("boutton_log", "Go!")
				)
			),
			conditionalPanel(
				condition = "output.logged == 'ok'",
				tabsetPanel(id="panel_principal",
					tabPanel("Accueil",
						br(),
						column(3,
  						h2("Que faire ici ?"),
  						h4(actionLink("link_enquete","Enquêter")),
  						p("Dans cet onglet, vous pouvez nous envoyer enquêter pour vous, copier des résultats d'enquête de vos confrères Archanges ou Princes Démons, ou même intercepter leurs enquêtes."),
  						br(),h4(actionLink("link_lecture","Lire les indices")),
  						p("Dans cet onglet, vous pourrez lire le résultat de chacune de nos enquêtes."),
  						br(),h4(actionLink("link_discussion","Discuter")),
  						p("Dans cet onglet, vous pourrez discuter avec vos collègues Archanges ou Princes Démons, anonymement ou non.")
						),
						column(8,offset = 1,
  						br(),br(),h4("Dernier indice obtenu"),
  						br(),DT::dataTableOutput("tableau_indices_lite"),
  						br(),br(),h4("Dernier message reçu"),
  						DT::dataTableOutput("tableau_chat_lite")
						)
					),
					tabPanel("Enquêter",
						tabsetPanel(
							tabPanel("Lancer une enquête",
								br(),
								p("Vous pouvez nous envoyer enquêter pour obtenir plus d'informations sur un sujet précis. Cela vous coûtera de base 1 point de pouvoir. Pour chaque point supplémentaire, vous augmenterez nos chances de faire une enquête parfaite."),
								p("Vous pouvez enquêter de deux manières :"),
								p("1- Recherche Classique : Vous utilisez les ragots que vous avez."),
								p("2- Recherche Avancée : Vous nous donnez un ou plusieurs mots-clefs concernant des sujets que vous avez entendu durant la soirée. Attention, si vous nous envoyez vers une piste illogique, le résultat le sera également."),
								br(),
								column(5,
									tabsetPanel(
										tabPanel("Recherche classique",
											selectInput("choix_enquete", "Choisir l'enquête:",
											  c("dynamique"),selectize=FALSE,size=12,width='100%'
											),
											actionButton("boutton_enquete", "Lancer la recherche"),
											helpText("(L'enquête prendra entre 2 et 10 minutes)")
										),
										tabPanel("Recherche avancée",
											helpText("(n'oubliez pas les accents)"),
											textInput("advanced_1", "Premier mot-clef"),
											textInput("advanced_2", "Deuxième mot-clef"),
											textInput("advanced_3", "Troisième mot-clef"),
											actionButton("boutton_enquete_advanced", "Lancer la recherche"),
											helpText("(L'enquête prendra entre 2 et 10 minutes)")
										)
									)
								),
								column(5,
									numericInput("PA", "Nombre de points de pouvoir :", 1, min = 1, max = 10,step=1),
									plotOutput("plot_proba",width="100%")
								)
							),
							tabPanel("Copier une enquête",
								br(),
								p("Selon vos ordres, nous pouvons aller récupérer une copie du rapport d'enquête obtenu par un Archange ou Prince Démon."),
								p("Vous devez nous demander cela dans les 3 minutes après que votre cible ait obtenu son rapport d'enquête. Nous serons discrets, il n'en saura rien."),
								p("Cela vous coûtera 2 points de pouvoir."),
								br(),
								selectInput("choix_user_copie", "Choisir la cible",
								c("Andrealphus","Baal","Blandin","Crocell","Dominique","Jesus","Malphas","Valefor","Yves"),selectize=FALSE,size=10),
								actionButton("boutton_enquete_copie", "Récupérer une copie de l'enquête"),
								h4("(coûte 2 points de pouvoir)")
							),
							tabPanel("Intercepter une enquête",
								br(),
								p("Si vous remarquez, via une conversation ou une indiscrétion, qu'un Archange ou Prince Démon se prépare à enquêter sur un sujet qui ne vous arrange pas (bref, qu'on cherche à vous nuire...), vous pouvez nous demander d'intercepter le rapport."),
								p("Vous devez nous envoyer en interception avant qu'il ne lance son enquête. S'il fait ensuite une enquête dans les 15 minutes, nous serons en mesure de l'intercepter et de vous la transmettre."),
								p("Cela vous coûtera 2 points de pouvoir. Mais nous préférons vous prévenir, nous avons environ 1 chance sur 6 de nous faire capter par nos concurrents et de ne pas pouvoir intercepter convenablement l'indice."),
								br(),
								selectInput("choix_user_interception", "Choisir la cible",
								c("Andrealphus","Baal","Blandin","Crocell","Dominique","Jesus","Malphas","Valefor","Yves"),selectize=FALSE,size=10),
								actionButton("boutton_enquete_interception", "Intercepter l'enquête"),
								h4("(coûte 2 points de pouvoir)")
							)
						)
					),
					tabPanel("Lire les indices", 
						DT::dataTableOutput("tableau_indices")
					),
					tabPanel("Discuter", 
					         br(),
						 column(4,
							 selectInput("choix_user_chat", "Choisir la cible",
								c("Andrealphus","Baal","Blandin","Crocell","Dominique","Jesus","Malphas","Valefor","Yves"),selectize=FALSE,size=10),
								textAreaInput("message_chat","Message :",width='100%'),
							numericInput("timer_chat", "Délai d'attente (en minute) pour envoyer le message", 1, min = 0, max = 15),
							checkboxInput("ano_chat", "Message anonyme ?"),
							p(textOutput("info_chat")),
							actionButton("boutton_chat", "Envoyer le message")
						),
						column(8,
							DT::dataTableOutput("tableau_chat")
						)
					)
				)
			),
			conditionalPanel(
				condition = "output.logged == 'admin'",
				tabsetPanel(
					tabPanel("Vérifier actions",
						DT::dataTableOutput("table_admin")
					),
					tabPanel("Vérifier Indices",
					         DT::dataTableOutput("table_indices_admin")
					),
					tabPanel("Modifier Indices",
					         downloadButton('downloadIndices', 'Télécharger les indices actuels'),
					         fileInput('loadIndices', 'Charger de nouveaux indices',accept='.csv'),
					         br(),
					         DT::dataTableOutput("table_admin_indices")
					),
					tabPanel("Vérifier info PJ",
						DT::dataTableOutput("table_admin_PJ")
					),
					tabPanel("Ajouter PA", 
						selectInput("choix_user_PA", "Choisir le personnage",
						  c("All","Andrealphus","Baal","Blandin","Crocell","Dominique","Jesus","Malphas","Valefor","Yves")
						),
						numericInput("PA_admin", "Nombre de PA à ajouter:", 1, min = -5, max = 5),
						actionButton("boutton_admin_PA", "Ajouter/Retirer les PA")
					),
					tabPanel("Ajouter Indices",
						column(5,
							selectInput("choix_user_indice", "Choisir le personnage",
							  c("Andrealphus","Baal","Blandin","Crocell","Dominique","Jesus","Malphas","Valefor","Yves")
							),
							selectInput("choix_enquete_admin", "Choisir l'enquête:",
							  c("dynamique"),selectize=FALSE,size=25,width='100%'
							)
						),
						column(5,
							selectInput("choix_variation", "Choisir le résultat:",
							  c("dynamique")
							),
							actionButton("boutton_admin_indice", "Ajouter l'indice"),
							h3("Lettre"),
							textOutput("admin_indice_lettre"),
							h3("Enquête râtée"),
							textOutput("admin_indice_1"),
							h3("Enquête réussie"),
							textOutput("admin_indice_2"),
							h3("Enquête parfaite"),
							textOutput("admin_indice_3")
						)
					),
					tabPanel("Actions dé",
						numericInput("Nb_echec", "Nombre d'échec:", 1, min = 0, max = 10),
						numericInput("Nb_reussite", "Nombre de réussite:", 4, min = 0, max = 10),
						numericInput("Nb_reussite_parfaite", "Nombre de réussite parfaite:", 1, min = 0, max = 10),
						actionButton("boutton_admin_des", "Changer les dés"),
						plotOutput("plot_proba_admin",width="100%")
					),
					tabPanel("Actions spéciales",
						actionButton("enquete_speciale_on", "Activer les enquêtes spéciales ?"),
						actionButton("enquete_speciale_off", "Desactiver les enquêtes spéciales ?"),
						br(),
						actionButton("enquete_chat_gpt_on", "Activer les enquêtes par Chat GPT ?"),
						actionButton("enquete_chat_gpt_off", "Desactiver les enquêtes par Chat GPT ?"),
						br(),
						textOutput("admin_enquete_speciale"),
						br(),
						actionButton("boutton_admin_reset", "Remettre a réro la BDD"),
						actionButton("boutton_admin_reset_chat", "Remettre a réro le chat"),
						actionButton("boutton_admin_reset_normale", "Remettre a réro les enquêtes normales"),
						actionButton("boutton_admin_reset_speciale", "Remettre a réro les actions spéciales")
					)
				)
			)
		)
	)
))