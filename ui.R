library(shiny)
# if (!require("DT")) install.packages('DT')
# library(DT) 
library(DT)

options(DT.options = list(
	# language = list(
		# search = 'Filter:';
		# info = 'Résultats _START_ to _END_ sur _TOTAL_ au total',
		# paginate = list(previous = 'Précédent', `next` = 'Suivant')	
	# )
	language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json')
))

# Define UI for dataset viewer application
shinyUI(fluidPage(
  
	# Application title
	titlePanel("Lien avec votre équipe d'enquêteurs"),

	sidebarLayout(
		sidebarPanel(
			conditionalPanel(
				condition = "output.logged == 'not_ok'",
				textInput("username", "Identifiant", ""),
				textInput("password", "Mot de passe", ""),
				actionButton("boutton_log", "Go!")
			),
			conditionalPanel(
				condition = "output.logged == 'ok'",
				h3("Personnage : "),
				textOutput("user_name"),
				h3("Réserver de Points d'action : "),
				textOutput("user_PA")
			),
			conditionalPanel(
				condition = "output.logged == 'admin'",
				h3("Administration de la murder")
			),
			h4(HTML("<A HREF=\"javascript:history.go(0)\">Quitter l'interface</A>")),
			div(h4(textOutput("info")), style = "color:red")
		),
		mainPanel(
			conditionalPanel(
				condition = "output.logged == 'not_ok'",
				verbatimTextOutput("info_log")
			),
			conditionalPanel(
				condition = "output.logged == 'ok'",
				tabsetPanel(
					tabPanel("Accueil",
						br(),
						h2("Que faire ici ?"),
						h4("Enquêter"),
						p("Vous partez chacun avec un crédit de cinq points de pouvoirs. Au cours de la soirée, vous en regagneriez quatre par heure. Grâce à ces points de pouvoirs, vous pouvez nous envoyer en mission.
						Vous avez chacun des pistes de départ, des ragots sur lesquels vous souhaitez probablement en savoir plus. N'hésitez pas à nous envoyer se renseigner pour vous.
						Plus d'info dans l'onglet enquêter"),
						br(),
						h4("Lire les indices"),
						p("Dans cet onglet, vous pourrez lire le résultat de chacune de nos enquêtes."),
						br(),
						h4("Discuter"),
						p("Dans cet onglet, vous pourrez discuter avec vos collègues Archanges ou Prince Démon, anonymement ou non."),
						br(),br(),br(),
						h4("Dernier indice obtenu"),
						br(),
						DT::dataTableOutput("tableau_indices_lite"),
						br(),br(),br(),
						h4("Dernier message reçu"),
						DT::dataTableOutput("tableau_chat_lite")
					),
					tabPanel("Enquêter",
						tabsetPanel(
							tabPanel("Lancer une enquête",
								br(),
								p("Vous pouvez nous envoyer enquêter pour obtenir plus d'informations sur un sujet précis. Cela vous coûtera de base 1 point de pouvoir. Pour chaque point supplémentaire, vous augmenterez nos chances de faire une enquête parfaite. La misssion nous prendra entre 2 et 10 minutes."),
								p("Vous pouvez enquêter de deux manières :"),
								p("1- Vous utilisez les ragots que vous avez"),
								p("2- Vous nous donnez 1 ou plusieurs mots-clefs concernant des sujets que vous avez entendu durant vos propres enquêtes. Attention, si vous nous envoyer vers une piste illogique, le résultat ne le sera pas davantage."),
								br(),
								column(5,
									tabsetPanel(
										tabPanel("Recherche classique",
											selectInput("choix_enquete", "Choisir l'enquête:",
											  c("dynamique"),selectize=FALSE,size=12,width='100%'
											),
											actionButton("boutton_enquete", "Lancer la recherche")
										),
										tabPanel("Recherche avancée",
											textInput("advanced_1", "Premier mot-clef"),
											textInput("advanced_2", "Deuxième mot-clef"),
											textInput("advanced_3", "Troisième mot-clef"),
											actionButton("boutton_enquete_advanced", "Lancer la recherche")
										)
									)
								),
								column(5,
									numericInput("PA", "Nombre de PA:", 1, min = 1, max = 10),
									plotOutput("plot_proba",width="100%")
								)
							),
							tabPanel("Copier une enquête",
								br(),
								p("Selon vos ordres, nous pouvons aller récupérer une copie du rapport d'enquête obtenu par un Archange ou Prince Démon.
								
								Vous devez nous demander cela dans les 3 minutes après que votre cible ait obtenu son rapport d'enquête. Cela vous coûtera 2 points de pouvoir"),
								br(),
								selectInput("choix_user_copie", "Choisir la cible",
								c("Andréalphus","Baal","Blandine","Crocell","Dominique","Jésus","Malphas","Valefor","Yves"),selectize=FALSE,size=10),
								actionButton("boutton_enquete_copie", "Récupérer une copie de l'enquête"),
								h4(textOutput("info_copie"))
							),
							tabPanel("Intercepter une enquête",
								br(),
								p("
								Si vous remarquez, via une conversation ou une indiscrétion, qu'un Archange ou Prince Démon se prépare à enquêter sur un sujet qui ne vous arrange (bref, qu'on cherche à vous nuire...), vous pouvez nous demander d'intercepter le rapport."),
								p("Vous devez nous envoyer en interception avant qu'il ne lance son enquête. S'il fait ensuite une enquête dans les 15 minutes, nous serons en mesure de l'intercepter et de vous la transmettre."),
								p("Cela vous coûtera 2 points de pouvoir. Mais nous préférons vous prévenir, nous avons environ 1 chance sur 6 de nous faire capter par nos concurrents et de ne pas pouvoir intercepter convenablement l'indice."),
								br(),
								selectInput("choix_user_interception", "Choisir la cible",
								c("Andréalphus","Baal","Blandine","Crocell","Dominique","Jésus","Malphas","Valefor","Yves"),selectize=FALSE,size=10),
								actionButton("boutton_enquete_interception", "Intercepter l'enquête"),
								h4(textOutput("info_interception"))
							)
						)
					),
					tabPanel("Lire les indices", 
						# dataTableOutput("tableau_indices", escape=FALSE)
						DT::dataTableOutput("tableau_indices")
					),
					tabPanel("Discuter", 
						 column(4,
							 selectInput("choix_user_chat", "Choisir la cible",
								c("Andréalphus","Baal","Blandine","Crocell","Dominique","Jésus","Malphas","Valefor","Yves")
								),
								textInput("message_chat","Message :"),
							numericInput("timer_chat", "Délai d'attente (en minute) pour envoyer le message", 1, min = 0, max = 15),
							checkboxInput("ano_chat", "Message anonyme ?"),
							h4(textOutput("info_chat")),
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
					tabPanel("Vérifier",
						DT::dataTableOutput("table_admin")
					),
					tabPanel("Ajouter PA", 
						selectInput("choix_user_PA", "Choisir le personnage",
						  c("All","Andréalphus","Baal","Blandine","Crocell","Dominique","Jésus","Malphas","Valefor","Yves")
						),
						numericInput("PA_admin", "Nombre de PA à ajouter:", 1, min = -5, max = 5),
						actionButton("boutton_admin_PA", "Ajouter/Retirer les PA"),
						h4(textOutput("info_admin_PA"))
					),
					tabPanel("Ajouter Indices",
						column(5,
							selectInput("choix_user_indice", "Choisir le personnage",
							  c("Andréalphus","Baal","Blandine","Crocell","Dominique","Jésus","Malphas","Valefor","Yves")
							),
							selectInput("choix_enquete_admin", "Choisir l'enquête:",
							  c("dynamique"),selectize=FALSE,size=25,width='100%'
							)
						),
						column(5,
							selectInput("choix_variation", "Choisir le résultat:",
							  c("Enquête râtée","Enquête réussie","Enquête parfaite")
							),
							actionButton("boutton_admin_indice", "Ajouter l'indice")
						)
					),
					tabPanel("Actions dé",
						numericInput("Nb_echec", "Nombre d'échec:", 1, min = 0, max = 10),
						numericInput("Nb_reussite", "Nombre de réussite:", 4, min = 0, max = 10),
						numericInput("Nb_reussite_parfaite", "Nombre de réussite parfaite:", 1, min = 0, max = 10),
						actionButton("boutton_admin_des", "Changer les dés"),
						plotOutput("plot_proba_admin",width="100%")
					),
					tabPanel("Activer enquête spéciale",
						selectInput("enquete_speciale", "Activer les enquêtes spéciales ?",
						  c("Non","Oui")
						)
					)
				)
			)
		)
	)
))
