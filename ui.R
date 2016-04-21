shinyUI(navbarPage(theme="bootstrap2.css",title=strong("ElektoR", style = "color:red;float:left"),
                   windowTitle="ElektoR A Mandátumszimulátor",
                   tabPanel("Mandátumok",
                            fluidPage(
                              h2(strong("Összes Mandátum"),align="center"),
                              column(4,
                                br(),     
                                htmlOutput("eredmenytabla")
                                ),
                              column(6,
                                strong(htmlOutput("eredmenykor")))
                                ),
                            fluidPage(
                              br(),br(),h3(strong("Billegő körzetek"),align="center"),
                              column(12,br(),
                                     htmlOutput("billego"))
                            )
                   ),
                   tabPanel("Választási szabályok",
                            fluidPage(
                              column(4,
                                helpText("Jelenlegi választási szabályok az alapértelmezettek"),
                                radioButtons("toredek", 
                                             label = strong("Töredékszavazatok a győztestől"), 
                                             choices = list("Igen" = 1, 
                                                            "Nem" = 0,
                                                            "Egyáltalán nincs töredékszavazat" = -1),
                                             selected = 1),
                                sliderInput("kuszob", label = strong("Bejutási küszöb"),
                                            min = 0, max = 25, value = 5,, post="%")
                                ),
                              column(4,
                                sliderInput("listashely", label = strong("Listás helyek száma"),
                                            min = 0, max = 500, value = 93),
                                sliderInput("bonusz", label = strong("Többségi bónusz (Listás helyek százaléka)"),
                                            min = 0, max = 100, value = 0, post="%"),
                                radioButtons("lista_elosztas", 
                                             label = strong("Listás szavazatok kiosztási módja"), 
                                             choices = list("Sainte-Laguë módszer" = "saintelague",
                                                            "Módosított Sainte-Laguë módszer" = "modositottsaintelague",
                                                            "d'Hondt módszer" = "dhondt"),
                                             selected="dhondt")
                                    )
                                   )
                            ),
                   tabPanel("Szavazatszámok módosítása",
                            fluidPage(
                              column(4,
                                     haha<-uiOutput("part_valaszto"),
                                     uiOutput("megye_valaszto"),
                                     uiOutput("oevk_valaszto"),
                                     uiOutput("szavazat_valaszto"),
                                     actionButton("modosit", label="Módosít"),
                                     actionButton("torles", label="Feltételek törlése")
                            ),
                              column(6,
                                     h4(strong("Módosított szavazatok"),align="left"),
                                     htmlOutput("modositasok"))
                            
                            )),
                   tabPanel("Kapcsolat",
                            h4("Az applikáció használata tudományos célokra forrásmegjelöléssel ingyenes, de a szerző előzetes engedélyéhez kötött."),
                            h4("Egyéb felhasználás esetén illetve ha kérdése, észrevétele lenne az applikációval kapcsolatban az alábbi címen talál meg:",br(),br(),
                              a(href="mailto:vtisza@gmail.com","vtisza@gmail.com", style="color:blue"))
                   )
))