source("valasztas_kiertekelo.R")
library(googleVis)
library(shinyjs)
betolto2014()


shinyServer(function(input, output) {
  values <- reactiveValues()
  values$listas<-listas
  values$egyeni<-egyeni
  values$modositas <- data.frame(szervezet= character(0), megye= character(0), oevk= character(0), szavazat=character(0),stringsAsFactors=FALSE)
  eredmeny<-reactive({
    tmp<-valasztasi_eredmeny(values$egyeni,values$listas,level,input$kuszob/100,input$listashely,
                             input$toredek,elosztas=input$lista_elosztas, bonusz=input$bonusz/100)
    colnames(tmp)<-c('Szervezet','Mandátum','Arány')
    tmp<-tmp[order(-tmp$Arány),]
    return(tmp)
                    })
  output$eredmenytabla<-renderGvis({
          tabla<-gvisTable(eredmeny(),formats=list(Arány="#.#%"))
          })
  output$eredmenykor<-renderGvis({
          gvisPieChart(data=eredmeny(),numvar=eredmeny()[,2],labelvar=eredmeny()[,1],
                       options=list(is3D=F, pieHole=0.4,
                                    chartArea="{left:30,top:20,width:'90%',height:'110%'}",
                                    fontSize=12, colors="['#E68A00','#B20000','blue','darkgreen']"))
          })
  output$billego<-renderGvis({
    adat<-billegokorzet(values$egyeni,0.05)
    colnames(adat)<-c("Megye","OEVK","Győztes jelölt","Győztes szervezet","Győztes szavazat","Második jelölt","Második szervezet","Második szavazat","Különbség")
    tabla1<-gvisTable(data=adat,
                      options=list(page='enable',pageSize=5),
                      formats=list(Különbség="#.###%","Győztes szavazat"="#.##%",
                                   "Második szavazat"="#.##%"))
  })
  output$part_valaszto<-renderUI(
    selectInput("szervezet", "Szervezet:",
                choices = c('mind',unique(levels(listas$szervezet)))
                )
  )
  output$megye_valaszto<-renderUI(
    return (selectInput("megye", "Megye:",
                choices = c('mind',unique(levels(listas$megye)))
                )
          )
  )
  output$oevk_valaszto<-renderUI(
    if(is.null(input$megye)|| input$megye=="mind"){selectInput("oevk", "Választókerülete:",choices = c('mind'))}
    else
    selectInput("oevk", "Választókerülete:",choices = c('mind',unique(listas[listas$megye==input$megye,]$oevk))
    )
  )
  output$szavazat_valaszto<-renderUI(
    return (numericInput("szavazat", "Szavazatszám változtatása %-kal:",
                        value = 0, min=-100, max=200, step=1
      )
    )
  )
  observeEvent(input$torles, {
    values$egyeni<<-egyeni
    values$listas<<-listas
    values$modositas<<-data.frame(szervezet= character(0), megye= character(0), oevk= character(0), szavazat=character(0),stringsAsFactors=FALSE)
    })
  observeEvent(input$modosit, {
    values$listas<<-szavazat_modosito(values$listas,szervezet=input$szervezet,megye=input$megye, oevk=input$oevk,szavazat=(1+(input$szavazat/100)))
    values$egyeni<<-szavazat_modosito(values$egyeni,szervezet=input$szervezet,megye=input$megye, oevk=input$oevk,szavazat=(1+(input$szavazat/100)))
    values$modositas<<-as.data.frame(rbind(c(input$szervezet,input$megye,input$oevk,input$szavazat),values$modositas), stringsAsFactors=F)
    values$modositas[]<<-lapply(values$modositas,as.character)
    colnames(values$modositas)<-c("Szervezet","Megye","OEVK","Szavazat")
    })
  output$modositasok<-renderGvis({
    modosittabla<-gvisTable(values$modositas)
  })

})

#rm(list=ls(all=TRUE)) 