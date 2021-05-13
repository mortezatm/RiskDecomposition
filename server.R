function(input, output, session) {
        
        values <- reactiveValues(rst = 1, run = 0)
        observeEvent(input$run, {
                values$run <- values$run + 1
                values$rst <- 0
                })
        observeEvent(input$rst, {
                values$run <- 0
                values$rst <- values$rst + 1
                })
        
        bndSumTable <- reactive({
                if (values$run) {
                        BndDTcreator(sprd())
                } else {
                        BndDTcreator(BndSprdDflt[1, ])
                        }
                })
        
        BldngsSumCalc <- reactive({
                if (values$run) {
                        return(premCalctor(bndz = sprd(), inPref = iPrf()))
                } else {
                        return(BldngsSum)
                        }
                })
        
        output$BndSumry <- DT::renderDataTable(
                DT::datatable(bndSumTable(),
                              caption = "Table 1: Parametric CAT Bond Summary", selection = "none", 
                              options = list(dom = 't', 
                                             initComplete = JS(jsHederOut),
                                             rowCallback = JS(jsBndSumry),
                                             ordering = FALSE,
                                             columnDefs = list(list(className = "dt-right", targets = 1),
                                                               list(className = "dt-center", targets = c(2, 4)))
                                             )
                              )
                )
        
        output$bndSprd = render_dt(BndSprdDflt, 'row', rownames = FALSE,
                              options = list(dom = 't',
                                             autoWidth = TRUE,
                                             initComplete = JS(jsHederIn),
                                             ordering = FALSE,
                                             columnDefs = list(list(className = "dt-center", targets = "_all"))
                                             )
                              )
        sprd <- reactiveVal(BndSprdDflt[1, ])
        observeEvent(input$run, { # input$bndSprd_cell_edit
                req(input$bndSprd_cell_edit)
                sprd(editData(BndSprdDflt, input$bndSprd_cell_edit, 'bndSprd', rownames = FALSE))
                })
        
        output$insurerApetite = render_dt(insurerApetiteDflt, 'row', rownames = FALSE,
                                   options = list(dom = 't',
                                                  autoWidth = TRUE,
                                                  initComplete = JS(jsHederIn),
                                                  ordering = FALSE,
                                                  columnDefs = list(list(className = "dt-center", targets = "_all"),
                                                                    list(width = '150px', targets = 0),
                                                                    list(width = '120px', targets = 1))
                                                  )
                                   )
        iPrf <- reactiveVal(insurerApetiteDflt[1, ])
        observeEvent(input$run, { # input$insurerApetite_cell_edit
                req(input$insurerApetite_cell_edit)
                iPrf(editData(insurerApetiteDflt, input$insurerApetite_cell_edit, 'insurerApetite', rownames = FALSE))
                })
        
        output$cityInfo <- renderUI({
                HTML(paste("The city is in Clatsop County, the north east of Oregon State in the US",
                           "The city has a total area of 3.92 sq mi",
                           "The city serves about 6'892 residents",
                           "The Necanicum river and Neawanna creek divide the city into three parts",
                           "The East side of Neawanna creek is not considered in this study",
                           "The total of 2929 residential buildings are included in this study", sep="<br/>"))
                })

        mapDflt <- mapDflt %>%
                addPolygons(data = floodArea,
                            group = "yr1000",
                            weight = 1,
                            smoothFactor = 0.5,
                            color = ~factpal5(yr1000>0),
                            opacity = ~ifelse(yr1000>0, 1, 0),
                            fillColor = ~factpal5(yr1000>0),
                            fillOpacity = ~ifelse(yr1000>0, 1, 0)) %>%
                addLegend(color = "#58CCED",
                          labels = "yr1000",
                          group = "yr1000",
                          position = "topleft",
                          opacity = 1) %>%
                addPolygons(data = floodArea,
                            group = "yr0500",
                            weight = 1,
                            smoothFactor = 0.5,
                            color = ~factpal4(yr0500>0),
                            opacity = ~ifelse(yr0500>0, 1, 0),
                            fillColor = ~factpal4(yr0500>0),
                            fillOpacity = ~ifelse(yr0500>0, 1, 0)) %>%
                addLegend(color = "#3895D3",
                          labels = "yr0500",
                          group = "yr0500",
                          position = "topleft",
                          opacity = 1) %>%
                addPolygons(data = floodArea,
                            group = "yr0200",
                            weight = 1,
                            smoothFactor = 0.5,
                            color = ~factpal3(yr0200>0),
                            opacity = ~ifelse(yr0200>0, 1, 0),
                            fillColor = ~factpal3(yr0200>0),
                            fillOpacity = ~ifelse(yr0200>0, 1, 0)) %>%
                addLegend(color = "#1261A0",
                          labels = "yr0200",
                          group = "yr0200",
                          position = "topleft",
                          opacity = 1) %>%
                addPolygons(data = floodArea,
                            group = "yr0100",
                            weight = 1,
                            smoothFactor = 0.5,
                            color = ~factpal2(yr0100>0),
                            opacity = ~ifelse(yr0100>0, 1, 0),
                            fillColor = ~factpal2(yr0100>0),
                            fillOpacity = ~ifelse(yr0100>0, 1, 0)) %>%
                addLegend(color = "#072F5F",
                          labels = "yr0100",
                          group = "yr0100",
                          position = "topleft",
                          opacity = 1) %>%
                addLayersControl(
                        overlayGroups = c("yr0100", "yr0200", "yr0500", "yr1000"),
                        options = layersControlOptions(collapsed = FALSE)) %>%
                hideGroup(c("yr0100", "yr0200", "yr0500", "yr1000"))
                
        output$map <- renderLeaflet(mapDflt)
        
        bldngClcd <- eventReactive(input$map_shape_click, {
                clcdItem <- input$map_shape_click
                if (is.null(clcdItem)) {
                        return(NULL)
                } else {
                        return(clcdItem$id)
                        }
                })
        
        output$bldngPlots <- reactivePlot(function() {
                g <- CATscen[CATscen$Building == bldngClcd(), ]
                validate(need(nrow(g) > 0, "only ORANGE buildings are included in this study"))
                
                # full losses histogram
                p1 <- gghistogram(g, x = "Damage", bins = 33, y = "..density..", fill = "grey",
                                  alpha = 0.6, xlab = "Loss ($)", ylab = "Density",
                                  title = paste("Histogram of Full Losses for ", g$Building[1]),
                                  add = "mean", add.params = list(size = 1, linetype = "dashed")) +
                        scale_y_continuous(labels = fancy_scientific) +
                        theme(plot.margin = unit(c(2, 1, 1, 1), "cm"),
                              axis.title.x = element_text(vjust = -8),
                              axis.title.y = element_text(vjust = 8),
                              plot.title = element_text(hjust = 0.5, vjust = 10))
                # QQ plot of full losses
                p2 <- ggqqplot(g, x = "Damage", main = paste("Q-Q Plot for Full Losses for ", g$Building[1])) +
                        scale_y_continuous(labels = fancy_scientific) +
                        theme(plot.margin = unit(c(2, 1, 1, 1), "cm"),
                              axis.title.x = element_text(vjust = -8),
                              axis.title.y = element_text(vjust = 8),
                              plot.title = element_text(hjust = 0.5, vjust = 10))
                
                # residual losses histogram
                p3 <- gghistogram(g, x = "resid", bins = 9, y = "..density..", fill = "grey",
                                  alpha = 0.6, xlab = "Residual loss ($)", ylab = "Density",
                                  title = paste("Histogram of Residual Losses for ", g$Building[1]),
                                  add = "mean", add.params = list(size = 1, linetype = "dashed"))
                span <- c(min(g$resid), max(g$resid)) %>%
                        abs() %>%
                        max()
                span <- span %>%
                        {(./10^(floor(log10(.))))*2} %>%
                        ceiling()/2 %>%
                        {./10^(floor(log10(span)))}
                p3 <- p3 + xlim(c(-1, 1)*span) +
                        scale_y_continuous(labels = fancy_scientific) +
                        stat_overlay_normal_density(color = "red", linetype = "dashed", size = 1.5) + 
                        theme(plot.margin = unit(c(2, 1, 1, 1), "cm"),
                              axis.title.x = element_text(vjust = -8),
                              axis.title.y = element_text(vjust = 8),
                              plot.title = element_text(hjust = 0.5, vjust = 10))
                
                # QQ plot of residual losses
                p4 <- ggqqplot(g, x = "resid", main = paste("Q-Q Plot for Residual Losses for ", g$Building[1])) +
                        scale_y_continuous(labels = fancy_scientific) +
                        theme(plot.margin = unit(c(2, 1, 1, 1), "cm"),
                              axis.title.x = element_text(vjust = -8),
                              axis.title.y = element_text(vjust = 8),
                              plot.title = element_text(hjust = 0.5, vjust = 10))
                
                # plots together
                p <- ggarrange(ggarrange(p1, p2, ncol = 2),
                               ggarrange(p3, p4, ncol = 2),
                               nrow = 2)
                
                return(print(p))
                })
        
        output$lossEntire  <- renderPlot( replayPlot(lossEntire  ) )
        output$histResid   <- renderPlot( replayPlot(lossResid   ) )
        output$QQTotal     <- renderPlot( replayPlot(lossEntireQQ) )
        output$QQResid     <- renderPlot( replayPlot(lossResidQQ ) )
        output$propVal     <- renderPlot( replayPlot(propVal     ) )
        output$propTyp     <- renderPlot( replayPlot(propTyp     ) )
        output$propTypBx   <- renderPlot( replayPlot(propTypBx   ) )
        
        output$premHist <- reactivePlot(function() {
                # monthly building premiums
                p1 <- gghistogram(BldngsSumCalc(), x = "PremMnth", bins = 10, y = "..density..",
                                  fill = "grey", ylim = c(0, 30*10^-4), alpha = 0.6,
                                  xlab = "Monthly Premium ($)", ylab = "Density",
                                  title = "Histogram of Monthly Premiums of the Insured Buildings",
                                  add = "mean", add.params = list(size = 1, linetype = "dashed"))
                bnsInfo <- ggplot_build(p1)$data[[1]]
                bnsMids <- round((bnsInfo$xmin + bnsInfo$xmax)/2, 0)
                p1 <- p1 + scale_x_continuous(breaks = bnsMids, labels = bnsMids) +
                        scale_y_continuous(labels = fancy_scientific) +
                        theme(plot.margin = unit(c(2, 1, 1, 1), "cm"),
                              axis.title.x = element_text(vjust = -8),
                              axis.title.y = element_text(vjust = 8),
                              plot.title = element_text(hjust = 0.5, vjust = 10)) # printed out 9*6
                return(print(p1))
                })
        
        output$premHistNrm <- reactivePlot(function() {
                # monthly building premiums normalized by building values
                p1 <- gghistogram(BldngsSumCalc(), x = "PremValuMnth", bins = 10, y = "..density..",
                                  fill = "grey", alpha = 0.6 , ylim = c(0, 10^3),
                                  xlab = "Monthly Premium (percentage of property value)", ylab = "Density",
                                  title = "Histogram of Monthly Premiums Divided by Property Values",
                                  add = "mean", add.params = list(size = 1, linetype = "dashed"))
                bnsInfo <- ggplot_build(p1)$data[[1]]
                bnsMids <- round((bnsInfo$xmin + bnsInfo$xmax)/2*100, 2)
                p1 <- p1 + scale_x_continuous(breaks = bnsMids/100, labels = paste0(bnsMids,"%")) +
                        scale_y_continuous(labels = fancy_scientific) +
                        theme(plot.margin = unit(c(2, 1, 1, 1), "cm"),
                              axis.title.x = element_text(vjust = -8),
                              axis.title.y = element_text(vjust = 8),
                              plot.title = element_text(hjust = 0.5, vjust = 10)) # printed out 9*6
                return(print(p1))
                })
        
        output$BldngInfo <- renderUI({
                validate(need(sum(BldngsSum$name == bldngClcd()) > 0, ""))
                HTML(paste(
                        "<p>", "</p>", "<br/>",
                        "ID:", bldngClcd(), "<br/>",
                        "Type:"      , BldngsSum$Btype[BldngsSum$name == bldngClcd()], "<br/>",
                        "Floor Area:", round(BldngsSum$area [BldngsSum$name == bldngClcd()], 0), "(m<sup>2</sup>)" , "<br/>",
                        "Value:"     , "$", formatC(BldngsSum$value[BldngsSum$name == bldngClcd()], format = "f", big.mark = ",", digits = 0), "<br/>"
                        ))
        })
        
        BldngSumry <- eventReactive({
                input$map_shape_click
                values$run
                }, {
                        return(BldngTblCreator(bldngClcd(), BldngsSum = BldngsSumCalc()))
                })
                
        output$BldngSumryI <- DT::renderDataTable({
                validate(need(sum(BldngsSum$name == bldngClcd()) > 0, "only ORANGE buildings are included in this study"))
                DT::datatable(BldngSumry()[["Iz"]], escape = FALSE,
                              colnames = c("c<sup style='vertical-align: .4em;'>I</sup><sub style='position: relative; left: -.4em; vertical-align: -.5em;'>1</sub>",
                                           "c<sup style='vertical-align: .4em;'>I</sup><sub style='position: relative; left: -.4em; vertical-align: -.5em;'>2</sub>",
                                           "c<sup style='vertical-align: .4em;'>I</sup><sub style='position: relative; left: -.4em; vertical-align: -.5em;'>3</sub>",
                                           "c<sup style='vertical-align: .4em;'>I</sup><sub style='position: relative; left: -.4em; vertical-align: -.5em;'>4</sub>",
                                           "I"),
                              caption = "Table 2: CAT Bond Interest", selection = "none", 
                              options = list(dom = 't',
                                             initComplete = JS(jsHederOut),
                                             rowCallback  = JS(jsBldngSumryI),
                                             ordering = FALSE,
                                             columnDefs = list(list(className = "dt-center", targets = "_all"))
                                             )
                              )
                })
        
        output$BldngSumryV <- DT::renderDataTable({
                validate(need(sum(BldngsSum$name == bldngClcd()) > 0, ""))
                DT::datatable(BldngSumry()[["Vz"]], escape = FALSE,
                              colnames = c("c<sup style='vertical-align: .4em;'>V</sup>",
                                           "V"),
                              caption = "Table 3: Residual Risk", selection = "none", 
                              options = list(dom = 't',
                                             initComplete = JS(jsHederOut),
                                             rowCallback  = JS(jsBldngSumryV),
                                             ordering = FALSE,
                                             columnDefs = list(list(className = "dt-center", targets = "_all"))
                                             )
                              )
                })
        
        output$BldngSumryU <- DT::renderDataTable({
                validate(need(sum(BldngsSum$name == bldngClcd()) > 0, ""))
                DT::datatable(BldngSumry()[["Uz"]], escape = FALSE,
                              colnames = c("q",
                                           "c<sup style='vertical-align: .4em;'>U</sup>",
                                           "U"),
                              caption = "Table 4: Underwriting Gain", selection = "none", 
                              options = list(dom = 't', 
                                             initComplete = JS(jsHederOut),
                                             rowCallback  = JS(jsBldngSumryAM),
                                             ordering = FALSE,
                                             columnDefs = list(list(className = "dt-center", targets = "_all"))
                                             )
                              )
                })
        
        output$BldngSumryA <- DT::renderDataTable({
                validate(need(sum(BldngsSum$name == bldngClcd()) > 0, ""))
                DT::datatable(BldngSumry()[["anulPrem"]],
                              caption = "Table 5: Annual Premium", selection = "none", 
                              options = list(dom = 't', 
                                             initComplete = JS(jsHederOut),
                                             rowCallback  = JS(jsBldngSumryAM),
                                             ordering = FALSE,
                                             columnDefs = list(list(className = "dt-center", targets = "_all"),
                                                               list(width = '40px', targets = 0),
                                                               list(width = '80px', targets = 1),
                                                               list(width = '150px', targets = 2))
                              )
                )
        })
        
        output$BldngSumryM <- DT::renderDataTable({
                validate(need(sum(BldngsSum$name == bldngClcd()) > 0, ""))
                DT::datatable(BldngSumry()[["mnthPrem"]],
                              caption = "Table 6: Monthly Premium", selection = "none", 
                              options = list(dom = 't', 
                                             initComplete = JS(jsHederOut),
                                             rowCallback  = JS(jsBldngSumryAM),
                                             ordering = FALSE,
                                             columnDefs = list(list(className = "dt-center", targets = "_all"),
                                                               list(width = '40px', targets = 0),
                                                               list(width = '80px', targets = 1),
                                                               list(width = '150px', targets = 2))
                              )
                )
        })
        
        output$floorPlan <- renderImage({
                validate(need(sum(BldngsSum$name == bldngClcd()) > 0, ""))
                planType <- BldngsSumCalc()$Btype[BldngsSumCalc()$name == bldngClcd()]
                if (planType == "F1") {
                        return(list(src = "www/F1bldng.png", filetype = "image/png", alt = "F1"))
                } else if (planType == "F2") {
                        return(list(src = "www/F2bldng.png", filetype = "image/png", alt = "F2"))
                } else if (planType == "F3") {
                        return(list(src = "www/F3bldng.png", filetype = "image/png", alt = "F3"))
                } else if (planType == "F4") {
                        return(list(src = "www/F4bldng.png", filetype = "image/png", alt = "F4"))
                }
        }, deleteFile = FALSE)
        
        output$txtz <- renderText(c(iPrf(), sprd(), length(BldngSumry()))) 
}

