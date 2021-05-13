vars <- c(
        "Is SuperZIP?" = "superzip",
        "Centile score" = "centile",
        "College education" = "college",
        "Median income" = "income",
        "Population" = "adultpop"
)


fluidPage(
        tags$head(
                tags$style(HTML("
                        .shiny-output-error-validation {
                                color: red; text-align: center; font-size: 18px;
                        }
                        h3 {
                                color: #E84A27; background-color: #13294b; height: 60px; width: 90%; line-height: 60px; margin-left: 5%;
                        }
                "))
        ),
        titlePanel("Seaside, Oregon, USA"),
        fluidRow(
                column(4, align="center",
                       fluidRow( leafletOutput("map", height = "900px", width = "600px") ),
                       br(),
                       hr(style = "border-top: 1px solid #000000;"),
                       fluidRow( plotOutput("premHist"   , height = 300, width = 500) )
                       ),
                column(8,
                       fluidRow( h3("Nat. Cat. Risk Decomposition", align = "center") ),
                       fluidRow(
                               sidebarPanel(
                                       width = 5,
                                       h4("City Info.", style = "color: #E84A27"),
                                       htmlOutput("cityInfo")
                                       ),
                               sidebarPanel(
                                       width = 7,
                                       fluidRow(
                                               column(width = 7, dt_output("CAT Bond Spread (%)", "bndSprd") ),
                                               column(width = 5, dt_output("Insurer Preference", "insurerApetite") )
                                               ),
                                       br(),
                                       actionButton("run", label = "RUN"),
                                       actionButton("rst", label = "RESET")
                                       )
                               ),
                       hr(style = "border-top: 1px solid #000000;"),
                       fluidRow(
                               column(5, plotOutput("propVal"  , height = 300, width = 500) ),
                               column(3, plotOutput("propTyp"  , height = 300, width = 300) ),
                               column(4, plotOutput("propTypBx", height = 300, width = 400) )
                               ),
                       hr(style = "border-top: 1px solid #000000;"),
                       fluidRow(
                               column(5,
                                      fluidRow( DT::dataTableOutput("BndSumry") ),
                                      br(),
                                      hr(style = "border-top: 1px solid #000000;"),
                                      fluidRow( plotOutput("premHistNrm", height = 300, width = 550) )
                                      ),
                               column(7,
                                      fluidRow( h3("Building Info.", align = "center") ),
                                      sidebarPanel(width = 12,
                                                   fluidRow(
                                                           column(3, htmlOutput("BldngInfo")),
                                                           column(9, DT::dataTableOutput("BldngSumryI"))
                                                           ),
                                                   br(),
                                                   fluidRow(
                                                           column(4, DT::dataTableOutput("BldngSumryV")),
                                                           column(8, DT::dataTableOutput("BldngSumryU"))
                                                           ),
                                                   br(),
                                                   fluidRow(
                                                           column(6, DT::dataTableOutput("BldngSumryA")),
                                                           column(6, DT::dataTableOutput("BldngSumryM"))
                                                           )
                                                   )
                                      )
                               )
                       )
                ),
        fluidRow(
                column(7,
                       hr(style = "border-top: 1px solid #000000;"),
                       fluidRow(
                               h3("Plots for Entire Region", align = "center")
                       ),
                       fluidRow(
                               column(7, plotOutput("lossEntire", height = 300, width = 700) ),
                               column(5, plotOutput("QQTotal"   , height = 300, width = 400) )
                               ),
                       fluidRow(
                               column(7, plotOutput("histResid", height = 300, width = 700) ),
                               column(5, plotOutput("QQResid"  , height = 300, width = 400) )
                               ),
                       hr(style = "border-top: 1px solid #000000;"),
                       fluidRow( h3("Plots for Individual Building", align = "center") ),
                       fluidRow( plotOutput("bldngPlots", height = 600, width = 1100) )
                       ),
                column(5, align="center", imageOutput("floorPlan") )
                )
        )