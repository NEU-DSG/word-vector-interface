library(shinydashboard)

library("rjson")
library(shiny)
library(magrittr)
library(tidyverse)
library(shinyjs)
library(wordVectors)
library(DT)
library(wordcloud)
library(ggrepel)



json_file <- "data/catalog.json"
json_data <- fromJSON(file=json_file)



fileList <- c()
list_clustering <- list()
list_models <- list()
list_Desc <- list()
vectors <- list()

Selected_default <- 1
Selected_compare_1 <- 1
Selected_compare_2 <- 1

ls_download_cluster <- c()



i <- 1
for(fn in json_data) {
  if(fn$public == "true")
  {
    print(fn$shortName)
    print(fn$location)
    val <- fn$shortName

    if(val == "WWO Full Corpus")
    {
      Selected_default <- val
      Selected_compare_1 <-val
    }

    if(val == "WWO Body Content")
    {
      print(i)
      Selected_compare_2 <- val
    }

    fileList <- append(fileList, val)
    list_models[[fn$shortName]] <- read.vectors(fn$location)
    list_Desc[[fn$shortName]] <- fn$description
    list_clustering [[fn$shortName]] <- kmeans( list_models[[fn$shortName]] , centers=150,iter.max = 40)

    data <- as.matrix(list_models[[fn$shortName]])
    vectors[[fn$shortName]] <-stats::predict(stats::prcomp(data))[,1:2]

    i = i + 1
  }
}



# Create a link to search WWO, optionally with a proxy URL.
linkToWWO <- function(keyword, session) {
  url <- paste0("https://wwo.wwp.northeastern.edu/WWO/search?keyword=",keyword)
  requestParams <- parseQueryString(session$clientData$url_search)
  proxy <- requestParams$proxy
  proxy <- ifelse( exists("proxy") && proxy != '', proxy, 'none' )
  # Only use the proxy value if it starts with HTTP or HTTPS protocol
  if ( grepl('^https?://', proxy) ) {
    url <- paste0(proxy,url)
  }
  paste0("<a target='_blank' href='",url,"'>",keyword,"</a>")
}

  

body <- dashboardBody(

  includeScript(path = "script.js"),

  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),

  tags$head(tags$style(HTML("

@media only screen and (max-width: 1000px) {
                            body {
                            }


                            #table-main-2 {
                            display: none !important;
                            }

                            .col-sm-6 {
                            flex: 0 0 100% !important;
                            max-width: 100% !important;
                            }

                            .compare_width {
                            width: 100% !important;
                            }

                            #wwvt-home {
                            margin-left: 16px !important;
                            align-self: center !important;
                            color: #fefefe !important;
                            font-size: 16px !important;
                            }


                            .navbar-collapse.collapse {
                            height: auto!important;
                            padding-bottom: 0;
                            overflow: visible!important;
                            display: none !important;
                            }


                            .navbar-nav {
                            display: none !important;

                            }

                            .side-open {
                            transform: none !important;
                            }

                            .side-close {
                            -webkit-transform: translate(-230px,0) !important;
                            -ms-transform: translate(-230px,0) !important;
                            -o-transform: translate(-230px,0) !important;
                            transform: translate(-230px,0) !important;
                            }

                            }

                            #downloadData {
                            margin-top: 6px !important;
                            height: fit-content !important;
                            color: #444 !important;
                            }



                            .dataTables_wrapper {
                            overflow-y : auto;
                            }

                            .visualization {
                            width : 100%;
                            }

                            #Download_reset_button {
                            display: flex !important;
                            margin: 0 !important;
                            padding: 0 !important;
                            }

                            .datatables {
                            min-height : 20px !important;
                            }


                            .nav-item {
                            margin: 0px 2px;
                            }

                            .dropdown-menu {
                            font-size:16px !important;
                            }

                            #sidebarItemExpanded {
                            margin-top:20px;
                            }

                            .btn-group-vertical>.btn-group:after, .btn-group-vertical>.btn-group:before, 
                            .btn-toolbar:after, .btn-toolbar:before, .clearfix:after, .clearfix:before, 
                            .container-fluid:after, .container-fluid:before, .container:after, .container:before, 
                            .dl-horizontal dd:after, .dl-horizontal dd:before, .form-horizontal .form-group:after, 
                            .form-horizontal .form-group:before, .modal-footer:after, .modal-footer:before, 
                            .modal-header:after, .modal-header:before, .nav:after, .nav:before, 
                            .navbar-collapse:after, .navbar-collapse:before, .navbar-header:after, .navbar-header:before, 
                            .navbar:after, .navbar:before, .pager:after, .pager:before, 
                            .panel-body:after, .panel-body:before, .row:after, .row:before {
                            display: table;
                            content: unset;
                            }

                            #word_cloud.shiny-output-error-validation {
                              visibility: visible;
                              color: inherit;
                              font-size: 1.25em;
                              font-style: italic;
                            }

                            #word_cloud > img {
                            display: block;
                            margin-left: auto;
                            margin-right: auto;
                            }

                            .btn {
                            font-size:14px !important;
                            }
                            .form-control {
                            font-size:14px !important;
                            }
                            .compare_width {
                            width: 60%
                            }

                            body {
                            font-family: 'Source Sans Pro', 'Helvetica Neue', Helvetica, Arial, sans-serif !important;
                            font-size: 14px !important;
                            }
                            .main-header .navbar {
                            position: unset; margin : 0; font-size: 18px !important; height:68px !important;
                            };
                            .box{-webkit-box-shadow: none; -moz-box-shadow: none; box-shadow: none;}
                            .content-wrapper {overflow-y: scroll;}
                            .model_header {height : 160px}

                            #wwvt-home {
                            margin-left: 16px !important;
                            align-self: center !important;
                            color: #fefefe !important;
                            font-size: 21.6px !important;
                            }

                            #wwvt-home:hover {
                            color: #ccc !important;
                            text-decoration: none !important;
                            }

                            .box {
                              overflow: auto;
                            }

                            .home_desc {
                              font-size: 14px !important;
                              border-bottom-style: solid !important;
                              margin-bottom: 47px !important;
                              border-radius: 2px !important;
                              padding: 20px !important;
                              border-bottom-width: medium !important;
                            }

                            .welcome_desc {
                              font-size: 1.2em !important;
                            }

                            .dataTables_filter {
                                display: none !important;
                            }

                            .nav-over-flow{
                            background-color: #343a40!important
                            }"))),

  fluidRow(
    tabBox(
      # The id lets us use input$tabset1 on the server to find the current tab
      id = "tabset1", height = "250px", width = 12,
      tabPanel("Home", value=1,
               div(
                 fluidRow (
                   box(
                     p( class="home_desc welcome_desc", 
                       "Welcome to the Women Writers Vector Toolkit (WWVT) discovery interface! This interface will allow you to query terms in word2vec models that were trained on texts from the Women Writers Online, Victorian Women Writers Project, and Early English Books Online–Text Creation Partnership collections."),
                     tags$h1(textOutput("model_name_basic")),
                     div(
                       class = "model_desc", 
                       p(uiOutput("model_desc_basic"))
                     ),
                     width=12
                   )
                 )
               ,
                 fluidRow(
                   box(
                     box(solidHeader = TRUE, textInput("basic_word1", "Query term:", width = "500px"), width=12),
                     box(
                       # solidHeader = TRUE,
                       DT::dataTableOutput("basic_table"),
                       id = "table-main-1",
                       width=12
                    ),
                    width=6
                  ),
                  box(
                    p("To get started, type a word in the “Query term” box. The results that appear are the words that are closest to the term that you queried in vector space—that is, words that appear in similar contexts in the corpus used to train your model."),
                    p("On the left-hand sidebar, you can select different models to query; you can also increase the number of words in your results set. More ways to explore these models can be accessed under the “Compare,” “Clusters,” “Operations,” and “Visualization” tabs above."),
                    p("If you click on any individual term, a new page will take you to the Women Writers Online interface (subscription required; see ", 
                      tags$a(href="https://wwp.northeastern.edu/wwo/license/", target="_blank", "this page"), 
                      " for information on subscribing and setting up a free trial) to search for your term in the WWO collection.")),
                  width = 6
                  )
                )
        ),
      tabPanel("Compare", value=2,
               id = "compareTab-Id",

               fluidRow(
                 box( solidHeader = TRUE, textInput("basic_word_c", "Query term:", width = "500px"), width=12)
               ),


               fluidRow(
                 box(

                   box(
                     solidHeader = TRUE,
                     class = "model_header",
                     tags$h1(textOutput("model_name_compare_1")),
                     div(class = "model_desc", p(uiOutput("model_desc_compare_1"))),

                     # div(class = "model_desc", p(textOutput("model_desc_compare_1"),
                     #                               "The text has been regularized",
                     #                               a("[read more]", href=paste("https://wwp.northeastern.edu/lab/wwvt/methodology/index.html", sep=""), target="_blank")
                     #                             )
                         # ),
                     width = 12
                   ),
                   box(
                     DT::dataTableOutput("basic_table_c1"),
                     width = 12
                   )
                 ),
                 box(
                   box(
                     solidHeader = TRUE,
                     class = "model_header",
                     tags$h1(textOutput("model_name_compare_2")),
                     div(class = "model_desc", p(uiOutput("model_desc_compare_2"))),

                     # div(class = "model_desc", p(textOutput("model_desc_compare_2"),
                     #                             "The text has been regularized",
                     #                             a("[read more]", href=paste("https://wwp.northeastern.edu/lab/wwvt/methodology/index.html", sep=""), target="_blank")
                     #                             )
                     #     ),
                     width = 12
                   ),
                   box(
                     DT::dataTableOutput("basic_table_c2"),
                     width = 12
                  )
                 )
               )
          ),

      tabPanel("Clusters", value=3,
               fluidRow(
                 box(

                   div(class="home_desc", 
                       p("Clusters are generated based on neighboring words in vector space—words that are used in similar contexts will be clustered together. Each column represents a different cluster, randomly selected from 150 total clusters; the words in the list are those closest to the center of the cluster."),
                       p("Use the dropdown on the left to select which model you want to view. Click the “Download” button to download the set of clusters you are viewing. You can also hit the “reset clusters” button to see a new set of clusters and use the slider to see more terms from each cluster. (Note that adjusting the number of terms per cluster will also reset the clusters.)"),
                       p("If you click on any individual term, a new page will take you to the Women Writers Online interface (subscription required; see ", 
                         tags$a(href="https://wwp.northeastern.edu/wwo/license/", target="_blank", "this page"), 
                         " for information on subscribing and setting up a free trial) to search for your term in the WWO collection.")),

                   tags$h1(textOutput("model_name_cluster")),
                   div(class = "model_desc", p(uiOutput("model_desc_cluster"))),
                   br(),
                   actionButton("clustering_reset_input_fullcluster1", "Reset clusters"),

                   # div(class = "model_desc", p(textOutput("model_desc_cluster"),
                   #                             "The text has been regularized",
                   #                             a("[read more]", href=paste("https://wwp.northeastern.edu/lab/wwvt/methodology/index.html", sep=""), target="_blank")
                   #                             )
                   #     ),
                   width=12
                 ),
                 box(
                  # solidHeader = TRUE,
                  DTOutput('clusters_full'), width = 12)
               )
      ),
      tabPanel("Operations", value=4,
               fluidRow(
                 box(

                   div(class="home_desc",
                     p("Using the sidebar on the left, you can select from several different operations and choose which model you would like to query.")
                  ),
                  tags$h1(textOutput("model_name_operation")),
                   div(class = "model_desc", p(uiOutput("model_desc_operation"))),
                   width=12
                 )
              ),
              fluidRow(
                box( width = 7,
                 conditionalPanel(condition="input.operator_selector=='Addition'",
                                  #class = "compare_width",
                                    box(
                                      solidHeader = TRUE,
                                      shinyjs::useShinyjs(),
                                      id = "addition_panel",
                                      column(4,
                                             textInput("addition_word1", "Word 1")),
                                      column(
                                        2, br(), tags$label(class = "col-sm-4 control-label", icon("plus"))
                                      ),
                                      column(4,
                                             textInput("addition_word2", "Word 2")
                                      ),
                                      width = 12
                                    ),
                                    box(
                                      DT::dataTableOutput("addition_table"),
                                      width = 12
                                    ),
                    width = 12
                 ),

                 conditionalPanel(condition="input.operator_selector=='Subtraction'",
                                  #class = "compare_width",
                                  box(
                                      solidHeader = TRUE,
                                      shinyjs::useShinyjs(),
                                      id = "subtraction_panel",
                                      column(4,
                                             # Sidebar with a inputs
                                             textInput("subtraction_word1", "Word 1")),
                                      column(
                                        2,br(), tags$label(class = "col-sm-4 control-label", icon("minus"))
                                      ),
                                      column(4,
                                             textInput("subtraction_word2", "Word 2")
                                      ),
                                      width = 12
                                  ),
                                  box(
                                    DT::dataTableOutput("subtraction_table"),
                                    width = 12
                                  ),
                    width = 12
                 ),
                 conditionalPanel(condition="input.operator_selector=='Advanced'",
                                  #class = "compare_width",
                                  box(
                                       solidHeader = TRUE,
                                       shinyjs::useShinyjs(),
                                       id = "advanced_panel",
                                       column(2,
                                              # Sidebar with a inputs
                                              textInput("advanced_word1", "Word 1")),
                                       column(2,
                                              class = "mathCol",
                                              selectInput("advanced_math", "Math",
                                                          choices = list("+" = "+", "-" = "-", "*" = "*", "/" = "/"),
                                                          selected = 1)),
                                       column(2,
                                              textInput("advanced_word2", "Word 2")),
                                       column(2,
                                              class = "mathCol",
                                              selectInput("advanced_math2", "Math",
                                                          choices = list("+" = "+", "-" = "-", "*" = "*", "/" = "/"),
                                                          selected = 1)),
                                       column(2,
                                              textInput("advanced_word3", "Word 3")
                                       ),
                                       width = 12
                                    ),
                                    box(
                                      DT::dataTableOutput("advanced_table"),
                                      width = 12
                                    ),
                      width = 12
                 ),

                 conditionalPanel(condition="input.operator_selector=='Analogies'",
                                  #class = "compare_width",
                                  box(
                                    solidHeader = TRUE,
                                    shinyjs::useShinyjs(),
                                    id = "analogies_panel",
                                    column(2,
                                           # Sidebar with a inputs
                                           textInput("analogies_word1", "Word 1")),
                                    column(
                                      1, br(), tags$label(class = "col-sm-4 control-label", icon("minus"))
                                    ),
                                    column(2,
                                           textInput("analogies_word2", "Word 2")),
                                    column(
                                      1, br(), tags$label(class = "col-sm-4 control-label", icon("plus"))
                                    ),
                                    column(2,
                                           textInput("analogies_word3", "Word 3")
                                    ),
                                    width = 12
                                  ),
                                  box(
                                    DT::dataTableOutput("analogies_table"),
                                    width = 12
                                  )
                      )
                 ),
                 box(
                   p("Addition allows you to add the contexts associated with two terms to each other, while subtraction allows you to subtract the contexts associated with one word from another. To see how these work, try “orange” + “red” and “orange” - “red” and compare the results."),
                   p("The analogies operation allows you to subtract the contexts associated with one term from another, and then add the contexts associated with a third term. For example, you might subtract “man” from “woman” to get a vector associated with the contexts of “woman” as distinct from “man”; then, adding the vector for “king” will bring in its contexts to give you words associated with the distinction between woman and man AND with royalty; in many models, this will be “queen.” Or, put more simply: woman - man + king = queen; woman is to man as queen is to king."),
                   p("The advanced option allows you to create a query of your own using multiple operations."),
                   p("If you click on any individual term, a new page will take you to the Women Writers Online interface (subscription required; see ", 
                     tags$a(href="https://wwp.northeastern.edu/wwo/license/", target="_blank", "this page"), 
                     " for information on subscribing and setting up a free trial) to search for your term in the WWO collection."),
                   width = 5
                 )

               )
      ),
      tabPanel("Visualization", value=5,
               fluidRow(

                 box(
                   tags$h1(textOutput("model_name_visualisation")),
                   div(class = "model_desc", p(uiOutput("model_desc_visualisation"))),
                   width=12
                 ),

                 conditionalPanel(condition="input.visualisation_selector=='wc'",
                    class = "visualization",
                    shinyjs::useShinyjs(),
                    tags$head(tags$style("#word_cloud{height:calc(100vh - 200px) !important;}")),
                    box( solidHeader = TRUE, 
                         textInput("word_cloud_word", "Query term:", width = "500px"), 
                         width=12),
                    box(
                      solidHeader = FALSE,
                      box(
                        solidHeader = TRUE,
                        plotOutput("word_cloud", height="600px"),
                        width = 8
                      ),
                      box(
                        #solidHeader = TRUE,
                        div(class = "model_desc", 
                            p("The visualizations tab allows you to create a
                              word cloud for the query term you would like to
                              analyze. The word cloud will produce a collage
                              of the most similar words to your query term
                              using the WWO general corpus model. You can
                              adjust the visualization based on the number
                              of words you would like to see appear
                              (top slider bar on the left of this page).
                              These terms are based on their percentage of
                              similarity to the query term. The similarity
                              percentage is also represented in the visualization
                              by the color of each word. See below for the color
                              key. The second slider down from the similarity
                              bar will allow you to adjust the number of words you
                              would like in your word cloud, and the bottom-most
                              slider controls the size of the plot image."),
                                    div("Similarity Color Key"),
                                    div("Similarity % -- Color"),
                                    div("91 – 100 -- gray"),
                                    div("81 – 90 -- brown"),
                                    div("71 – 80 -- orange"),
                                    div("51 – 70 -- green"),
                                    div("00 – 50 -- pink")
                        ),
                        width = 4
                      ),
                      width = 12
                    )
                 ),


                 conditionalPanel(condition="input.visualisation_selector=='scatter'",
                      class = "visualization",
                      shinyjs::useShinyjs(),
                      box(
                         plotOutput("scatter_plot", height = "600px"),
                         width = 8
                      )
                 ),
                 conditionalPanel(condition="input.visualisation_selector=='scatter_closest'",
                      class = "visualization",
                      shinyjs::useShinyjs(),
                      box( solidHeader = TRUE, textInput("scatter_plot_term", "Query term:", width = "500px"), width=12),
                      box(
                        plotOutput("scatter_plot_closest",height = "600px"),
                        width = 8
                      )
                 )
               )
      )
    )
  )
)

shinyApp(
  ui = dashboardPage(
      tags$header(
        class = "main-header", checked = NA,

        tags$link(rel = "stylesheet", type = "text/css", href = "https://stackpath.bootstrapcdn.com/bootstrap/4.1.2/css/bootstrap.min.css"),
        # tags$script(src = "https://code.jquery.com/jquery-3.3.1.slim.min.js"),
        # tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.3/umd/popper.min.js"),
        # tags$script(src = "https://stackpath.bootstrapcdn.com/bootstrap/4.1.2/js/bootstrap.min.js"),
        htmlTemplate("template.html", name = "header-component")

        # tags$link(rel = "stylesheet", type = "text/css", href = "style/main.css"),
        # tags$nav(
        #   class = "navbar navbar-expand-lg navbar-dark bg-dark fixed-top",
        #   tags$div(
        #     class = "container",
        #     tags$div(
        #       class = "d-inline-flex",
        #
        #       tags$img(class="d-inline-block align-top", src='assets/logo.png', height='52', width='61'),
        #       tags$a(id="wwvt-home", href="www.rstudio.com", "Women Writers Vector Toolkit")
        #
        #     ),
        #     tags$div(
        #
        #     )
        #   )
        # )
      )
    ,

    # dashboardHeader(),
    dashboardSidebar(

      conditionalPanel(condition="input.tabset1==1",
                       selectInput("modelSelect", "Model",
                                   choices = fileList,
                                   selected = Selected_default),
                       br(),
                       sliderInput("max_words_home",
                                   "Number of Words:",
                                   min = 1,  max = 150,  value = 10),
                       br(),
                       actionButton("clustering_reset_input", "Reset clusters")

      ),

      conditionalPanel(condition="input.tabset1==2",
                       selectInput("modelSelectc1", "Model 1",
                                   choices = fileList,
                                   selected = Selected_compare_1),
                       selectInput("modelSelectc2", "Model 2",
                                   choices = fileList,
                                   selected = Selected_compare_2),
                       sliderInput("max_words",
                                   "Number of Words:",
                                   min = 1,  max = 150,  value = 10)

      ),

      conditionalPanel(condition="input.tabset1==3",
                       selectInput("modelSelect_clusters", "Model",
                                   choices = fileList,
                                   selected = Selected_default),
                       br(),
                       column(
                         id = "Download_reset_button",
                         width = 12,
                         actionButton("clustering_reset_input_fullcluster", "Reset clusters"),
                         downloadButton("downloadData", "Download")
                       ),
                       br(),
                       br(),
                       sliderInput("max_words_cluster",
                                   "Number of Words:",
                                   min = 1,  max = 150,  value = 10)

      ),

      conditionalPanel(condition="input.tabset1==4",
                       selectInput("modelSelect_analogies_tabs", "Model",
                                   choices = fileList,
                                   selected = Selected_default),

                       selectInput("operator_selector", "Select operator",
                                   choices = c("Addition", "Subtraction", "Analogies", "Advanced"),
                                   selected = 1)
      ),

      conditionalPanel(condition="input.tabset1==5",
                       selectInput("modelSelect_Visualisation_tabs", "Model",
                                   choices = fileList,
                                   selected = Selected_default),

                       selectInput("visualisation_selector","Select visualisation",
                                   choices = list(
                                     "Word Cloud" = "wc",
                                     "Query Term Scatterplot" = "scatter_closest",
                                     "Cluster Scatterplot" = "scatter"),
                                   selected = 1),

                       conditionalPanel(condition="input.visualisation_selector=='wc'",
                              sliderInput("freq",
                                          "Similarity",
                                          step = 5,
                                          ticks = c(1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100),
                                          min = 0,  max = 100, value = 15),
                              sliderInput("max",
                                          "Maximum Number of Words:",
                                          min = 0,  max = 150,  value = 100),
                              sliderInput("scale",
                                          "Size of plot:",
                                          min = 0,  max = 5,  value = 3)
                       ),
                       conditionalPanel(condition="input.visualisation_selector=='scatter'",
                            selectInput("scatter_cluster", "Cluster",
                                        choices = list("Cluster 1" = "V1",
                                                       "Cluster 2" = "V2",
                                                       "Cluster 3" = "V3",
                                                       "Cluster 4" = "V4",
                                                       "Cluster 5" = "V5",
                                                       "Cluster 6" = "V6",
                                                       "Cluster 7" = "V7",
                                                       "Cluster 8" = "V8",
                                                       "Cluster 9" = "V9",
                                                       "Cluster 10" = "V10" ),
                                        selected = 1),
                            sliderInput("scatter_number",
                                        "Number of Words:",
                                        min = 5,  max = 30,  value = 10),
                            actionButton("clustering_reset_input_visualisation", "Reset clusters")

                       ),
                       conditionalPanel(condition="input.visualisation_selector=='scatter_closest'",
                                        selectInput("scatter_plot_closest_choice", "Cluster",
                                                    choices = list("Top 10",
                                                                   "Top 20",
                                                                   "Top 40",
                                                                   "Top 60",
                                                                   "Top 80",
                                                                   "Top 150"),
                                                    selected = 1)

                       )
      )

    ),
    body
  ),
  
  server = function(input, output, session) {
    # The currently selected tab from the first box
    output$tabset1Selected <- renderText({
      input$tabset1
    })


    set.seed(122)
    histdata <- rnorm(500)


    output$plot1 <- renderPlot({
      plot(mtcars$wt, mtcars$mpg)
    })

    outputOptions(output, "plot1", suspendWhenHidden = FALSE)



    output$downloadData <- downloadHandler(
      filename = function() {
        paste(input$modelSelect_clusters[[1]], ".csv", sep = "")
      },

      content = function(file) {
        data <- sapply(ls_download_cluster,function(n) {
          paste0(names(list_clustering[[input$modelSelect_clusters[[1]]]]$cluster[list_clustering[[input$modelSelect_clusters[[1]]]]$cluster==n][1:150]))
        }) %>% as_data_frame()

        write.csv(data, file, row.names = FALSE)
      })


    observeEvent(input$modelSelect, {
      output$model_name_basic <- renderText(input$modelSelect[[1]])
      # output$model_desc_basic <- renderText({list_Desc[[input$modelSelect[[1]]]]})


      url <- a("[read more]", href="https://wwp.northeastern.edu/lab/wwvt/methodology/")
      output$model_desc_basic <- renderUI({
        tagList(paste(list_Desc[[input$modelSelect[[1]]]], "The text has been regularized."), url)
      })

    })


    observeEvent(input$modelSelectc1, {
      output$model_name_compare_1 <- renderText(input$modelSelectc1[[1]])
      # output$model_desc_compare_1 <- renderText({list_Desc[[input$modelSelectc1[[1]]]]})

      url <- a("[read more]", href="https://wwp.northeastern.edu/lab/wwvt/methodology/")
      output$model_desc_compare_1 <- renderUI({
        tagList(paste(list_Desc[[input$modelSelectc1[[1]]]], "The text has been regularized."), url)
      })


    })

    observeEvent(input$modelSelectc2, {
      output$model_name_compare_2 <- renderText(input$modelSelectc2[[1]])
      # output$model_desc_compare_2 <- renderText({list_Desc[[input$modelSelectc2[[1]]]]})


      url <- a("[read more]", href="https://wwp.northeastern.edu/lab/wwvt/methodology/")
      output$model_desc_compare_2 <- renderUI({
        tagList(paste(list_Desc[[input$modelSelectc2[[1]]]], "The text has been regularized."), url)
      })


    })


    observeEvent(input$modelSelect_clusters, {
      output$model_name_cluster <- renderText(input$modelSelect_clusters[[1]])
      # output$model_desc_cluster <- renderText({list_Desc[[input$modelSelect_clusters[[1]]]]})

      url <- a("[read more]", href="https://wwp.northeastern.edu/lab/wwvt/methodology/")
      output$model_desc_cluster <- renderUI({
        tagList(paste(list_Desc[[input$modelSelect_clusters[[1]]]], "The text has been regularized."), url)
      })



    })


    observeEvent(input$modelSelect_analogies_tabs, {
      output$model_name_operation <- renderText(input$modelSelect_analogies_tabs[[1]])
      # output$model_desc_operation <- renderText({list_Desc[[input$modelSelect_analogies_tabs[[1]]]]})

      url <- a("[read more]", href="https://wwp.northeastern.edu/lab/wwvt/methodology/")
      output$model_desc_operation <- renderUI({
        tagList(paste(list_Desc[[input$modelSelect_analogies_tabs[[1]]]], "The text has been regularized."), url)
      })

    })


    observeEvent(input$modelSelect_Visualisation_tabs, {
      output$model_name_visualisation <- renderText(input$modelSelect_Visualisation_tabs[[1]])
      # output$model_desc_visualisation <- renderText({paste(list_Desc[[input$modelSelect_Visualisation_tabs[[1]]]], "The text has been regularized")})

      url <- a("[read more]", href="https://wwp.northeastern.edu/lab/wwvt/methodology/")
      output$model_desc_visualisation <- renderUI({
        tagList(paste(list_Desc[[input$modelSelect_Visualisation_tabs[[1]]]], "The text has been regularized."), url)
      })

    })


    output$word_cloud <- renderPlot({
        validate(
          need(tolower(input$word_cloud_word) != "", 
               "To generate a word cloud, enter a query term in the text field above."))
        data <- list_models[[input$modelSelect_Visualisation_tabs[[1]]]] %>% closest_to(tolower(input$word_cloud_word), 150)
        colnames(data) <- c("words", "sims")
        data <- mutate(data, sims = as.integer(sims * 100))

        set.seed(1234)
        wordcloud(words = data$words, freq = data$sims,
                  min.freq = input$freq, max.words=input$max,
                  random.order=FALSE, random.color = FALSE, rot.per = 0.30, ordered.colors = FALSE,
                  colors = brewer.pal(8,"Dark2"), scale= c(input$scale,0.5),
                  use.r.layout = TRUE)
    })

    # rv <- reactiveValues()
    # rv$setupComplete <- FALSE


    dataset <- reactive({

      times <- input$clustering_reset_input_visualisation

      df2 <- sapply(sample(1:150,10),function(n) {
        paste0(names(list_clustering[[input$modelSelect_Visualisation_tabs[[1]]]]$cluster[list_clustering[[input$modelSelect_Visualisation_tabs[[1]]]]$cluster==n][1:150]))
      }) %>% as_data_frame()

      df2
      # rv$setupComplete <- TRUE

    })

    datascatter <- reactive({

      df2 <- dataset()
      # print(df2)

      x <- c()
      y <- c()
      names <- c()
      cluster <- c()

      vector <- vectors[[input$modelSelect_Visualisation_tabs[[1]]]]
      for (column in colnames(df2))
      {
        for (word in head(df2,input$scatter_number)[column][[1]]){
          x <- append(x, vector[word, 'PC1'])
          y <- append(y, vector[word, 'PC2'])
          names <- append(names,word)
          cluster <- append(cluster,column)
        }
      }

      df_new <- data.frame(x = x, y = y, names = names, cluster = as.factor(cluster), stringsAsFactors = FALSE)
      df_new

    })

    # output$setupComplete <- reactive({
    #   return(rv$setupComplete)
    # })

    # outputOptions(output, 'setupComplete', suspendWhenHidden=FALSE)


    output$scatter_plot <- renderPlot({
      ggplot(datascatter(), aes(x=x, y=y, colour=cluster), height="600px", width="800px") +
        geom_point() +
        geom_text_repel(aes(label = ifelse(cluster == input$scatter_cluster, as.character(names),'')), hjust=0.5,vjust=-0.5)

    })


    outputOptions(output, "scatter_plot", suspendWhenHidden = FALSE)


    dataset_closet <- reactive({

      data <- as.matrix(list_models[['WWO Full Corpus']])
      vectors <-stats::predict(stats::prcomp(data))[,1:2]

      x <- c()
      y <- c()
      names <- c()
      cluster <-c()

      closeword <- list_models[['WWO Full Corpus']] %>% closest_to(tolower(input$scatter_plot_term), 150)

      i = 0
      for(word in closeword[[1]])
      {
        x <- append(x, vectors[word, 'PC1'])
        y <- append(y, vectors[word, 'PC2'])
        if (i <= 10 ) cluster <- append(cluster, "top 10")
        if (i > 10 & i <= 20 ) cluster <- append(cluster, "top 20")
        if (i > 20 & i <= 40 ) cluster <- append(cluster, "top 40")
        if (i > 40 & i <= 60 ) cluster <- append(cluster, "top 60")
        if (i > 60 & i <= 80 ) cluster <- append(cluster, "top 80")
        if (i > 80 & i <= 100 ) cluster <- append(cluster, "top 100")
        if (i > 100  ) cluster <- append(cluster, "top 150")
        i <- i + 1
        names <- append(names,word)
      }
      df_new <- data.frame(x = x, y = y, names = names, cluster = as.factor(cluster), stringsAsFactors = FALSE)
      df_new

    })

    output$scatter_plot_closest <- renderPlot({
      ggplot(dataset_closet(), aes(x=x, y=y, colour=cluster)) +
        geom_point() +
        geom_text_repel(aes(label=ifelse(cluster == tolower(input$scatter_plot_closest_choice) ,as.character(names),'')), hjust=0.5,vjust=-0.5)
    })

    outputOptions(output, "scatter_plot_closest", suspendWhenHidden = FALSE)


    output$addition_table <- DT::renderDataTable(DT::datatable({
      validate(need(input$addition_word1 != "" && input$addition_word2 != "", "Enter query term into word 1 and word 2."))
      data <- list_models[[input$modelSelect_analogies_tabs[[1]]]] %>% closest_to(list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==tolower(input$addition_word1),] +
                                                                                  list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==tolower(input$addition_word2),], 150) %>% mutate("Link" <- linkToWWO(keyword=.$word, session=session)) %>% .[c(3,2)]

    }, escape = FALSE, colnames=c("Word", "Similarity to word(s)"), options = list(lengthMenu = c(10, 20, 100, 150), pageLength = 10, searching = TRUE)))

    # Subtraction tab
    output$subtraction_table <- DT::renderDataTable(DT::datatable({
      validate(need(input$subtraction_word1 != "" && input$subtraction_word2 != "", "Enter query term into word 1 and word 2."))
      data <- list_models[[input$modelSelect_analogies_tabs[[1]]]] %>% closest_to(list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==tolower(input$subtraction_word1),] -
                                                                                  list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==tolower(input$subtraction_word2),], 150) %>% mutate("Link" <- linkToWWO(keyword=.$word, session=session)) %>% .[c(3,2)]

    }, escape = FALSE, colnames=c("Word", "Similarity to word(s)"), options = list(lengthMenu = c(10, 20, 100, 150), pageLength = 10, searching = TRUE)))


    output$analogies_table <- DT::renderDataTable(DT::datatable({
      validate(need(input$analogies_word1 != "" && input$analogies_word2 != "" && input$analogies_word3 != "", "Enter query term into Word 1, Word 2, and Word 3."))
      data <- list_models[[input$modelSelect_analogies_tabs[[1]]]] %>% closest_to(list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==tolower(input$analogies_word1),] - list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==tolower(input$analogies_word2),] + list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==tolower(input$analogies_word3),], input$all_count) %>% mutate("Link" <- linkToWWO(keyword=.$word, session=session)) %>% .[c(3,2)]

    }, escape = FALSE, colnames=c("Word", "Similarity to word(s)"), options = list(lengthMenu = c(10, 20, 100, 150), pageLength = 10, searching = TRUE)))


    output$advanced_table <- DT::renderDataTable(DT::datatable({
      validate(need(input$advanced_word1 != "", "Enter query term into Word 1."))
      data <- list_models[[input$modelSelect_analogies_tabs[[1]]]] %>% closest_to(tolower(input$advanced_word1), 150)
      if (input$advanced_word2 != "" && input$advanced_word3 == "") {
        if (input$advanced_math == "+") {
          data <- list_models[[input$modelSelect_analogies_tabs[[1]]]] %>% closest_to(list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==tolower(input$advanced_word1),] + list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==tolower(input$advanced_word2),], 150) %>% mutate("Link" <- linkToWWO(keyword=.$word, session=session)) %>% .[c(3,2)]
        }
        if (input$advanced_math == "-") {
          data <- list_models[[input$modelSelect_analogies_tabs[[1]]]] %>% closest_to(list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==tolower(input$advanced_word1),] - list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==tolower(input$advanced_word2),], 150) %>% mutate("Link" <- linkToWWO(keyword=.$word, session=session)) %>% .[c(3,2)]
        }
        if (input$advanced_math == "*") {
          data <- list_models[[input$modelSelect_analogies_tabs[[1]]]] %>% closest_to(list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==tolower(input$advanced_word1),] * list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==tolower(input$advanced_word2),], 150) %>% mutate("Link" <- linkToWWO(keyword=.$word, session=session)) %>% .[c(3,2)]
        }
        if (input$advanced_math == "/") {
          data <- list_models[[input$modelSelect_analogies_tabs[[1]]]] %>% closest_to(list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==tolower(input$advanced_word1),] - list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==(input$advanced_word2),], 150) %>% mutate("Link" <- linkToWWO(keyword=.$word, session=session)) %>% .[c(3,2)]
        }
      }

      if (input$advanced_word2 != "" && input$advanced_word3 != "") {

        if (input$advanced_math == "+" && input$advanced_math2 == "+") {
          data <- list_models[[input$modelSelect_analogies_tabs[[1]]]] %>% closest_to(list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==tolower(input$advanced_word1),] + list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==tolower(input$advanced_word2),] + list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==tolower(input$advanced_word3),], 150) %>% mutate("Link" <- linkToWWO(keyword=.$word, session=session)) %>% .[c(3,2)]
        }
        if (input$advanced_math == "+" && input$advanced_math2 == "-") {
          data <- list_models[[input$modelSelect_analogies_tabs[[1]]]] %>% closest_to(list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==tolower(input$advanced_word1),] + list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==tolower(input$advanced_word2),] - list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==tolower(input$advanced_word3),], 150) %>% mutate("Link" <- linkToWWO(keyword=.$word, session=session)) %>% .[c(3,2)]
        }
        if (input$advanced_math == "+" && input$advanced_math2 == "*") {
          data <- list_models[[input$modelSelect_analogies_tabs[[1]]]] %>% closest_to(list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==tolower(input$advanced_word1),] + list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==tolower(input$advanced_word2),] * list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==tolower(input$advanced_word3),], 150) %>% mutate("Link" <- linkToWWO(keyword=.$word, session=session)) %>% .[c(3,2)]
        }
        if (input$advanced_math == "+" && input$advanced_math2 == "/") {
          data <- list_models[[input$modelSelect_analogies_tabs[[1]]]] %>% closest_to(list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==tolower(input$advanced_word1),] + list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==tolower(input$advanced_word2),] / list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==tolower(input$advanced_word3),], 150) %>% mutate("Link" <- linkToWWO(keyword=.$word, session=session)) %>% .[c(3,2)]
        }

        if (input$advanced_math == "-" && input$advanced_math2 == "+") {
          data <- list_models[[input$modelSelect_analogies_tabs[[1]]]] %>% closest_to(list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==tolower(input$advanced_word1),] - list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==tolower(input$advanced_word2),] + list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==tolower(input$advanced_word3),], 150) %>% mutate("Link" <- linkToWWO(keyword=.$word, session=session)) %>% .[c(3,2)]
        }
        if (input$advanced_math == "-" && input$advanced_math2 == "-") {
          data <- list_models[[input$modelSelect_analogies_tabs[[1]]]] %>% closest_to(list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==tolower(input$advanced_word1),] - list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==tolower(input$advanced_word2),] - list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==tolower(input$advanced_word3),], 150) %>% mutate("Link" <- linkToWWO(keyword=.$word, session=session)) %>% .[c(3,2)]
        }
        if (input$advanced_math == "-" && input$advanced_math2 == "*") {
          data <- list_models[[input$modelSelect_analogies_tabs[[1]]]] %>% closest_to(list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==tolower(input$advanced_word1),] - list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==tolower(input$advanced_word2),] * list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==tolower(input$advanced_word3),], 150) %>% mutate("Link" <- linkToWWO(keyword=.$word, session=session)) %>% .[c(3,2)]
        }
        if (input$advanced_math == "-" && input$advanced_math2 == "/") {
          data <- list_models[[input$modelSelect_analogies_tabs[[1]]]] %>% closest_to(list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==tolower(input$advanced_word1),] - list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==tolower(input$advanced_word2),] / list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==tolower(input$advanced_word3),], 150) %>% mutate("Link" <- linkToWWO(keyword=.$word, session=session)) %>% .[c(3,2)]
        }

        if (input$advanced_math == "*" && input$advanced_math2 == "+") {
          data <- list_models[[input$modelSelect_analogies_tabs[[1]]]] %>% closest_to(list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==tolower(input$advanced_word1),] * list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==tolower(input$advanced_word2),] + list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==tolower(input$advanced_word3),], 150) %>% mutate("Link" <- linkToWWO(keyword=.$word, session=session)) %>% .[c(3,2)]
        }
        if (input$advanced_math == "*" && input$advanced_math2 == "-") {
          data <- list_models[[input$modelSelect_analogies_tabs[[1]]]] %>% closest_to(list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==tolower(input$advanced_word1),] * list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==tolower(input$advanced_word2),] - list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==tolower(input$advanced_word3),], 150) %>% mutate("Link" <- linkToWWO(keyword=.$word, session=session)) %>% .[c(3,2)]
        }
        if (input$advanced_math == "*" && input$advanced_math2 == "*") {
          data <- list_models[[input$modelSelect_analogies_tabs[[1]]]] %>% closest_to(list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==tolower(input$advanced_word1),] * list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==tolower(input$advanced_word2),] * list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==tolower(input$advanced_word3),], 150) %>% mutate("Link" <- linkToWWO(keyword=.$word, session=session)) %>% .[c(3,2)]
        }
        if (input$advanced_math == "*" && input$advanced_math2 == "/") {
          data <- list_models[[input$modelSelect_analogies_tabs[[1]]]] %>% closest_to(list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==tolower(input$advanced_word1),] * list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==tolower(input$advanced_word2),] / list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==tolower(input$advanced_word3),], 150) %>% mutate("Link" <- linkToWWO(keyword=.$word, session=session)) %>% .[c(3,2)]
        }

        if (input$advanced_math == "/" && input$advanced_math2 == "+") {
          data <- list_models[[input$modelSelect_analogies_tabs[[1]]]] %>% closest_to(list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==tolower(input$advanced_word1),] / list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==tolower(input$advanced_word2),] + list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==tolower(input$advanced_word3),], 150) %>% mutate("Link" <- linkToWWO(keyword=.$word, session=session)) %>% .[c(3,2)]
        }
        if (input$advanced_math == "/" && input$advanced_math2 == "-") {
          data <- list_models[[input$modelSelect_analogies_tabs[[1]]]] %>% closest_to(list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==tolower(input$advanced_word1),] / list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==tolower(input$advanced_word2),] - list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==tolower(input$advanced_word3),], 150) %>% mutate("Link" <- linkToWWO(keyword=.$word, session=session)) %>% .[c(3,2)]
        }
        if (input$advanced_math == "/" && input$advanced_math2 == "*") {
          data <- list_models[[input$modelSelect_analogies_tabs[[1]]]] %>% closest_to(list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==tolower(input$advanced_word1),] / list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==tolower(input$advanced_word2),] * list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==tolower(input$advanced_word3),], 150) %>% mutate("Link" <- linkToWWO(keyword=.$word, session=session)) %>% .[c(3,2)]
        }
        if (input$advanced_math == "/" && input$advanced_math2 == "/") {
          data <- list_models[[input$modelSelect_analogies_tabs[[1]]]] %>% closest_to(list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==tolower(input$advanced_word1),] / list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==tolower(input$advanced_word2),] / list_models[[input$modelSelect_analogies_tabs[[1]]]][rownames(list_models[[input$modelSelect_analogies_tabs[[1]]]])==tolower(input$advanced_word3),], 150) %>% mutate("Link" <- linkToWWO(keyword=.$word, session=session)) %>% .[c(3,2)]
        }

      }
      data
    }, escape = FALSE, colnames=c("Word", "Similarity to word(s)"),  options = list(lengthMenu = c(10, 20, 100, 150), pageLength = 10, searching = TRUE)))

    output$basic_table <- DT::renderDataTable(DT::datatable({
      # list_models[[input$modelSelect[[1]]]]
      data <- list_models[[input$modelSelect[[1]]]] %>% closest_to(tolower(input$basic_word1), 150) %>% mutate("Link" <- linkToWWO(keyword=.$word, session=session)) %>% .[c(3,2)]

    }, escape = FALSE, colnames=c("Word", "Similarity to word(s)"), options = list(dom = 't', pageLength = input$max_words_home, searching = FALSE)))


    output$basic_table_c1 <- DT::renderDataTable(DT::datatable({
      # list_models[[input$modelSelect[[1]]]]
      data <- list_models[[input$modelSelectc1[[1]]]] %>% closest_to(tolower(input$basic_word_c), 150) %>% mutate("Link" <- linkToWWO(keyword=.$word, session=session)) %>% .[c(3,2)]

    }, escape = FALSE, colnames=c("Word", "Similarity to word(s)"), options = list(dom = 't', pageLength = input$max_words, searching = FALSE)))


    output$basic_table_c2 <- DT::renderDataTable(DT::datatable({
      # list_models[[input$modelSelect[[1]]]]
      data <- list_models[[input$modelSelectc2[[1]]]] %>% closest_to(tolower(input$basic_word_c), 150) %>% mutate("Link" <- linkToWWO(keyword=.$word, session=session)) %>% .[c(3,2)]

    }, escape = FALSE, colnames=c("Word", "Similarity to word(s)"), options = list(dom = 't', pageLength = input$max_words, searching = FALSE)))


    output$tbl <- DT::renderDataTable(DT::datatable({
      data <- sapply(sample(1:150,4),function(n) {
        cword <- names(list_clustering[[input$modelSelect[[1]]]]$cluster[list_clustering[[input$modelSelect[[1]]]]$cluster==n][1:150])
        linkToWWO(keyword = cword, session = session)
      }) %>% as_data_frame()
    }, escape = FALSE, colnames=c(paste0("cluster_",1:4)), options = list(dom = 't', pageLength = input$max_words_home, searching = FALSE)))


    observeEvent(input$clustering_reset_input, {
      output$tbl <- DT::renderDataTable(DT::datatable({
        data <- sapply(sample(1:150,4),function(n) {
          cword <- names(list_clustering[[input$modelSelect[[1]]]]$cluster[list_clustering[[input$modelSelect[[1]]]]$cluster==n][1:150])
          linkToWWO(keyword = cword, session = session)
        }) %>% as_data_frame()
      }, escape = FALSE, colnames=c(paste0("cluster_",1:4)),options = list(dom = 't', pageLength = input$max_words_home, searching = FALSE)))
    })


    output$clusters_full <- DT::renderDataTable(DT::datatable({
      data <- sapply(sample(1:150,10),function(n) {
        ls_download_cluster <<- c(ls_download_cluster,n)
        cword <- names(list_clustering[[input$modelSelect_clusters[[1]]]]$cluster[list_clustering[[input$modelSelect_clusters[[1]]]]$cluster==n][1:150])
        linkToWWO(keyword = cword, session = session)
      }) %>% as_data_frame()

    }, escape = FALSE, colnames=c(paste0("cluster_",1:10)), options = list(dom = 'ft', lengthMenu = c(10, 20, 100, 150), pageLength = input$max_words_cluster, searching = TRUE)))



    observeEvent(input$clustering_reset_input_fullcluster, {
      ls_download_cluster <<- c()
      output$clusters_full <- DT::renderDataTable(DT::datatable({
        data <- sapply(sample(1:150,10),function(n) {
          ls_download_cluster <<- c(ls_download_cluster,n)
          cword <- names(list_clustering[[input$modelSelect_clusters[[1]]]]$cluster[list_clustering[[input$modelSelect_clusters[[1]]]]$cluster==n][1:150])
          linkToWWO(keyword = cword, session = session)
        }) %>% as_data_frame()
      }, escape = FALSE, colnames=c(paste0("cluster_",1:10)), options = list(dom = 'ft', lengthMenu = c(10, 20, 100, 150), pageLength = input$max_words_cluster, searching = TRUE)))
    })


    observeEvent(input$clustering_reset_input_fullcluster1, {
      ls_download_cluster <<- c()
      output$clusters_full <- DT::renderDataTable(DT::datatable({
        data <- sapply(sample(1:150,10),function(n) {
          ls_download_cluster <<- c(ls_download_cluster,n)
          cword <- names(list_clustering[[input$modelSelect_clusters[[1]]]]$cluster[list_clustering[[input$modelSelect_clusters[[1]]]]$cluster==n][1:150])
          linkToWWO(keyword = cword, session = session)
        }) %>% as_data_frame()
      }, escape = FALSE, colnames=c(paste0("cluster_",1:10)), options = list(dom = 'ft', lengthMenu = c(10, 20, 100, 150), pageLength = input$max_words_cluster, searching = TRUE)))
    })


  },
  options = list(port = 3939)
)
