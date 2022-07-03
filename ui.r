# Sanity work------------
rm(list=ls(all=TRUE))
options(warn=-1)
#------------------------

source('global.R', local=F)

# These variables are not used by server. Hence these can be local
# For the input controls
maps <- c("All", unique(df$map_name))
status_names <- as.character(unique(df$status))
#------------------------------------------------------------------


u <- shinyUI(fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom_style.css")
  ),
  
  shinyjs::useShinyjs(),
  navbarPage('MMP Prototype 2', selected="vizNM",  
             tabPanel(title='About the App', id='aboutNM',
                      value='aboutNM',
                      div(id="about_ct",
                          tags$div(id='about_title_div',
                          HTML("<h2>&nbsp;Mapping Militants Project</h2>")),
                          tags$br(),
                          tags$div(id="mpp_video", 
                                   HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/5Gj_l0x-SEQ" title="CISAC Who We Are REVISED" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
                                   ),
                          tags$br(),
                          tags$p("The MMP research project traces the evolution of militant organizations and the interactions that develop among them over time. Findings are presented in interactive “maps,” which provide both (1) visual representations of how relationships among militant organizations change over time and (2) links to in-depth profiles of individual groups. The project provides uniquely accessible and clear genealogical information about violent extremist organizations that, combined with the detailed group profiles, is an invaluable resource to students, scholars, journalists, policy analysts, and others interested in violent oppositional organizations. The project helps identify patterns in, as well as causes and consequences of, violent extremist group evolution by describing and comparing the genealogy of different families of organizations. Genealogies are presented in interactive diagrams or “maps” that detail how groups form, split, merge, collaborate, compete, shift ideological direction, adopt or renounce violence, grow, shrink, and eventually decline over time.  The MMP research project also provides a database of detailed and documented group profiles. It develops computer software to assemble, organize, and display the profiles and genealogical information that researchers have produced."),
                          tags$br(),
                          tags$p("From 2009 to 2012, MMP was funded by an award from the Social and Behavioral Dimensions of National Security, Conflict, and Cooperation competition, a joint venture between the National Science Foundation and the Department of Defense. From 2012 to 2019 the research was supported by Stanford University, including the Freeman Spogli Institute for International Studies Policy Implementation Lab. In 2019, the project received funding from the", tags$a(href = "https://www.unomaha.edu/ncite/index.php", "National Counterterrorism, Innovation, Technology, and Education Center (NCITE)", .noWS = "outside"), " a U.S. Department of Homeland Security Center of Excellence.  The project relies primarily on research assistance from Stanford undergraduate and graduate students.", '!', .noWS = c("after-begin", "before-end"))
                          )
             ),
             
             tabPanel(title='Download the data', id='downloadNM', value="downloadNM",
                      div(id="dd_body",
                        div(id="dd_sp",
                            
                          selectInput("dd_map_name",
                                      "Select map:",
                                      # For debugging purposes change to maps[1] once finished
                                      selected = maps[1],
                                      choices = maps),
                          
                          sliderInput("dd_range", 
                                      label = "Choose a start and end year:",
                                      min = min(df$year), max = max(df$year), 
                                      value = c(min(df$year), max(df$year)), 
                                      sep = "",
                                      width=360) ,
                          div(id="db_div",
                          downloadButton('downloadData'))
                        ),
                        
                        
                        div(id="dd_mp",
                            dataTableOutput('dataTable')
                          )
                      ) # dd_body
             ), # tabpanel
             
             tabPanel(title='Visualize the Data', id='vizNM', value='vizNM',
                      sidebarLayout(position = "left",
                                    shinyjs::hidden(
                                      div( id="sp", class="sp",
                                           div(id="sp2", 
                                               div(id ="icon_div",
                                                   tags$img(id="closeSp", height = 20, width = 20, 
                                                            src = 'close.png') ),
                                               h2("Options"),
                                               selectInput("map_name",
                                                           "Select map:",
                                                           # For debugging purposes change to maps[1] once finished
                                                           selected = maps[2],
                                                           choices = maps),
                                               
                                               checkboxGroupInput("filterEdges",
                                                                  "Filter relationship:",
                                                                  selected = unique(df$status_id),
                                                                  choices = c("Affiliates"=5, 
                                                                              "Allies"=2, 
                                                                              "Mergers"=3,
                                                                              "Rivals"=1,
                                                                              "Splinters"=4)
                                               ),
                                               selectInput("selectStatus",
                                                           "Highlight one status",
                                                           selected = 0,
                                                           choices = c("None"=0,
                                                                       "Affiliates"=5, 
                                                                       "Allies"=2, 
                                                                       "Mergers"=3,
                                                                       "Rivals"=1,
                                                                       "Splinters"=4
                                                                       )),
                                               
                                               sliderInput("range", 
                                                           label = "Choose a start and end year:",
                                                           min = min(df$year), max = max(df$year), 
                                                           value = c(min(df$year), max(df$year)), 
                                                           sep = "",
                                                           width=360)
                                               
                                           ))),
                                    
                                    mainPanel(
                                      div(id = "mp", class="mp",
                                          h2("Network Plots"),
                                          actionButton('toggleMenu', 'Filter'),
                                          tabsetPanel(
                                            
                                            tabPanel("Spatial",
                                                     visNetworkOutput("networkvisfinal",
                                                                      width="2000px", 
                                                                      height="1200px"),
                                                     style = "background-color: #eeeeee;"),
                                            tabPanel("Hierarchical", 
                                                     visNetworkOutput("visnetworktimeline",
                                                                      height="500px"),
                                                     style = "background-color: #eeeeee;"),
                                            tabPanel("Sankey", 
                                                     sankeyNetworkOutput("diagram",
                                                                         height="500px"),
                                                     style = "background-color: #eeeeee;")
                                            
                                          )
                                      )
                                    )
                      )
             ),
             tabPanel(title='Profile Links', id='plNM', value='plNM'),
             id='nbp',
  )
  
))


