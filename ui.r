# Sanity work------------
rm(list=ls(all=TRUE))
options(warn=-1)
#------------------------

source('global.R', local=F)

# These variables are not used by server. Hence these can be local
# For the input controls
status_names <- as.character(unique(df$status))
#------------------------------------------------------------------

options(device.ask.default = FALSE)
u <- shinyUI(fluidPage(
  
  shinyjs::useShinyjs(),
  tags$head(
    tags$script(src='my_script.js'),
    tags$link(rel = "stylesheet", type = "text/css", href = "custom_style.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "http://maxcdn.bootstrapcdn.com/font-awesome/4.1.0/css/font-awesome.min.css")
  ),
  navbarPage('MMP Prototype 2', selected="vizNM", id="nbp", 
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
                    mainPanel(
                      div(id = "mp", class="mp", 
                          h2("Network Plots"),
                          tabsetPanel(
                            
                            tabPanel("Spatial",
                                     div(id="nvf_body",
                                         div(id="nvf_legend",
                                            
                                           div(id="spatial_legend",
                                               h2("Options"),
                                               selectInput("map_name",
                                                           "Select map:",
                                                           # For debugging purposes change to maps[1] once finished
                                                           selected = maps[map_idx],
                                                           choices = maps),
                                               
                                               HTML('</br>'),
                                               
                                               # input group name
                                               textInput(inputId = 'inputGN', label='Search profile'),
                                               conditionalPanel(condition='input.inputGN != ""',
                                                                htmlOutput(outputId='gn_list')),
                                               
                                               checkboxGroupInput("filterEdges",
                                                                  "Filter relationship:",
                                                                  selected = unique(df$status_id),
                                                                  choices = c("Affiliates"=5, 
                                                                              "Allies"=2, 
                                                                              "Mergers"=3,
                                                                              "Rivals"=1,
                                                                              "Splinters"=4)
                                               ),
                                               HTML('</br>'),
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
                                               HTML('</br>'),
                                               sliderInput("range", round=T,
                                                           label = "Choose a start and end year:",
                                                           min = min(df$year), max = max(df$year), 
                                                           value = c(min(df$year), max(df$year)), 
                                                           sep = "",
                                                           width=360),
                                               
                                               HTML('</br></br></br>'),
                                               htmlOutput(outputId='year_slider'), # year slider for animation
                                               HTML('</br>'),
                                               div(id="animate_opts", 
                                                   div(id="animateChkbox",
                                                     checkboxInput(inputId='animate_spatial',
                                                                   label='Animate') 
                                                   ),
                                                   div(id="animateBtnDiv",
                                                   
                                                   #primary button    
                                                   actionButton(inputId="animateBtn", 
                                                                label="Play/Pause"),
                                                   
                                                   )
                                               ),
                                               
                                               uiOutput("nvf_legend_sub")
                                         ) # End for conditionalPanel
                                        ),
                                         

                                         div(id="nvf_mp",
                                             div(id="popDiv"),
                                             uiOutput("reg_hideDesc"),
                                             div(id="filterDesig_div", 
                                                 checkboxGroupInput("filterDesig",
                                                                    label='Filter Designation',
                                                                    choices=c('All','US','UN', 'State'),
                                                                    selected=c('All','US','UN', 'State'),
                                                                    inline=T
                                                                    )
                                                 ),
                                             visNetworkOutput("networkvisfinal",
                                                              width="100%", 
                                                              height="1000px"
                                                              ),
                                             HTML('
                                                   <div id="loadingBar">
                                                     <div class="outerBorder">
                                                     <div id="lb_text">0%</div>
                                                     <div id="border">
                                                     <div id="bar"></div>
                                                     </div>
                                                     </div>
                                                     </div>'
                                                  ),
                                     
                                     tags$script(
                                       '
                                       document.getElementById("spatial_legend").style.display = "block";
                                       
                                       var lb = document.getElementById("bar"); // inner part of the loading bar
                                        var txt = document.getElementById("lb_text");
                                        var loadingBar = document.getElementById("loadingBar");
                                        function updatePB(params)
                                        {
                                        loadingBar.style.display="block";
                                          var maxWidth = 496;
                                          var minWidth = 20;
                                          //alert(params.iterations + "" + params.total);
                                          let iterations = params.iterations;
                                          if(typeof params.iterations != "number"){
                                            iterations = 1000;
                                          }
                                          var widthFactor = iterations / 1000;
                                          var width = Math.max(minWidth, maxWidth * widthFactor);
                                          
                                          lb.style.width = width + "px";
                                          //alert(widthFactor + " " + params.iterations + " " + typeof iterations);
                                          txt.innerText = Math.round(widthFactor * 100) + "%";
                                          
                                          if(widthFactor==1)
                                            loadingBar.style.display="none";
                               
                                        }
                                        
                                        Shiny.addCustomMessageHandler("updatePB", updatePB);
                                        
                                        /*function completePB(dummy)
                                        {
                                          alert("completePB called");
                                          let maxWidth = 496;
                                          let minWidth = 20;
                                          lb.style.width = maxWidth + "px";
                                          //txt.innerText = "100%";
                                          //loadingBar.style.display = "none";
                                        }*/
                                        Shiny.addCustomMessageHandler("completePB", updatePB);
                                        
                                       '
                                     ),
                                       
                                             div(id='footer', 
                                               uiOutput(outputId="link"),
                                               HTML("<img src='fullscreen.png' width='30px' height='30px' onmouseover='this.src=\"fullscreen_hover.png\";' onmouseout='this.src=\"fullscreen.png\";' onclick='toggleFS();' >")
                                             )
                                     )
                                     )
                                     ,
                                     style = "background-color: #FCFCF3;"),
                            
                            tabPanel("Hierarchical", 
                                     
                                     div(id='h_select_map', 
                                       selectInput("h_map_name",
                                                   "Select map:",
                                                   # For debugging purposes change to maps[1] once finished
                                                   selected = 'Iraq',
                                                   choices = unique(df$map_name),
                                                   width = '200px') 
                                       ),
                                     
                                     div(id='h_subcontainer', 
                                         div(id="year_ruler", 
                                             uiOutput("year_ruler_sub")),
                                         
                                         visNetworkOutput("visnetworktimeline", 
                                                          width="2477px",
                                                          height="1000px"),
                                         
                                         div(id="h_legend", uiOutput("h_legend_sub"))
                                       ),
                                     
                                     tags$script("
                                                 
                                      var year_ruler_sub = document.getElementById('year_ruler_sub');
                                      year_ruler_sub.style.marginTop = '0px';
                                      //var original_cy = 500;
                                      
                                      function moveLegend(d_y){
                                        //alert(c_y);
                                        //let diff_y = c_y - original_cy;
                                        let mTop = parseInt(year_ruler_sub.style.marginTop);
                                        let new_mTop = mTop + d_y;
                                        year_ruler_sub.style.marginTop = new_mTop + 'px';
                                      }
                                      
                                      Shiny.addCustomMessageHandler('moveLegend', moveLegend);
                                      const ZOOM_SPEED = 0.01;
                                      let zoom = 1;
                                      function scaleLegend(direction)
                                      {
                                        if(direction == '+')
                                          year_ruler_sub.style.transform = `scale(${zoom += ZOOM_SPEED})`;
                                        else
                                          year_ruler_sub.style.transform = `scale(${zoom -= ZOOM_SPEED})`;
                                      }
                                      
                                      Shiny.addCustomMessageHandler('scaleLegend', scaleLegend);
                                    
                                    "),
                                     
                                     style = "background-color: #FCFCF3;"),
                            
                            tabPanel("Sankey", 
                                     selectInput(inputId="s_map_name",
                                                 label="Map Name",
                                                 choices=maps,
                                                 selected='Iraq'),
                                     sankeyNetworkOutput("diagram",
                                                         height="500px"),
                                     style = "background-color: #FCFCF3;"),
                            
                            tabPanel("Geographical",
                                     selectInput("g_map_name", 
                                                 label="Select Map:", 
                                                 choices = unique(df$map_name),
                                                 selected = "Iraq"),
                                     plotOutput("geoMap"),
                                     style = "background-color: #FCFCF3;"),
                            
                            tabPanel("Map",
                                     selectInput(inputId='m_map_name', 
                                                 label='Map Name',
                                                 choices=maps,
                                                 selected='Iraq'),
                                     leafletOutput(outputId='mapOutput')
                                     ),
                            
                            tabPanel("Statistics",

                                       sidebarLayout(
                                         sidebarPanel(
                                           selectInput(inputId='stat_level',
                                                       label='Information',
                                                       choices=c('General'='General',
                                                                 'Map'='Map',
                                                                 'Active Groups'='ActiveGroups',
                                                                 'Number of Profiles/Map'='NProfilesInMap',
                                                                 'Top 5 Profiles'='top5profiles'),
                                                       selected='General'),
                                           width=4
                                         ),
                                         mainPanel(
                                           conditionalPanel('input.stat_level=="General"',
                                               div(id='generalStats',
                                                   div(id='generalStats_mapController',
                                                       sliderInput(inputId='stats_sample_size',
                                                                   label='Sample size',
                                                                   min=10, max=30, step=5,
                                                                   value=10),
                                                       HTML("</br>")
                                                   ),

                                                   div(id='sub_generalStats1',
                                                       plotOutput("membersGrowth"),
                                                       HTML("
                                                       <div class='fig_caption'>The average growth of members over the years the group had been active and the color represents the amount of influence a group can have on the entire extremist network</div>"
                                                       )
                                                   ),

                                                   div(id='sub_generalStats2',
                                                       h3("Active profiles"),
                                                       dataTableOutput("showActiveProfiles")
                                                   )
                                               )
                                           ),
                                           
                                           conditionalPanel('input.stat_level=="Map"',
                                                            div(
                                                              HTML("</br>"),
                                                              plotOutput("basicStats_map", height='1800px',
                                                                         width='1800px'),
                                                            )
                                                          ),
                                           conditionalPanel('input.stat_level=="ActiveGroups"',
                                                            
                                                            div(
                                                              plotOutput("activeg_year", height='900px')
                                                            )
                                                            
                                                            ),
                                           conditionalPanel('input.stat_level=="NProfilesInMap"',
                                                            
                                                            div(
                                                              plotOutput("nprofiles_map", height='1200px',
                                                                        width='1600px')
                                                            )
                                                            
                                           ),
                                           
                                           conditionalPanel('input.stat_level=="top5profiles"',
                                                            plotOutput("top.profiles.most.edges")
                                                            ),
                                           
                                           conditionalPanel('input.stat_level="ttest"',
                                                            textOutput("showTtest")
                                                            ),
                                           
                                           width=8
                                            ),
                                         fluid=F
                                         )
                                         
                                       ),
                                     
                            
                          ) # End of tabset panel that covers all types of viz
                          
                      ), # End of div that covers div
                    ) # End of mainPanel for visualize the data 
             ), # End of tabPanel for visualiZe the data
             
             tabPanel(title='Admin', id='admin', value="admin", 
                      mainPanel(
                         tabsetPanel(id = 'admin_tbsp', selected='admin_em',
                                     tabPanel(title='Edit Maps', value='admin_em',
                                              div(id='admin_em_sm',
                                                uiOutput(outputId='em_profiles'),
                                              ),
                                              
                                              div(id='admin_em_mm',
                                                  h2('Manage a Map'),
                                                  div(id="save_button_container", 
                                                      actionButton(inputId='em_mp_save',
                                                                   label='Save Changes'),
                                                      actionButton(inputId='em_mp_back',
                                                                   label='Back')),
                                                
                                                HTML('<div id="admin_em_mm_node_details" class="admin_em_mm_container">
                                                     <table class="em_mm_table">
                                                     <tr>
                                                     <td><h4>Name</h4></td>
                                                     <td><input type="text" id="em_mp_name"  name="em_mp_name" class="emfield_input" /></td>
                                                     </tr>
                                                     
                                                     <tr>
                                                     <td><h4>URL</h4></td>
                                                     <td><input type="text" id="em_mp_url"  name="em_mp_url" class="emfield_input" /></td>
                                                     </tr>
                                                     
                                                     <tr>
                                                     <td><h4>Description</h4></td>
                                                     <td><textarea id="em_mp_desc" class="emfield_input" maxlength="500"></textarea></td>
                                                     </tr>
                                                     
                                                     <tr>
                                                     <td><h4>Start year</h4></td>
                                                     <td><input type="text" id="em_mp_sy"  name="em_mp_sy" class="emfield_input" /></td>
                                                     </tr>
                                                     
                                                     <tr>
                                                     <td><h4>End year</h4></td>
                                                     <td><input type="text" id="em_mp_ey"  name="em_mp_ey" class="emfield_input" /></td>
                                                     </tr>
                                                     
                                                     <tr>
                                                     <td><h4>Published</h4></td>
                                                     <td><input type="checkbox" id="em_mp_pub"  name="em_mp_pub" /></td>
                                                     </tr>
                                                     
                                                     </table></br></br></div>
                                                   '),
                                                
                                                HTML('<div id="admin_em_mm_zoom_levels" class="admin_em_mm_container">
                                                        <h2>ZOOM LEVELS</h2>
                                                        <select name="zoom_levels", id="zoom_levels"></select>
                                                      </div>
                                                     '),
                                                
                                                HTML('
                                                     <div id="admin_em_mm_groups" class="admin_em_mm_container">
                                                       <h2>INCLUDED GROUPS</h2>
                                                       <div id="included_groups" class="admin_em_mm_container">
                                                          
                                                       </div>
                                                     </div>
                                                     ')
                                                )
                                                
                                                ),
                                     tabPanel(title='Edit Profiles1', value='admin_ep1',
                                              div(id='admin_ep_sp1',
                                                  
                                                htmlOutput(outputId='ep_profiles'),
                                                actionButton(inputId='ep_save_changes',
                                                             label='Save Changes'),
                                                actionButton(inputId='ep_discard_changes',
                                                             label='Discard Changes'),
                                                HTML('</br></br>')
                                                )
                                              )
                                              )
                                     )
                         ),
             tabPanel(title="Profile Links", id="plNM", value="plNM")
  ))
  
)
































