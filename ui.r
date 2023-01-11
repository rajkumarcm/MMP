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
  navbarPage('Mapping Militants Project', selected="vizNM", id="nbp", 
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
                          tags$p("From 2009 to 2012, MMP was funded by an award from the Social and Behavioral Dimensions of National Security, Conflict, and Cooperation competition, a joint venture between the National Science Foundation and the Department of Defense. From 2012 to 2019 the research was supported by Stanford University, including the Freeman Spogli Institute for International Studies Policy Implementation Lab. In 2019, the project received funding from the", tags$a(href = "https://www.unomaha.edu/ncite/index.php", "National Counterterrorism, Innovation, Technology, and Education Center (NCITE)", .noWS = "outside"), " a U.S. Department of Homeland Security Center of Excellence.  The project relies primarily on research assistance from Stanford undergraduate and graduate students.", '!', .noWS = c("after-begin", "before-end")),
                          tags$br(),
                          tags$p("This app lets you manipulate, explore, and download the Mapping Militants Project (MMP) data. It was created in Shiny using R (V 4.2.1)"),
                          tags$p("MMP records information about 112 different groups and 712 different relationships among these groups across 15 global conflict theaters."),
                          # tags$br(),
                          tags$p("This app has the following main functionalities:"),
                          tags$ul(
                            tags$li("The Download the Data tab generates downloadable portions of the MMP relational edge and node data."),
                            tags$li("The Visualize the Data tab generates different network diagrams including a spatial, hierarchical, and geographic map. You can choose the maps, groups, and relationships to visualize. There is also an option to generate descriptive statistics and information regarding the MMP data")
                          ),
                          tags$br(),
                          )
                      ),
             
             tabPanel(title='Download the Data', id='downloadNM', value="downloadNM",
                      div(id="dd_body",
                        div(id="dd_sp",
                            
                          selectInput("dd_map_name",
                                      "Select Map:",
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
                          tabsetPanel(id='vz_tbsp', selected='vz_spatial',
                            
                            tabPanel(title="Spatial", value="vz_spatial",
                                     div(id="nvf_body",
                                         div(id="nvf_legend",
                                            
                                           div(id="spatial_legend",
                                               h2("Options"),
                                               selectInput("map_name",
                                                           "Select Map:",
                                                           # For debugging purposes change to maps[1] once finished
                                                           selected = maps[2],
                                                           choices = maps),
                                               
                                               HTML('</br>'),
                                               
                                               # input group name
                                               textInput(inputId = 'inputGN', label='Search Profile:'),
                                               conditionalPanel(condition='input.inputGN != ""',
                                                                htmlOutput(outputId='gn_list')),
                                               
                                               div(id="chkbx_filter_div",
                                                   div(class="sub_chkbx_filter_div",
                                                     checkboxGroupInput("filterEdges",
                                                                        "Filter Relationship:",
                                                                        selected = unique(df$status_id),
                                                                        choices = c("Affiliates"=5, 
                                                                                    "Allies"=2, 
                                                                                    "Mergers"=3,
                                                                                    "Rivals"=1,
                                                                                    "Splinters"=4)
                                                                        )
                                                       ),
                                                   div(class="sub_chkbx_filter_div", 
                                                       checkboxGroupInput("filterDesig",
                                                                          label='Filter Designation:',
                                                                          choices=c('All','US','UN', 'State', 'Others'),
                                                                          selected=c('All','US','UN', 'State', 'Others')
                                                                         )
                                                       )
                                               ),
                                               HTML('</br>'),
                                               selectInput("selectStatus",
                                                           "Highlight One status:",
                                                           selected = 0,
                                                           choices = c("None"=0,
                                                                       "Affiliates"=5, 
                                                                       "Allies"=2, 
                                                                       "Mergers"=3,
                                                                       "Rivals"=1,
                                                                       "Splinters"=4)
                                                          ),
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
                                                                   label='Show changes over time') 
                                                   ),
                                                   div(id="animateBtnDiv",
                                                   
                                                   #primary button    
                                                   actionButton(inputId="animateBtn", 
                                                                label="Animate",
                                                                icon=icon("play-circle")),
                                                   
                                                   )
                                               ),
                                               
                                               uiOutput("nvf_legend_sub")
                                         ) # End for conditionalPanel
                                        ),
                                         

                                         div(id="nvf_mp",
                                             div(id="popDiv"),
                                             uiOutput("reg_hideDesc"),
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
                                     style = "background-color: #f0f0f0;"),
                            
                            tabPanel(title="Hierarchical", value="vz_hierarchical",
                                     div(id="popDiv2"),
                                     uiOutput("reg_hideDesc2"),
                                     
                                     div(id="h_legend", 
                                         uiOutput(outputId="h_legend_sub")),
                                     
                                     div(id='h_select_map', 
                                       selectInput("h_map_name",
                                                   "Select Map:",
                                                   # For debugging purposes change to maps[1] once finished
                                                   selected = 'Iraq',
                                                   choices = maps[2:length(maps)],
                                                   width = '200px')
                                       ),
                                     
                                     div(id='h_subcontainer', 
                                         div(id="year_ruler", 
                                             uiOutput("year_ruler_sub")),
                                         
                                         visNetworkOutput("visnetworktimeline", 
                                                          width="2477px",
                                                          height="1000px"),
                                       ),
                                     
                                     div(id="h_tb_container",
                                         div(id="tb_year_ruler_container", 
                                             uiOutput("tb_year_ruler")),
                                         
                                         visNetworkOutput("h_traceback",
                                                          width="1660px")
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
                                     
                                     style = "background-color: #f0f0f0;"),
                            

                            tabPanel(title="Geographical", value="vz_geo",
                                     selectInput(inputId='g_map_name', label='Map Name:',
                                                 choices=maps,
                                                  selected=maps[1]),
                                     plotlyOutput("geoMap", height="810px", width="1411px"),
                                     style = "background-color: #f0f0f0;"),

                            tabPanel(title="Statistics", value="vz_stats",

                                       sidebarLayout(
                                         sidebarPanel(
                                           selectInput(inputId='stat_level',
                                                       label='Summary Statistics',
                                                       choices=c('General'='General',
                                                                 'Map'='Map',
                                                                 'Active Groups'='ActiveGroups',
                                                                 'Number of Profiles/Map'='NProfilesInMap'#,
                                                                 #'Top 5 Profiles'='top5profiles'
                                                                 ),
                                                       selected='General'),
                                           width=4
                                         ),
                                         mainPanel(
                                           conditionalPanel('input.stat_level=="General"',
                                               div(id='generalStats',
                                                   div(id='generalStats_mapController',
                                                       sliderInput(inputId='stats_sample_size',
                                                                   label='Sample Size: Number of profiles to include in the following plot',
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
                                                              plotOutput("basicStats_map", height='1100px',
                                                                         width='1300px'),
                                                            )
                                                          ),
                                           conditionalPanel('input.stat_level=="ActiveGroups"',
                                                            
                                                            div(
                                                              plotOutput("activeg_year", height='900px')
                                                            )
                                                            
                                                            ),
                                           conditionalPanel('input.stat_level=="NProfilesInMap"',
                                                            
                                                            div(
                                                              plotOutput("nprofiles_map", height='800px',
                                                                        width='1300px')
                                                            )
                                                            
                                           ),
                                           # 
                                           # conditionalPanel('input.stat_level=="top5profiles"',
                                           #                  plotOutput("top.profiles.most.edges")
                                           #                  ),
                                           
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
                  tabsetPanel(id = 'admin_tbsp', selected='admin_ep1',
                    
                     tabPanel(title='Edit Profiles', value='admin_ep1',
                        div(id='admin_ep_sp1',
                          HTML('</br>'),
                          # actionButton(inputId='newProf_btn', label='New Profile/Back'),
                          # HTML('</br></br>'),
                          div(id="ep_profiles_container",
                            htmlOutput(outputId='ep_profiles'),
                            actionButton(inputId='ep_save_changes',
                                         label='Save Changes'),
                            # actionButton(inputId='ep_discard_changes',
                            #              label='Discard Changes'),
                              )
                            ) # Closing admin_ep_sp1
                          ), # Closing edit profiles
                      tabPanel(title='New Profile', value='admin_np',
                             
                             HTML('</br></br>'),
                             HTML('
                             <button id="fill_debug" onclick="debug_newProf()">Fill for debugging</button>
                             <div id="new_prof_container" class="admin_new_container">
                                <table>
                                <tr>
                                 <td><label>Name<span class="mandatory">*</span></label></td>
                                 <td><input type="text" id="new_prof_name" class="emfield_input_small "/></td>
                                </tr>
                                <tr>
                                 <td><label>URL<span class="mandatory">*</span></label></td>
                                 <td><input type="text" id="new_prof_url" class="emfield_input_small "/></td>
                                </tr>
                                <tr>
                                  <td><label>Description</label></td>
                                  <td><textarea id="new_prof_desc" class="emfield_input_small" style="width:500px;" maxlength="500"></textarea></td>
                                </tr>
                                <tr>
                                  <td><label>Year founded<span class="mandatory">*</span></label></td>
                                  <td><input type="number" id="new_prof_sy"  name="new_prof_sy" class="emfield_input_small" min="1700" /></td>
                                </tr>
                                <tr>
                                  <td><label>Year dissolved<span class="mandatory"></span></label></td>
                                  <td><input type="number" id="new_prof_ey" class="emfield_input_small" min="0" max="2022"  /></td>
                                </tr>
                                <tr>
                                  <td><label>Active<span class="mandatory"></span></label></td>
                                  <td><input type="checkbox" id="new_prof_active" style="margin-left:100px;" /></td>
                                </tr>
                                <tr>
                                  <td><label>Complete</label></td>
                                  <td><input type="checbox" id="new_prof_complete" class="emfield_input_small" /></td>
                                </tr>

                                <tr>
                                  <td><label>First attack<span class="mandatory">*</span></label></td>
                                  <td>
                                    <input type="number" id="new_prof_fatt" min="1700" max="2022" class="emfield_input_small" />
                                  </td>
                                </tr>
                                </tr>
                                <tr>
                                  <td><label>Last attack</label></td>
                                  <td>
                                    <input type="number" id="new_prof_latt" min="1700" max="2022" class="emfield_input_small" />
                                  </td>
                                </tr>
                                </tr>
                                <tr>
                                  <td><label>Last updated<span class="mandatory">*</span></label></td>
                                  <td>
                                    <input type="number" id="new_prof_lupdated" min="1700" max="2022" class="emfield_input_small" />
                                  </td>
                                </tr>
                                <tr>
                                  <td><label>HQ City<span class="mandatory">*</span></label></td>
                                  <td><input type="text" id="new_prof_city" class="emfield_input_small" /></td>
                                </tr>
                                <tr>
                                  <td><label>HQ Country<span class="mandatory">*</span></label></td>
                                  <td><input type="text" id="new_prof_country" class="emfield_input_small"  /></td>
                                </tr>
                                <tr>
                                  <td><label>HQ Province<span class="mandatory">*</span></label></td>
                                  <td><input type="text" id="new_prof_province" class="emfield_input_small" /></td>
                                </tr>
                                <tr>
                                  <td><label>Initial member size<span class="mandatory">*</span></label></td>
                                  <td><input type="number" id="new_prof_ims" min="0" class="emfield_input_small" /></td>
                                </tr>
                                <tr>
                                  <td><label>Initial size year</label></td>
                                  <td><input id="new_prof_isy" type="number" min="0" class="emfield_input_small"/></td>
                                </tr>
                                <tr>
                                  <td><label>Min size members</label></td>
                                  <td><input id="new_prof_msm" type="number" min="0" class="emfield_input_small"/></td>
                                </tr>
                                <tr>
                                  <td><label>Max size members<span class="mandatory">*</span></label></td>
                                  <td><input id="new_prof_max_sm" type="number" min="0" class="emfield_input_small"/></td>
                                </tr>
                                <tr>
                                  <td><label>Min size year</label></td>
                                  <td><input id="new_prof_msy" type="number" min="0" class="emfield_input_small"/></td>
                                </tr>
                                <tr>
                                  <td><label>Max size year</label></td>
                                  <td><input id="new_prof_max_sy" type="number" min="0" class="emfield_input_small"/></td>
                                </tr>
                                <tr>
                                  <td><label>Designated types</label></td>
                                  <td>'),
                             checkboxGroupInput(inputId='new_prof_spons_types', label='',
                                                choices=c('US', 'UN', 'State',
                                                          'Others'), inline=T),
                             HTML(' 
                                  </td>
                                </tr>
                                <tr>
                                  <td></td>
                                  <td><label style="margin-left:100px; color:maroon; font-style:italic;">Sponsor names must be seperated by a comma</label></td>
                                </tr>
                                <tr>
                                  <td><h4>Sponsor Names</h4></td>
                                  <td><input type="text" id="new_prof_spons_names" class="emfield_input_small" /></td>
                                </tr>
                                <tr>
                                  <td><h4>Other designated names</h4></td>
                                  <td><input type="text" id="new_prof_od_names" class="emfield_input_small" /></td>
                                </tr>
                                <tr>
                                  <td></td>
                                  <td><label style="margin-left:100px; color:maroon; font-style:italic;">Sponsor names must be seperated by a comma</label></td>
                                </tr>
                                <tr>
                                  <td><h4>Published</h4></td>
                                  <td><input type="checkbox" id="new_prof_pub"  name="new_prof_pub" style="margin-left:100px;" /></td>
                                </tr>
                                <tr>
                                 <td><h4>Notes</h4></td>
                                 <td><textarea id="new_prof_comments" class="emfield_input_small"></textarea></td>
                                </tr>
                                </table>
                                <button id="new_prof_sc_btn" onclick="javascript:new_prof_sc();">Save Changes</button>
                                </br><br/>
                                <div id="new_prof_warnings_container" class="warnings_div">'),
                             textOutput(outputId="new_prof_warnings", inline = F),
                             HTML('</div></div></br></br></br>')
                             ),
                    
                      tabPanel(title='New Edge', value='admin_nr',
                         HTML('
                              <div id="new_edge_container" class="admin_new_container">
                                <table>
                                  <tr>
                                   <td><h4>From group name</h4></td>
                                   <td>'
                         ),
                         
                         selectInput(inputId='new_rel_fgn', label='', 
                                     choices=profile_names,
                                     selected=profile_names[1]),
                         
                         HTML('   </td>
                                 </tr>
                                 
                                 <tr>
                                  <td><h4>To group name</h4></td>
                                  <td>'
                             ),
                         selectInput(inputId='new_rel_tgn', label='',
                                     choices=profile_names,
                                     selected=profile_names[1]),
                                  
                        HTML('    </td>
                                 </tr>
                                 
                                 <tr>
                                  <td><h4>Type</h4></td>
                                  <td>
                                   <select id="new_rel_type" style="background-color:white;">
                                     <option value="Splinters">Splinters</option>
                                     <option value="Allies">Allies</option>
                                     <option value="Rivals">Rivals</option>
                                     <option value="Mergers">Mergers</option>
                                     <option value="Affiliates">Affiliates</option>
                                   </select>
                                  </td>
                                 </tr>
                                 
                                 </tr>
                                  <tr>
                                    <td><h4>Map name</h4></td>
                                    <td><div id="new_rel_nm_flex" style="flex-direction:row;">'
                              ),
                              
                              selectInput(inputId='new_rel_map_name', label='',
                                          choices=c(maps[2:length(maps)], 'Other'),
                                          selected=maps[2], multiple=T),
                              textInput(inputId='new_mn', label=''),
                                    
                              HTML('</div></td>
                                  </tr>
                                  
                                  <tr>
                                   <td><h4>Description</h4></td>
                                   <td><textarea id="new_rel_desc" style="width:500px;" maxlength="500"></textarea></td>
                                  </tr>
                                  
                                  <tr>
                                   <td><h4>Year</h4></td>
                                   <td><input type="number" id="new_rel_year" min="1700" max="2022"></td>
                                  </tr>
                                  
                                   <tr>
                                    <td><h4>Primary</h4></td>
                                    <td>'
                              ),
                             selectInput(inputId='new_rel_primary', label='',
                                         choices=maps[2:length(maps)],
                                         selected=maps[2]),
                             actionButton(inputId="new_rel_mns_update", 
                                          label="Update Map Name"
                                          ),
                             
                             HTML('</td>
                                  </tr>
                                </table>
                                <div class="save_button">
                                  <button id="new_rel_sc_button", onclick="new_rel_trigger();">Create edge</button>
                                </div>
                                </br><br/>
                                <div id="new_rel_warnings_container" class="warnings_div">'),
                                 textOutput(outputId="new_rel_warnings", inline = F),
                        
                          HTML('</div>
                              ')
                              )
                    ) # Closing tabsetPanel(admin_tbsp)
                   ) # Closing mainPanel
                  ), # Closing tabPanel
                 tabPanel(title="Profile Links", id="plNM", value="plNM")
  ),
  
  div(id='logo_div', 
      HTML('<img src="mmp-white.png" style="width:7%; margin-left:0%;" alt="MMP Logo">')
      )
  
  )
  
)
































