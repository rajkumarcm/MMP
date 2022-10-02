/*
When relationship type is clicked on legend, show pop up with
information about the relationship.
*/
var affiliates = "<p  style='color:#FCFCE5;'>When a group pledges fealty to and relies on support (materiel, financial, ideological, etc) and/or guidance from another, usually more senior group, it is defined as an affiliate of that more senior group.</p>"+

"<p  style='color:#FCFCE5;'>Example: Usama Bin Ladin’s al-Qaida organization has many affiliate groups that have sworn allegiance to al-Qaida. Most of these affiliate groups exist in areas distinct from al-Qaida’s base of operations in the Afghanistan/Pakistan border areas. Algeria-based Al-Qaida in the Islamic Maghreb (AQIM), formerly the Salafist Group for Preaching and Combat (GSPC), pledged fealty to al-Qaida and is thus considered an al-Qaida affiliate; the same is true for al-Qaida in Iraq (AQI).<br><br></p>";

var allies = "<p  style='color:#FCFCE5;'>When groups are known to share similar ideology and/or goals and are known to communicate and sometimes coordinate operations, they are identified as allies. Ally relationship may contain elements of competition amongst groups/group members, but in general, the relationship is defined as one of cooperation.</p>"+

"<p  style='color:#FCFCE5;'>Example: In Southeast Asia, the relationship between two Sunni jihadist groups, the Abu Sayyaf Group (ASG) and Jemaah Islamiyah (JI), is characterized as an ally relationship. Philippines-based ASG elements have provided shelter to fugitive JI members and ASG elements have reportedly supported JI operations in the past. JI is also allied with al-Qaida, from which JI received both logistical and operational support; JI has retained its independence from al-Qaida, thus it is not characterized as an al-Qaida affiliate.<br><br></p>";

var merger = "<p  style='color:#FCFCE5;'>Group mergers occur when two or more groups agree to consolidate resources and operate jointly under the same banner towards the same cause, thus forming a new group. Group mergers require one or all groups shed their original identity and commit to the new group’s articulated vision.</p>"+

"<p  style='color:#FCFCE5;'>Example: Ayman al-Zawahiri’s Egyptian Islamic Jihad (EIJ) merges with Usama Bin Ladin’s al-Qaida in early 2001; the EIJ ceases to exist contemporaneously and al-Zawahiri becomes al-Qaida’s second in command.<br><br></p>";

var rivals = "<p  style='color:#FCFCE5;'>When groups engage in sustained competition, often vying for resources, prestige, and/or support, they are understood as rivals. Rival groups can engage in violence against each other, though sometimes the rivalry is less explicit. Shared ideology does not preclude groups from being rivals. Some rivalries may contain elements of cooperation, but in general, the relationship is defined as on of competitiveness.</p>"+

"<p  style='color:#FCFCE5;'>Example: In the Palestinian Territories, the Islamic Resistance Movement (HAMAS) and Palestinian Islamic Jihad (PIJ) are considered rivals despite their shared credentials as Sunni jihadist groups committed to violence against Israel. While there have been instances of cooperation between HAMAS and PIJ operatives, in general, the two groups work independently and compete for support among the Palestinian population and external supporters. Often, HAMAS and PIJ will articulate their differences in opinion/strategy/etc. publicly.<br><br></p>";

var splits = "<p  style='color:#FCFCE5;'>When part of one group establishes itself as an independent entity (almost always with a new name), it splits from the parent group. A group may also splinter into several smaller groups. It is important to note that splits are not always the result of dissension; sometimes a split is a tactical decision. For example, splitting into militant and political arms might grant the political arm more legitimacy while still allowing it to carry out militant activities.<br><br></p>";

function showDesc(label)
{
  text = "";
  if(label == "Allies")
    text = allies;
  else if(label == "Affiliates")
    text = affiliates;
  else if(label == "Mergers")
    text = merger;
  else if(label == "Rivals")
    text = rivals;
  else if(label == "Splinters")
    text = splits;
  var popDiv = document.getElementById("popDiv");
  //"<img src='close.png' onclick='hideDesc();' style='height:20px; width:20px; margin-left:90%; margin-top:20px;' />
  popDiv.innerHTML = "<div id='divPopTxt'>"+
                      "<div style='width:100%;'><h2 style='width:120px; height:100px; text-transform:uppercase;'>" + label + "</h2>" + 
                      "</div>"+
                      text + "</div>" ;
  popDiv.style.display ='block';
}

function hideDesc()
{
  let popDiv = document.getElementById("popDiv");
  popDiv.style.display ='none';
}

function toggleFS(){
  if(!document.fullscreenElement)
  {
    var elem = document.getElementById('nvf_body');
    elem.requestFullscreen();
  }
  else
    document.exitFullscreen();
}

function toggleSL(dummy) // show spatial legend
{
  let spatial_legend = document.getElementById('spatial_legend');
  if(spatial_legend.style.display == 'block')
  {
    spatial_legend.style.display = 'None';
  }
  else
    spatial_legend.style.display = 'block';
}

Shiny.addCustomMessageHandler("toggleSL", toggleSL);

function animateBtn1(years)
{
  alert(years);
  for(let i=0; i < 3; i++ )
  {
    Shiny.setInputValue('animateBtn', years[i]);
  }
}
Shiny.addCustomMessageHandler('animateBtn1', animateBtn1);


function showEditMap(map_info)
{
  let em_sm = document.getElementById('admin_em_sm');
  em_sm.style.display = 'None';
  
    // Before you display the manage map container, load the information
  let name = document.getElementById('em_mp_name');
  name.value = map_info.map_name;
  name.disabled = 'disabled';
  
  let url = document.getElementById('em_mp_url');
  url.value = map_info.URL;
  
  let desc = document.getElementById('em_mp_desc');
  desc.value = map_info.new_description;
  
  let syear = document.getElementById('em_mp_sy');
  syear.value = map_info.level;
  
  let eyear = document.getElementById('em_mp_ey');
  eyear.value = map_info.endyear;
  
  Shiny.setInputValue('showIncludedGroups', map_info.map_name+'~@~'+Date.now());
  
  let em_mm = document.getElementById('admin_em_mm');
  em_mm.style.display = 'block';
}
Shiny.addCustomMessageHandler('showEditMap', showEditMap);

function closeEditMap(_)
{
  let em_mm = document.getElementById('admin_em_mm');
  em_mm.style.display = 'None';
  
  let em_sm = document.getElementById('admin_em_sm');
  em_sm.style.display = 'block';
}
Shiny.addCustomMessageHandler('closeEditMap', closeEditMap);
/*
function checkMapChanges(map_info)
{
  
}

function saveMapChanges(map_info)
{
  // Can't write anything until the new dataframe is merged
}
Shiny.addCustomMessageHandler('saveMapChanges', saveMapChanges);

function drag(e){
  e.dataTransfer.setData(“text”,e.target.id);
}

function allowDrop(e){
  // default browser behavior prevents dragging. Hence we need to disable this..
  e.preventDefault();
} */
/*
function drop(e)
{
  e.preventDefault();
  clone = e.target.cloneNode(target);
  let data = e.dataTransfer.getData('text');
  if(clone.id != data)
  {
    
  }
}
*/

function removeRow(rowID)
{
  table = document.getElementById('edit_profiles');
  table.deleteRow(rowID);
}
Shiny.addCustomMessageHandler('removeRow', removeRow);

function tmpAnimate(i)
{
  let mt = -((i-1) * 50 - 9);
  yrsc.style.marginTop = mt + 'px';
}
Shiny.addCustomMessageHandler('tmpAnimate', tmpAnimate);

function resetYearAnSlider(_)
{
  yrsc.style.marginTop = '9px';
}
Shiny.addCustomMessageHandler('resetYearAnSlider', resetYearAnSlider);

function invalid_operation(message)
{
  alert('You have triggered an event ' + message + '. This event will not take effect');
}
Shiny.addCustomMessageHandler('invalid_operation', invalid_operation);

function refresh_page(_)
{
  document.location.reload();
}
Shiny.addCustomMessageHandler('refresh_page', refresh_page);


function toggleNewProf_div(_)
{
  edit_map = document.getElementById('em_profiles');
  new_prof_div = document.getElementById('new_prof_container');
  vis = new_prof_div.style.display;
  if(vis == 'block')
  {
    new_prof_div.style.display = 'none';
    edit_map.style.display = 'block';
  }
  else
  {
    new_prof_div.style.display = 'block';
    edit_map.style.display = 'none';
  }
}
Shiny.addCustomMessageHandler('toggleNewProf_div', toggleNewProf_div);































