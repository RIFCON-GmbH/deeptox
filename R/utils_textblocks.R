#' Generate standard texts
#' 
#' A list with longer textblocks that can be modified conveniently
#' here and inserted throughout the app by subsetting the list with the appropriate
#' list element name.
#'
#' @return a named list
#'
#' @examples
#' textblocks()[["parameterBoxIntro"]]
#' @noRd
textblocks <- function(){
  list(
  
  # Box text blocks; both direct text in box and info button text ####

  ## parameterBoxIntro ####
  parameterBoxIntro = "The parameter set for a species and compound can be loaded 
  from a file or entered manually and saved for future use. The parameter values
  determine the simulated rates of normal growth and reproduction for the species
  as well the effects of exposure to a given chemical. Hover the mouse over the
  info button for full parameter names or click for more detailed information.",
  
  ## physParaBoxInfo ####
  physParaBoxInfo = "The parameters for the physiological model describe the 
  rates of growth (cm/day) and reproduction (number of offspring/day) in the control 
  treatment.<br>
  <b>NOTE:</b> Lengths should be entered in cm.",
  
  ## kineticsBoxInfo ####
  kineticsBoxInfo = "The kinetics model describes the accrual and repair of
  'scaled damage' as a result of exposure to chemical over time. Scaled damage
  is an abstract variable, linking chemical exposure to physiological stress, it
  has the same units as external concentration.<br>
  <b>NOTE:</b> If your exposure profile has different time units to the 
  calibration data, then the rate constant kd will need to be adjusted accordingly
  (e.g. to convert from hour<sup>-1</sup> to day<sup>-1</sup>, kd must be divided by 24).",
  
  ## sublethDynamicsBoxInfo ####
  sublethDynamicsBoxInfo = paste0("These parameters describe the relationship between 
  scaled damage and (dimensionless) stress on the physiological model. The 
  'mode of action' determines which physiological processes are affected.<br>
  <b>NOTE:</b> Make sure the concentration units in the exposure profile match 
  the data used in calibration. Otherwise, the value of <i>zb</i> and <i>bb</i> will 
  need to be adjusted accordingly (e.g. if the exposure profile is in ",mu_char(),"g/L and 
  the calibration data were in mg/L, then <i>zb</i> must be multiplied by 1000 
  and <i>bb</i> must be divided by 1000)."),
  
  ## survDynamicsBoxInfo ####
  survDynamicsBoxInfo = "If survival is included in model predictions, these 
  parameters describe the relationship between scaled damage and survival hazard. 
  Survival effects are predicted using the GUTS-SD model. If survival data were 
  not used in model calibration, then the survival parameters should be left at 
  their default values (which result in no effects being predicted).",

  ## pMoABoxInfo ####
  pMoABoxInfo = "In this section the mode of action for the chemical should be 
  selected. This should be known from model calibration.",
  
  ## advParametersBoxInfo ####
  advParametersBoxInfo = "These parameters are not directly measurable and are 
  challenging to estimate so it is recommended they are kept at their default 
  values unless they have been estimated by the user. <i>FBV</i> is measurable but is 
  only used if <i>XR</i> is selected as a feedback in the kinetics module.",
  
  ## movingTimewindowBoxInfo ####
  movingTimewindowBoxInfo = "DeEP evaluates each exposure profile using the 
  'moving time window' approach (Sherborne et al., 2020). This means that the 
  whole profile is broken down into overlapping windows of a fixed length and 
  predictions are made for each window separately. Each simulation can be thought
  of as a 'virtual toxicity test' or VTT. A simple example would be a 6-day 
  exposure profile with a 3-day time window moving forward in 1-day timesteps 
  (Fig. 1). This would break down into four 3-day windows, with consecutive 
  windows having two days of overlap.<br><br>
  
  <img src='www/mtw.png' alt='moving time window example' width='400'><br>
  <b>Fig. 1</b> Diagram showing how a 6-day exposure profile would be padded with
  zeros and broken into eight overlapping windows of three days each.<br><br>
    The length of the window should be decided based on the specific question being
  approached. For example, this may be the duration of laboratory experiments 
  used for model calibration, the length of the species' most vulnerable life 
  stage, or the entire lifespan of the species. <br><br>
  The software also pads the exposure profile with zeros, such that the first 
  window only includes exposure in the last time step. The same number of zeros 
  are added to the end of the profile. This ensures that all possible combinations 
  of age and concentration are considered. In the example above, this would mean 
  that the number of time windows would increase from four to eight. The first 
  window would cover days -2 to 1 and the last would cover days 5 to 8.<br><br>
  <hr>
  Sherborne, N., Galic, N., Ashauer, R., 2020. Sublethal effect modelling for 
  environmental risk assessment of chemicals: Problem definition, model variants,
  application and challenges. Sci. Total Environ. 745, 141027. 
  https://doi.org/10.1016/j.scitotenv.2020.141027
  ",
  
  ## expProfilesBoxIntro ####
  expProfilesBoxIntro = "Here, the exposure profiles can be loaded in from a 
  csv file (comma separated) or txt file (tab separated). The profile should be 
  separated into two columns with no headers. Column 1 should contain the time 
  in days while column 2 should contain the chemical concentration at each timepoint. 
  Once the profiles are loaded, these can be selected to visualise the data on a plot.<br>
  <b>NOTE:</b> The time and concentration units of the exposure profile(s) should match those
  used in the calibration dataset. Otherwise certain parameter values must be updated
  accordingly (see manual).",
  
  ## resultsBoxIntro ####
  resultsBoxIntro = "Click 'Run exposure assessment' to calculate the EP<sub>x</sub> multiplier
  for all exposure profiles loaded above, the results will be summarised in a table. Click on
  any row of the results table to view the results for a single profile on a plot.
  For more information on how to interpret the results table and plots, click the info button.
  <br>
  To export all results into a word document, click 'Generate report'. This report
  gives a summary of the results and explains what they mean in a risk assessment
  context.",
  
  ## resultsBoxInfo ####
  resultsBoxInfo = "<h4>Summary Results Table</h4>
  The software returns a summary of results in a table format. Each row represents
    one of the exposure profiles tested. The columns are as follows:
    <ul>
      <li><b>Profile names</b> The name assigned to each exposure profile</li>
      <li><b>Pass/Fail</b> The result for the whole exposure profile</li>
      <li><b>EP<sub>x</sub></b> The lowest EP<sub>x</sub> multiplier predicted for any time window within the exposure profile</li>
      <li><b>Critical endpoint</b> The endpoint for which the lowest EP<sub>x</sub> was derived (survival, growth or reproduction)</li>
      <li><b>Total no. of VTTs</b> The total number of virtual toxicity tests (VVTs, i.e. the total number of time windows)</li>
      <li><b>No. of VTTs thinned</b> The number of VTTs excluded from analysis by thinning</li>
      <li><b>No. of VTTs tested</b> The number of VTTs conducted after thinning</li>
  </ul>
  
  <br>
  <h4>Results Plot</h4>
  The results for each individual profile can be visualised in a plot by selecting
  the corresponding row of the results table. The black line shows the lowest EP<sub>x</sub>
  multiplier predicted for any endpoint in each time window vs the start time of
  that time window. The user-specified pass/fail threshold is shown as a horizontal
  red line. If the black line drops below the threshold at any point, this 
  represents a failure to meet the risk assessment criterion. Values above the 
  Cutoff are not plotted.
  <br>
   <img src='www/resultsEx.png' alt='results example plot' width='400'> ",
  
  ## expProfilesCustomImport ####
  expProfilesCustomImport = "Upload a plain text file with two columns and no
  header:<br>&nbsp;&nbsp; <b>1st column:</b> time in days<br>&nbsp;&nbsp;
  <b>2nd column:</b> external concentration<br><br>If file is of 'csv'-type,
  comma-separated data is expected.<br>Files of 'txt'-type are expected to be
  tab-separated data.",
  
  ## expProfilesExampleImport ####
  expProfilesExampleImport = "Four example profiles from the 
  <a href='https://openguts.info/download.html' target='_blank'>openGUTS project</a>.",
  
  ## expProfilesToxswaImport ####
  expProfilesToxswaImport = "Profiles that can be directly loaded from the 
  <a href='https://esdac.jrc.ec.europa.eu/projects/toxswa' target='_blank'>TOXSWA</a> 
  out-files. So far only TOXSWA 4 and 5.5.3 are supported. The unit in which TOXSWA measures the concentration is g/m<sup>3</sup> or mg/L.
  All parameters should be used accordingly.",
  
  ## warningInputChanged ####
  warningInputChanged = HTML(paste0(
    "<div class=\"text-danger\">
        <b>Input data (parameters, settings or exposure profiles) changed after the results were created.</b><br>
        The results will be exported with the input data that was used to create them.<br></div>"
  )),
  
  ## debtheory info box ####
  debtheoryBoxInfo = HTML(paste0(
    "<h3>The DEBkiss physiological model</h3>
    Here, a simple overview of the DEBkiss physiological model is presented.
    This summary is highly simplified and more detailed information is available
    in the scientific literature  (Jager, 2020; Jager et al., 2013).<br><br>
    
    The DEBkiss model describes the assimilation of energy from food and its
    subsequent allocation to various organismal process. Assimilates from food are
    initially divided into two branches. The first, the somatic branch, includes
    energy for maintenance of existing body mass (structure) and the growth of new
    structure. The proportion of available assimilates allocated to the somatic
    branch is denoted by the Greek letter kappa (",kappa_char(),"). The other branch, the
    reproductive branch, includes the energy allocation to sexual maturity and
    reproduction. The fraction allocated to the reproductive branch is denoted (1-",kappa_char(),").
    The model is depicted in Fig. 1 below.<br><br>
    
    <img src=\"www/debtheory_fig1.png\" alt=\"debtheory fig1\" width='400'><br>
    <b>Fig. 1</b> Schematic showing the allocation of assimilates from food in a sexually
    mature female in the DEBkiss model. For juveniles, the 1-",kappa_char()," branch is not explicitly
    followed as there is no reproduction. Instead, the allocated energy is assumed to be
    used for maturation.<br><br>
    
    Somatic maintenance and maturity maintenance are essential processes, for
    survival and maintaining sexual maturity, respectively. As such, they take
    priority over the processes of growth and reproduction if resources become
    limited. Additionally, reproduction begins at a critical body size and
    reproductive rate increases with body size thereafter. The ultimate reproductive
    endpoint is the cumulative number of offspring. For some species this may be
    counted as the number of eggs while for others, offspring are counted after
    hatching. The cumulative number of hatched offspring depends on both the 
    mother’s egg production and survival rates to hatching/birth.

    <h3>Stress on physiological processes</h3>
    Exposure to a chemical leads to the accrual of internal 'damage' which is also
    repaired at a certain rate. Under constant exposure, the rates of damage accrual
    and repair reach equilibrium, so that the overall damage level also remains constant.
    Damage leads to 'stress' which can be applied to one or more processes in the physiological
    model. The various processes are referred to as 'physiological modes of
    action' (pMoAs) and each may affect the rates of growth and/or reproduction:
    <ul>
    <li>Stress on assimilation (Fig. 2A) reduces the total energy available. Maintenance rates are unaffected, so the rates of growth and reproduction are both reduced. Maximum body size is also reduced.</li>
    <li>Stress on maintenance (Fig. 2B) results in increased somatic maintenance and maturity maintenance (which is calculated as a function of somatic maintenance). As a result, less energy is available for growth and reproduction. Maximum body size is reduced.</li>
    <li>Stress on growth (Fig. 2C) makes growth a more energetically expensive process and results in reduced growth rate. However, maximum body size is unaffected. There are no effects on reproductive rate at a given body size. However, as reproduction is linked to body size, cumulative reproduction over time will also decrease.</li>
    </ul>
    <br><br>
    
    <img src=\"www/debtheory_fig2.png\" alt=\"debtheory fig2\" width='550'><br>
    <b>Fig. 2</b> Illustration of the impacts of stress on assimilation (A), maintenance (B), and growth (C). The direct impact of the stress in each pMoA is shown by a large red arrow while any indirect impacts on other processes are shown by smaller red arrows.<br><br>
    
    
    Stress may also directly affect reproductive processes:
    <ul>
      <li>Stress on reproduction (Fig. 3A) makes the production of eggs a more expensive process, leading to reduced reproduction with no impact on growth processes whatsoever. </li>
      <li>Stress on egg formation or embryo survival (Figure 3B) leads to decreased viable egg production and/or hatching/birth rates. This results in reduced reproductive output in terms of offspring without any effects on growth or egg production.</li>
    </ul>
    <br><br>
    
    <img src=\"www/debtheory_fig3.png\" alt=\"debtheory fig3\" width='550'><br>
    <b>Fig. 3</b> Illustration of the impacts of stress on reproduction(A), and embryo survival (B). The direct impact of the stress in each pMoA is shown by a large red arrow while any indirect impacts on other processes are shown by smaller red arrows.
    <br><br>
    <hr>
    Jager, T., 2020. Revisiting simplified DEBtox models for analysing ecotoxicity data. Ecol. Model. 416, 108904. https://doi.org/10.1016/j.ecolmodel.2019.108904
    <br><br>
    Jager, T., Martin, B.T., Zimmer, E.I., 2013. DEBkiss or the quest for the simplest generic model of animal life history. J. Theor. Biol. 328, 9-18. https://doi.org/10.1016/j.jtbi.2013.03.011

    "
  ))
  
)
}

