options(scipen=999)
loremipsum <- "Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet."


boxwidth <- 4


exposureProfilesExample <- data.frame(exposureprofiles=c("apple_R1_pond_01.txt","apple_R2_stream_01.txt","cereal_D1_ditch_01.txt","cereal_D1_stream_01.txt"))

paraDescTable <- read.table("inst/app/www/paraDesc.csv", sep=";", header=T, quote="\"", check.names=FALSE)


simsettings=list(
  y0_1=0.924,
  y0_2=0,
  y0_3=1,
  y0_4=0,
  ECpct=10,
  windowLength=21,
  thinning=TRUE,
  cutOff=1000,
  passFail=10,
  detail=TRUE,
  interval=1,# unit: days
  padDt=1# unit: days
)




initValues <- list(
  daphnia_debkiss=list(
    Meta = list(Name = "Pesticide-A D.magna calibrated",
             Species = "Daphnia magna",
             Compound = "Pesticide-A",
             Model="",
             Length = "cm",
             Time = "d",
             Concentration = "mg/L"),
    `Phys. parameters` = list(f = 1.0,
                           rB = 0.1305,
                           Lp = 2.627,
                           Lm = 3.834,
                           Rm = 14.76),
    `Survival dynamics` = list(b_s = 0.0,#0.000772,
                            z_s = 100000.0,#478.6,
                            h_b = 0.000),
    `Sublethal dyanamics` = list(z_b = 95.92,
                              b_b = 0.03297),
    `Kinetics` = list(kd = 1.107,
                   Xu = TRUE,
                   Xe = FALSE,
                   XG = FALSE,
                   XR = FALSE,
                   Lm_TK = 5),#3.834), 
    `Mode(s) of actions` = list(SA = FALSE,
                             SM = FALSE,
                             SG = FALSE,
                             SR1 = FALSE,
                             SR2 = TRUE),
    `Advanced parameters` = list(F_BV = 0.007,
                              K_RV = 1.0,
                              yP = 0.64,
                              kap = 0.8),
    `Simulation settings` = simsettings
  )# end of daphnia
)# end of initValues


ignorelist_params <- c("y0_2","y0_3", "y0_4", "b_s", "z_s", "h_b")

# The order of these parameters is important as these define the order in which
# the parameters are entering the C-code are are mapped according to their order
# to variable names
parameternames <- list( 
  daphnia_debkiss=c("f",
                    "rB",
                    "Lp",
                    "Lm",
                    "Rm",
                    "kap",
                    "F_BV",
                    "K_RV",
                    "b_s",
                    "z_s",
                    "h_b",
                    "z_b",
                    "b_b",
                    "kd",
                    "Xu",
                    "Xe",
                    "XG",
                    "XR",
                    "SA",
                    "SM",
                    "SG",
                    "SR1",
                    "SR2",
                    "yP",
                    "Lm_TK"
                    )
)


activatablePars <- data.frame(parameter=c("Lm_TK"), fallbackVal=c("Lm"))

selModel <- shiny::reactiveVal("daphnia_debkiss")


# custom colours ####
customColours <- list(sBlue = "#000066",
                      sGreen = "#019934",
                      rGreenLight = "#aebb77",
                      rGreenDark = "#849a2f",
                      darkBlue = "#1881c2",
                      lightBlue = "#c8e7f9"
                      )
