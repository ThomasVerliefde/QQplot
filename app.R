#
# This is a Shiny web application.
# Find out more about building applications with Shiny
#  here: http://shiny.rstudio.com/
#

# Author: Thomas Verliefde
# Version 1.33
# Date: 2017/10/26

# This app is meant to let you play around with a few different distributions,
#  and check what the effect of different variables (scale, location and shape) is
# The QQ-plots are all a comparison with a default normal distribution (mean=0,sd=1)
# There is a large slider you can use to track specific percentiles on all graphs
# This app was made for as course material.

#-------------#
# QQ-Plot APP #
#-------------#

########################################
#                                      #
#   IF YOU HAVE NOT INSTALLED SHINY,   #
#     PLEASE RUN THE CODE BELOW        #
#         (Press CTRL+Enter)           #
#                                      #
########################################

# Installs necessary packages and loads them
list.of.packages = c("shiny","ggplot2","sn","dplyr")
new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)){install.packages(new.packages,repos="http://cran.us.r-project.org")}
lapply(list.of.packages,require,character.only=T)

####################################
#                                  #
#   IF YOU HAVE INSTALLED SHINY,   #
#     JUST PRESS "Run App"         #
#                                  #
####################################


# If you want to run the app while checking the code,
#  run the code below without '#'

# runApp("QQplot", display.mode = "showcase")



# Define UI
ui <- fluidPage(
  
  tags$style(type="text/css", ".recalculating {opacity:1.0;}"),
  
  #----------#
  # Titelrow #
  #----------#
  
  fluidRow(
    
    #-----------------#
    # Resample Button #
    #-----------------#
    
    column(2,
           
           # Button to resample a sampled distribution
           actionButton(style="position:absolute;
                        font-size:90%;
                        margin-top:10px;
                        margin-bottom:10px;
                        color:darkgrey;",
                        "resample","Resample")
    ),
    
    #--------------------------------------#
    # Tabpanels Density - Cumulative Plots #
    #--------------------------------------#
    
    column(5,
           
           tabsetPanel(
             tabPanel(title="Density"),
             tabPanel(title="Cumulative"),
             id = "PlotTab", type = "pills"
           ),
           style="margin-top:10px;
           margin-bottom:10px;"
    ),
    
    #-------#
    # Titel #
    #-------#
    
    column(5,
           "QQ-Plot",
           style="font-size:150%;
          margin-top:10px;
          margin-bottom:10px;",
           align="left"
    )
  ),
  
  #------------#
  # Data Plots #
  #------------#
  
  fluidRow( 
    
    #---------#
    # Options #
    #---------#
    
    column(2, # Left Column
           
           # Distribution Choice
           selectInput("dist1","Distribution",choices=c("Normal","Skewed-N",
                                                        "Cauchy","Skewed-C")),
           
           # Conditional Sliders for Normal Dist
           conditionalPanel(
             condition = "input.dist1 == 'Normal'",

             # Mean Slider
             sliderInput("M",HTML("&mu;: Location"),min=-10,max=10,value=0,step=.5,ticks=F),
             # Standard Deviation Slider
             sliderInput("SD",HTML("&sigma;: Scale"),min=1,max=10,value=1,step=.5,ticks=F)
           ),

           # Conditional Sliders for Skewed-Normal Dist
           conditionalPanel(
             condition = "input.dist1 == 'Skewed-N'",

             # Xi Slider
             sliderInput("Xi_N",HTML("&xi;: Location"),min=-10,max=10,value=0,step=.5,ticks=F),
             # Omega Slider
             sliderInput("Omega_N",HTML("&omega;: Scale"),min=1,max=10,value=1,step=.5,ticks=F),
             # Alpha Slider
             sliderInput("Alpha_N",HTML("&alpha;: Shape"),min=-5,max=5,value=0,step=.1,ticks=F)
           ),

           # Conditional Sliders for Cauchy Dist
           conditionalPanel(
             condition = "input.dist1 == 'Cauchy'",

             # X0 Slider
             sliderInput("X0",HTML("x<sub>0</sub>: Location"),min=-10,max=10,value=0,step=.5,ticks=F),
             # Gamma Slider
             sliderInput("Gamma",HTML("&gamma;: Scale"),min=1,max=10,value=1,step=.5,ticks=F)
           ),

           # Conditional Sliders for Skewed-Cauchy Dist
           conditionalPanel(
             condition = "input.dist1 == 'Skewed-C'",
             
             # Xi Slider
             sliderInput("Xi_C",HTML("&xi;: Location"),min=-10,max=10,value=0,step=.5,ticks=F),
             # Omega Slider
             sliderInput("Omega_C",HTML("&omega;: Scale"),min=1,max=10,value=1,step=.5,ticks=F),
             # Alpha Slider
             sliderInput("Alpha_C",HTML("&alpha;: Shape"),min=-5,max=5,value=0,step=.1,ticks=F)
           ),
           
           # Checkbox to get sampled distributions
           checkboxInput("sampled","Sampled?",value=F),
           
           # Conditional Slider for Sampled Dist
           conditionalPanel(
             condition = "input.sampled",

             # Sample Size Slider
             sliderInput("N","Sample Size",min=10,max=1000,value=100,step=10,ticks=F)
           )
    ),
    
    #-------------------------------------#
    # Tabs for Density & Cumulative Plots #
    #-------------------------------------#
    
    column(5, # Middle Column
           

           # Depending on what tab(panel) is chosen, displays the relevant plot
           conditionalPanel(
             condition = "input.PlotTab == 'Density'",
             plotOutput("nplot1"),
             align="left"
           ),
           
           conditionalPanel(
             condition = "input.PlotTab == 'Cumulative'",
             plotOutput("cplot1"),
             align="left"
           )
    ),

    #---------#
    # QQ-Plot #
    #---------#
    
    column(5, # Right Column
           
           # Do note that this will always display 99 points
           plotOutput("qqplot"),
           align="left"
    )
  ),
  
  #-----------------------#
  # Percentile Slider Row #
  #-----------------------#
  
  fluidRow( # Large Slider Beneath Top Plots
    
    column(10,offset=2,
           "Percentile",
           style="font-size:75%;",
           sliderInput("percentile",label=NULL,min=0,max=1,value=0,
                       step=.01,ticks=T,width='400px',
                       animate = animationOptions(interval=1000, loop=F)),
           align="left")
    
  ),
  
  #-------------------#
  # Theoretical Plots #
  #-------------------#
  
  fluidRow( 
    
    #---------#
    # Options #
    #---------#
    
    column(2, # This selector has no dependencies, but looks pretty nice
           
           selectInput("dist2","Base Distribution",choices=c("Normal")) 
    ),
    
    #---------------------------#
    # Density & Cumulative Plot #
    #---------------------------#
    
    
    # The plots switch places depending on what non-base plot is shown.
    # This way, you can compare both density and cumulative plots easily
    column(5, 
           
           conditionalPanel(
             condition = "input.PlotTab == 'Density'",
             plotOutput("nplot2")
           ),
           
           conditionalPanel(
             condition = "input.PlotTab == 'Cumulative'",
             plotOutput("cplot2")
           )
    )
  )
)

#---------------------#
# Define server logic #
#---------------------#

server <- function(input, output, session) {
  
  #---------------------#
  # Default Normal Dist #
  #---------------------#
  
   #---------#
   # Density #
   #---------#
  
  Def_M = 0
  Def_SD = 1
  Def_N = 101
  Def_Min = Def_M - 3*Def_SD
  Def_Max = Def_M + 3*Def_SD
  Def_X = reactive(seq(Def_Min,Def_Max,length.out = Def_N))
  Def_Y = reactive(dnorm(Def_X(), mean = Def_M, sd = Def_SD))
  DNorm = reactive(data.frame(x = Def_X(), y = Def_Y()))
  DNormQ = reactive(
    if(input$percentile == 0){
      Def_Min
    }
    else if(input$percentile == 1){
      Def_Max
    }
    else{
      qnorm(input$percentile, mean = Def_M, sd = Def_SD)
    }
  )
  DNormSub = reactive(rbind(c(Def_Min,0),
                            filter(DNorm(), x <= DNormQ()),
                            c(DNormQ(),dnorm(DNormQ(),mean=Def_M,sd=Def_SD)),
                            c(DNormQ(),0)))
   #------------#
   # Cumulative #
   #------------#
  
  Def_YP = reactive(pnorm(Def_X(), mean = Def_M, sd = Def_SD))
  DNormP = reactive(data.frame(x = Def_X(), y = Def_YP()))
  DNormPSub = reactive(rbind(c(Def_Min,0),
                             filter(DNormP(), x <= DNormQ()),
                             c(DNormQ(),pnorm(DNormQ(),mean=Def_M,sd=Def_SD)),
                             c(DNormQ(),0)))
  
  #-------------------#
  # Basic Normal Dist #
  #-------------------#
  
   #---------#
   # Density #
   #---------#
  
  Norm_Min = reactive(input$M-3*input$SD)
  Norm_Max = reactive(input$M+3*input$SD)
  Norm_X = reactive(seq(Norm_Min(),Norm_Max(),length.out = Def_N))
  Norm_Y = reactive(dnorm(Norm_X(), mean = input$M, sd = input$SD))
  Norm = reactive(data.frame(x = Norm_X(), y = Norm_Y()))
  NormQ = reactive(
    if(input$percentile == 0){
      Norm_Min()
    }
    else if(input$percentile == 1){
      Norm_Max()
    }
    else{
      qnorm(input$percentile, mean = input$M, sd = input$SD)
    }
  )
  NormSub = reactive(rbind(c(Norm_Min(),0),
                           filter(Norm(), x <= NormQ()),
                           c(NormQ(),dnorm(NormQ(),mean=input$M,sd=input$SD)),
                           c(NormQ(),0)))
   
   #------------#
   # Cumulative #
   #------------#
   
  Norm_YP = reactive(pnorm(Norm_X(), mean = input$M, sd = input$SD))
  NormP = reactive(data.frame(x = Norm_X(), y = Norm_YP()))
  NormPSub = reactive(rbind(c(Norm_Min(),0),
                            filter(NormP(), x <= NormQ()),
                            c(NormQ(),pnorm(NormQ(),mean=input$M,sd=input$SD)),
                            c(NormQ(),0)))
  
  #--------------------------#
  # Basic Skewed-Normal Dist #
  #--------------------------#
  
   #---------#
   # Density #
   #---------#
  
  Skew_Min = reactive(input$Xi_N-3*input$Omega_N)
  Skew_Max = reactive(input$Xi_N+3*input$Omega_N)
  Skew_X = reactive(seq(Skew_Min(),Skew_Max(),length.out = Def_N))
  Skew_Y = reactive(dsn(Skew_X(), xi = input$Xi_N, omega = input$Omega_N, alpha = input$Alpha_N))
  Skew = reactive(data.frame(x = Skew_X(), y = Skew_Y()))
  SkewQ = reactive(
    if(input$percentile == 0){
      Skew_Min()
    }
    else if(input$percentile == 1){
      Skew_Max()
    }
    else{
      qsn(input$percentile, xi = input$Xi_N, omega = input$Omega_N, alpha = input$Alpha_N)
    }
  )
  SkewSub = reactive(rbind(c(Skew_Min(),0),
                           filter(Skew(), x <= SkewQ()),
                           c(SkewQ(),dsn(SkewQ(),xi=input$Xi_N,omega=input$Omega_N,alpha=input$Alpha_N)),
                           c(SkewQ(),0)))
  
   #------------#
   # Cumulative #
   #------------#
  
  Skew_YP = reactive(psn(Skew_X(),xi=input$Xi_N,omega=input$Omega_N,alpha=input$Alpha_N))
  SkewP = reactive(data.frame(x = Skew_X(), y = Skew_YP()))
  SkewPSub = reactive(rbind(c(Skew_Min(),0),
                            filter(SkewP(), x <= SkewQ()),
                            c(SkewQ(),psn(SkewQ(),xi=input$Xi_N,omega=input$Omega_N,alpha=input$Alpha_N)),
                            c(SkewQ(),0)))
  
  #-------------------#
  # Basic Cauchy Dist #
  #-------------------#
  
   #---------#
   # Density #
   #---------#
  
  Cau_Min = reactive(input$X0-32*input$Gamma)
  Cau_Min2 = reactive(input$X0-3*input$Gamma)
  Cau_Max = reactive(input$X0+32*input$Gamma)
  Cau_Max2 = reactive(input$X0+3*input$Gamma)
  Cau_X = reactive(c(qcauchy(seq(0,1,by=.01),location=input$X0,scale=input$Gamma)))
  Cau_Y = reactive(dcauchy(Cau_X(),location=input$X0,scale=input$Gamma))
  Cau = reactive(data.frame(x = Cau_X(), y = Cau_Y()))
  CauQ = reactive(
    if(input$percentile <= 0){
      Cau_Min()
    }
    else if(input$percentile >= 1){
      Cau_Max()
    }
    else{
      qcauchy(input$percentile, location = input$X0, scale = input$Gamma)
    }
  )
  CauSub = reactive(rbind(c(Cau_Min(),0),
                           filter(Cau(), x <= CauQ()),
                           c(CauQ(),dcauchy(CauQ(),location=input$X0,scale=input$Gamma)),
                           c(CauQ(),0)))
  
   #------------#
   # Cumulative #
   #------------#
  
  Cau_YP = reactive(pcauchy(Cau_X(), location = input$X0, scale = input$Gamma))
  CauP = reactive(data.frame(x = Cau_X(), y = Cau_YP()))
  CauPSub = reactive(rbind(c(Cau_Min(),0),
                            filter(CauP(), x <= CauQ()),
                            c(CauQ(),pcauchy(CauQ(),location=input$X0,scale=input$Gamma)),
                            c(CauQ(),0)))
  
  #--------------------------#
  # Basic Skewed-Cauchy Dist #
  #--------------------------#
  
   #---------#
   # Density #
   #---------#
  
  SCau_Min = reactive(input$Xi_C-33*input$Omega_C)
  SCau_Min2 = reactive(input$Xi_C-3*input$Omega_C)
  SCau_Max = reactive(input$Xi_C+33*input$Omega_C)
  SCau_Max2 = reactive(input$Xi_C+3*input$Omega_C)
  SCau_X = reactive(seq(SCau_Min(),SCau_Max(),length.out = Def_N))
  SCau_Y = reactive(dsc(SCau_X(), xi=input$Xi_C, omega=input$Omega_C, alpha=input$Alpha_C))
  SCau = reactive(data.frame(x = SCau_X(), y = SCau_Y()))
  SCauQ = reactive(
    if(input$percentile <= 0.01){
      SCau_Min()
    }
    else if(input$percentile >= .99){
      SCau_Max()
    }
    else{
      qsc(input$percentile, xi=input$Xi_C, omega=input$Omega_C, alpha=input$Alpha_C)
    }
  )
  SCauSub = reactive(rbind(c(SCau_Min(),0),
                           filter(SCau(), x <= SCauQ()),
                           c(SCauQ(),dsc(SCauQ(),xi=input$Xi_C,omega=input$Omega_C,alpha=input$Alpha_C)),
                           c(SCauQ(),0)))
  
   #------------#
   # Cumulative #
   #------------#
  
  SCau_YP = reactive(psc(SCau_X(),xi=input$Xi_C,omega=input$Omega_C,alpha=input$Alpha_C))
  SCauP = reactive(data.frame(x = SCau_X(), y = SCau_YP()))
  SCauPSub = reactive(rbind(c(SCau_Min(),0),
                            filter(SCauP(), x <= SCauQ()),
                            c(SCauQ(),psc(SCauQ(),xi=input$Xi_C,omega=input$Omega_C,alpha=input$Alpha_C)),
                            c(SCauQ(),0)))

  
  #---------------#
  # Sampled Dists #
  #---------------#
  
  # Here we create a "reactiveValues" object, to store all randomly sampled X values
  # This way, we can utilise a resample button to resample these values
  S = reactiveValues(RNorm_X = reactive(sort(rnorm(n=input$N,mean=input$M,sd=input$SD))),
                     RSkew_X = reactive(sort(rsn(n=input$N,xi=input$Xi_N,
                                                 omega=input$Omega_N,alpha=input$Alpha_N))),
                     RCau_X = reactive(sort(rcauchy(n=input$N,location=input$X0,scale=input$Gamma))),
                     RSCau_X = reactive(sort(rsc(n=input$N,xi=input$Xi_C,
                                                 omega=input$Omega_C,alpha=input$Alpha_C)))
  )
  # This makes it so when you press the resample button (input$resample), everything gets resampled
  observeEvent(input$resample,{
    S$RNorm_X = reactive(sort(rnorm(n=input$N,mean=input$M,sd=input$SD)));
    S$RSkew_X = reactive(sort(rsn(n=input$N,xi=input$Xi_N,omega=input$Omega_N,alpha=input$Alpha_N)));
    S$RCau_X = reactive(sort(rcauchy(n=input$N,location=input$X0,scale=input$Gamma)));
    S$RSCau_X = reactive(sort(rsc(n=input$N,xi=input$Xi_C,omega=input$Omega_C,alpha=input$Alpha_C)))
  })

  #---------------------#
  # Sampled Normal Dist #
  #---------------------#
  
   #---------#
   # Density #
   #---------#
  
  RNorm_Min = reactive(min(S$RNorm_X()))
  RNorm_Max = reactive(max(S$RNorm_X()))
  RNorm_Dens = reactive(density(S$RNorm_X(),
                             n=input$N, 
                             from = RNorm_Min(), 
                             to = RNorm_Max()))
  RNorm = reactive(data.frame(x = RNorm_Dens()$x, y = RNorm_Dens()$y))
  RNormQ = reactive(
    if(input$percentile == 0){
      RNorm_Min()
    }
    else if(input$percentile == 1){
      RNorm_Max()
    }
    else{
      quantile(S$RNorm_X(),input$percentile)
    }
  )
  RNormSub = reactive(rbind(c(RNorm_Min(),0),
                            filter(RNorm(), x <= RNormQ()),
                            c(RNormQ(),density(S$RNorm_X(),n=1,from=RNormQ())$y),
                            c(RNormQ(),0)))
   #------------#
   # Cumulative #
   #------------#
  
  RNorm_YP = reactive(ecdf(S$RNorm_X())(S$RNorm_X()))
  RNormP = reactive(data.frame(x = S$RNorm_X(), y = RNorm_YP()))
  RNormPSub = reactive(rbind(c(RNorm_Min(),0),
                            filter(RNormP(), x <= RNormQ()),
                            c(RNormQ(),ecdf(S$RNorm_X())(RNormQ())),
                            c(RNormQ(),0)))
  
  
  #----------------------------#
  # Sampled Skewed-Normal Dist #
  #----------------------------#
  
   #---------#
   # Density #
   #---------#
  
  RSkew_Min = reactive(min(S$RSkew_X()))
  RSkew_Max = reactive(max(S$RSkew_X()))
  RSkew_Dens = reactive(density(S$RSkew_X(),
                             n=input$N, 
                             from = RSkew_Min(), 
                             to = RSkew_Max()))
  RSkew = reactive(data.frame(x = RSkew_Dens()$x, y = RSkew_Dens()$y))
  RSkewQ = reactive(
    if(input$percentile == 0){
      RSkew_Min()
    }
    else if(input$percentile == 1){
      RSkew_Max()
    }
    else{
      quantile(S$RSkew_X(),input$percentile)
    }
  )
  RSkewSub = reactive(rbind(c(RSkew_Min(),0),
                            filter(RSkew(), x <= RSkewQ()),
                            c(RSkewQ(),density(S$RSkew_X(),n=1,from=RSkewQ())$y),
                            c(RSkewQ(),0)))
  
   #------------#
   # Cumulative #
   #------------#
  
  RSkew_YP = reactive(ecdf(S$RSkew_X())(S$RSkew_X()))
  RSkewP = reactive(data.frame(x = S$RSkew_X(), y = RSkew_YP()))
  RSkewPSub = reactive(rbind(c(RSkew_Min(),0),
                            filter(RSkewP(), x <= RSkewQ()),
                            c(RSkewQ(),ecdf(S$RSkew_X())(RSkewQ())),
                            c(RSkewQ(),0)))
  #---------------------#
  # Sampled Cauchy Dist #
  #---------------------#
  
   #---------#
   # Density #
   #---------#
  
  RCau_Min = reactive(input$X0-33*input$Gamma)
  RCau_Max = reactive(input$X0+33*input$Gamma)
  RCau_Dens = reactive(density(S$RCau_X(),
                                n=input$N, 
                                from = RCau_Min(), 
                                to = RCau_Max()))
  RCau = reactive(data.frame(x = RCau_Dens()$x, y = RCau_Dens()$y))
  RCauQ = reactive(
    if(input$percentile <= 0){
      Cau_Min()
    }
    else if(input$percentile >= 1){
      Cau_Max()
    }
    else{
      quantile(S$RCau_X(),input$percentile)
    }
  )
  RCauSub = reactive(rbind(c(RCau_Min(),0),
                            filter(RCau(), x <= RCauQ()),
                            c(RCauQ(),density(S$RCau_X(),n=1,from=RCauQ())$y),
                            c(RCauQ(),0)))
   #------------#
   # Cumulative #
   #------------#
  
  RCau_YP = reactive(ecdf(S$RCau_X())(S$RCau_X()))
  RCauP = reactive(data.frame(x = S$RCau_X(), y = RCau_YP()))
  RCauPSub = reactive(rbind(c(RCau_Min(),0),
                             filter(RCauP(), x <= RCauQ()),
                             c(RCauQ(),ecdf(S$RCau_X())(RCauQ())),
                             c(RCauQ(),0)))
  
  
  #----------------------------#
  # Sampled Skewed-Cauchy Dist #
  #----------------------------#
  
   #---------#
   # Density #
   #---------#
  
  RSCau_Min = reactive(input$Xi_C-33*input$Omega_C)
  RSCau_Max = reactive(input$Xi_C+33*input$Omega_C)
  RSCau_Dens = reactive(density(S$RSCau_X(),
                                n=input$N, 
                                from = RSCau_Min(), 
                                to = RSCau_Max()))
  RSCau = reactive(data.frame(x = RSCau_Dens()$x, y = RSCau_Dens()$y))
  RSCauQ = reactive(
    if(input$percentile == 0){
      SCau_Min()
    }
    else if(input$percentile == 1){
      SCau_Max()
    }
    else{
      quantile(S$RSCau_X(),input$percentile)
    }
  )
  RSCauSub = reactive(rbind(c(RSCau_Min(),0),
                            filter(RSCau(), x <= RSCauQ()),
                            c(RSCauQ(),density(S$RSCau_X(),n=1,from=RSCauQ())$y),
                            c(RSCauQ(),0)))
  
   #------------#
   # Cumulative #
   #------------#
  
  RSCau_YP = reactive(ecdf(S$RSCau_X())(S$RSCau_X()))
  RSCauP = reactive(data.frame(x = S$RSCau_X(), y = RSCau_YP()))
  RSCauPSub = reactive(rbind(c(RSCau_Min(),0),
                             filter(RSCauP(), x <= RSCauQ()),
                             c(RSCauQ(),ecdf(S$RSCau_X())(RSCauQ())),
                             c(RSCauQ(),0)))
  
  #--------------#
  # QQ-Plot Prep #
  #--------------#
  
  # Create a sequence from .01 to .99 with .01 steps
  # We don't include 0 and 1, as the qnorm, qsn, ... functions give (-)Inf return values
  # These don't play nice with ggplot (mostly warnings)
  perc = seq(0.01,.99,by=.01)
  
  # Create the full quantile range from .01 to .99
  DNormQFull = qnorm(perc, mean = Def_M, sd = Def_SD )
  NormQFull = reactive(qnorm(perc,mean = input$M, sd = input$SD))
  SkewQFull = reactive(qsn(perc,xi=input$Xi_N,omega=input$Omega_N,alpha=input$Alpha_N))
  CauQFull = reactive(qcauchy(perc,location=input$X0,scale=input$Gamma))
  SCauQFull = reactive(qsc(perc,xi=input$Xi_C,omega=input$Omega_C,alpha=input$Alpha_C))
  RNormQFull = reactive(quantile(S$RNorm_X(),perc))
  RSkewQFull = reactive(quantile(S$RSkew_X(),perc))
  RCauQFull = reactive(quantile(S$RCau_X(),perc))
  RSCauQFull = reactive(quantile(S$RSCau_X(),perc))
  
  # Create dataframes with the Default Normal Dist's quantile range and the other Dists' ranges
  NormQQ = reactive(data.frame(x = DNormQFull, y = NormQFull()))
  SkewQQ = reactive(data.frame(x = DNormQFull, y = SkewQFull()))
  CauQQ = reactive(data.frame(x = DNormQFull, y = CauQFull()))
  SCauQQ = reactive(data.frame(x = DNormQFull, y = SCauQFull()))
  RNormQQ = reactive(data.frame(x = DNormQFull, y = RNormQFull()))
  RSkewQQ = reactive(data.frame(x = DNormQFull, y = RSkewQFull()))
  RCauQQ = reactive(data.frame(x = DNormQFull, y = RCauQFull()))
  RSCauQQ = reactive(data.frame(x = DNormQFull, y = RSCauQFull()))
  
  #---------------------#
  # Plot Output Objects #
  #---------------------#
  
  #---------------#
  # Density Plots #
  #---------------#
  
  output$nplot1 = renderPlot({
    
    if(!(input$sampled)){ # Create a theoretical version if checkbox is unticked
      
      if(input$dist1=="Normal"){ # Create a normal distribution density plot
        
        ggplot(data = Norm(), aes(x,y)) +
          geom_line() +
          ylab('') + xlab('') + scale_y_continuous(breaks=F) +
          geom_vline(xintercept = NormQ(),colour="cyan4") +
          geom_polygon(data = NormSub(),x=NormSub()$x,y=NormSub()$y,fill="cyan3",alpha=.4)
        
      }
      else if(input$dist1=="Skewed-N") { # Create a skewed-normal distribution density plot
        
        ggplot(data = Skew(), aes(x,y)) + # Data for the plots
          geom_line() + # Actual density lineplot
          ylab('') + xlab('') + scale_y_continuous(breaks=F) + 
          geom_vline(xintercept = SkewQ(),colour="cyan4") + 
          geom_polygon(data = SkewSub(),fill="cyan3",alpha=.4)  
      }
      else if(input$dist1=="Cauchy") { # Create a cauchy distribution density plot
        
        ggplot(data = Cau(), aes(x,y)) +
          geom_line() +
          ylab('') + xlab('') + scale_y_continuous(breaks=F) +
          geom_vline(xintercept = CauQ(),colour="cyan4") +
          geom_polygon(data = CauSub(),fill="cyan3",alpha=.4)
        
      }
      else if(input$dist1=="Skewed-C") { # Create a skewed-cauchy distribution density plot
       
        ggplot(data = SCau(), aes(x,y)) +
          geom_line() +
          ylab('') + xlab('') + scale_y_continuous(breaks=F) +
          geom_vline(xintercept = SCauQ(),colour="cyan4") +
          geom_polygon(data = SCauSub(),fill="cyan3",alpha=.4)
         
      }
      
    }
    else if(input$sampled){ # Create a sampled version if checkbox is ticked
      
      if(input$dist1=="Normal"){ # Create a sampled normal distribution plot

        ggplot(data = RNorm(), aes(x,y)) +
          geom_line() +
          ylab('') + xlab('') + scale_y_continuous(breaks=F) +
          geom_vline(xintercept = RNormQ(),colour="cyan4") +
          geom_polygon(data = RNormSub(),fill="cyan3",alpha=.4) +
          geom_line(data=Norm(), aes(x,y), colour="firebrick1")
        
      }
      else if(input$dist1=="Skewed-N") { # Create a sampled skewed-normal distribution plot
        
        ggplot(data = RSkew(), aes(x,y)) +
          geom_line() +
          ylab('') + xlab('') + scale_y_continuous(breaks=F) +
          geom_vline(xintercept = RSkewQ(),colour="cyan4") +
          geom_polygon(data = RSkewSub(),fill="cyan3",alpha=.4) +
          geom_line(data=Skew(),aes(x,y), colour="firebrick1")
        
      }
      else if(input$dist1=="Cauchy") { # Create a sampled cauchy distribution plot

        ggplot(data = RCau(), aes(x,y)) +
          geom_line() +
          ylab('') + xlab('') + scale_y_continuous(breaks=F) +
          geom_vline(xintercept = RCauQ(),colour="cyan4") +
          geom_polygon(data = RCauSub(),fill="cyan3",alpha=.4) +
          geom_line(data=Cau(),aes(x,y), colour="firebrick1",alpha=.4)

      }
      else if(input$dist1=="Skewed-C") { # Create  a sampled skewed-cauchy distribution plot

        ggplot(data = RSCau(), aes(x,y)) +
          geom_line() +
          ylab('') + xlab('') + scale_y_continuous(breaks=F) +
          geom_vline(xintercept = RSCauQ(),colour="cyan4") +
          geom_polygon(data = RSCauSub(),fill="cyan3",alpha=.4) +
          geom_line(data=SCau(),aes(x,y), colour="firebrick1", alpha=.4)

      }
    }
  },width=function() {session$clientData$output_nplot1_height*1.0})
  
  #--------------------------#
  # Cumulative Density Plots #
  #--------------------------#
  
  output$cplot1 = renderPlot({ # Create the top right cumulative plot
    
    if(!(input$sampled)){ # Create a theoretical version if checkbox is unticked
      
      if(input$dist1=="Normal"){ # Create a cumulative normal distribution density plot
        
        ggplot(data = NormP(), aes(x,y)) +
          geom_line() +
          ylab('') + xlab('') + scale_y_continuous(breaks=seq(0,1,.25),minor_breaks=seq(0,1,.05)) +
          geom_vline(xintercept = NormQ(), colour="cyan4") +
          geom_hline(yintercept = input$percentile, colour="cyan4")
        
      }
      else if(input$dist1=="Skewed-N") { # Create a cumulative skewed-normal distribution density plot
        
        ggplot(data = SkewP(), aes(x,y)) +
          geom_line() +
          ylab('') + xlab('') + scale_y_continuous(breaks=seq(0,1,.25),minor_breaks=seq(0,1,.05)) +
          geom_vline(xintercept = SkewQ(), colour="cyan4") +
          geom_hline(yintercept = input$percentile, colour="cyan4")

      }
      else if(input$dist1=="Cauchy") { # Create a cumulative cauchy distribution density plot
        
        ggplot(data = CauP(), aes(x,y)) +
          geom_line() +
          ylab('') + xlab('') + scale_y_continuous(breaks=seq(0,1,.25),minor_breaks=seq(0,1,.05)) +
          geom_vline(xintercept = CauQ(), colour="cyan4") +
          geom_hline(yintercept = input$percentile, colour="cyan4")
        
      }
      else if(input$dist1=="Skewed-C") { # Create a cumulative skewed-cauchy distribution density plot
        
        ggplot(data = SCauP(), aes(x,y)) +
          geom_line() +
          ylab('') + xlab('') + scale_y_continuous(breaks=seq(0,1,.25),minor_breaks=seq(0,1,.05)) +
          geom_vline(xintercept = SCauQ(), colour="cyan4") +
          geom_hline(yintercept = input$percentile, colour="cyan4")
        
      }
    }
    else if(input$sampled){ # Create a sampled version if checkbox is ticked
      
      if(input$dist1=="Normal"){ # Create a sampled cumulative normal distribution density plot
        
        ggplot(data = RNormP(), aes(x,y)) +
          geom_line() +
          ylab('') + xlab('') + scale_y_continuous(breaks=seq(0,1,.25),minor_breaks=seq(0,1,.05)) +
          geom_vline(xintercept = RNormQ(), colour="cyan4") +
          geom_hline(yintercept = input$percentile, colour="cyan4") +
          geom_line(data = NormP(), aes(x,y), colour = "firebrick1")
        
      }
      else if(input$dist1=="Skewed-N") { # Create a sampled cumulative skewed-normal dist density plot
        
        ggplot(data = RSkewP(), aes(x,y)) +
          geom_line() +
          ylab('') + xlab('') + scale_y_continuous(breaks=seq(0,1,.25),minor_breaks=seq(0,1,.05)) +
          geom_vline(xintercept = RSkewQ(), colour="cyan4") +
          geom_hline(yintercept = input$percentile, colour="cyan4") +
          geom_line(data= SkewP(),aes(x,y),colour="firebrick1")
        
      }
      else if(input$dist1=="Cauchy") { # Create a sampled cumulative cauchy distribution density plot
        
        ggplot(data = RCauP(), aes(x,y)) +
          geom_line() +
          ylab('') + xlab('') + scale_y_continuous(breaks=seq(0,1,.25),minor_breaks=seq(0,1,.05)) +
          geom_vline(xintercept = RCauQ(), colour="cyan4") +
          geom_hline(yintercept = input$percentile, colour="cyan4") +
          geom_line(data= CauP(),aes(x,y),colour="firebrick1",alpha=.4)
        
      }
      else if(input$dist1=="Skewed-C") { # Create a sampled cumulative skewed-cauchy dist density plot
        
        ggplot(data = RSCauP(), aes(x,y)) +
          geom_line() +
          ylab('') + xlab('') + scale_y_continuous(breaks=seq(0,1,.25),minor_breaks=seq(0,1,.05)) +
          geom_vline(xintercept = RSCauQ(), colour="cyan4") +
          geom_hline(yintercept = input$percentile, colour="cyan4") +
          geom_line(data= SCauP(),aes(x,y),colour="firebrick1",alpha=.4)
        
      }
    }
  },width=function() {session$clientData$output_cplot1_height*1.0})
  
  #---------------------------------#
  # Default Normal Dist OutputPlots #
  #---------------------------------#
  
  output$nplot2 = renderPlot({ # Default Normal Distribution Density Plot
    
    ggplot(data = DNorm(), aes(x,y)) +
      geom_line() +
      ylab('') + xlab('') + scale_y_continuous(breaks=F) +
      geom_vline(xintercept=DNormQ(), colour="cyan4") +
      geom_polygon(data = DNormSub(), aes(x, y), fill="cyan3", alpha=.4)
    
  },width=function() {session$clientData$output_nplot2_height*1.0})
  
  output$cplot2 = renderPlot({ # Default Normal Distribution Cumulative Density Plot
    
    ggplot(data = DNormP(), aes(x,y)) +
      geom_line() +
      ylab("") + xlab('') + scale_y_continuous(breaks=seq(0,1,.25),minor_breaks=seq(0,1,.05)) +
      geom_vline(xintercept = DNormQ(), colour="cyan4") +
      geom_hline(yintercept = input$percentile, colour="cyan4")
    
  },width=function() {session$clientData$output_cplot2_height*1.0})
  
  #---------------------#
  # QQ-Plot OutputPlots #
  #---------------------#
  
  output$qqplot = renderPlot({
    
    if(!(input$sampled)){ # Create a theoretical version if checkbox is unticked
      
      if(input$dist1=="Normal"){ # Create a QQ-plot comparing normal with default
      
        ggplot(data=NormQQ(),aes(x,y)) +
          xlab("Base Distribution") + ylab("Normal") +
          # geom_abline(aes(slope = input$SD, intercept = input$M),colour="firebrick1") +
          geom_vline(xintercept = DNormQ(), colour="cyan4") +
          geom_hline(yintercept = NormQ(), colour="cyan4") +
          geom_point(colour="black",alpha=.4)
      
      }
      
      else if(input$dist1=="Skewed-N"){ # Create a QQ-plot comparing skewed-normal with default

        ggplot(data=SkewQQ(),aes(x,y)) +
          xlab("Base Distribution") + ylab("Skewed-Normal") +
          # geom_abline(aes(slope = sqrt(input$Omega_N ^ 2 * (1 - (2 * (input$Alpha_N/sqrt(1 + input$Alpha_N ^ 2)) ^ 2) / pi)),
          #                 intercept = input$Xi_N + input$Omega_N * (input$Alpha_N/sqrt(1 + input$Alpha_N ^ 2)) * sqrt(2 / pi)),
          #             colour="firebrick1") +
          geom_vline(xintercept = DNormQ(), colour="cyan4") +
          geom_hline(yintercept = SkewQ(), colour="cyan4") +
          geom_point(colour="black",alpha=.4)
          
        
      }
      
      else if(input$dist1=="Cauchy"){ # Create a QQ-plot comparing cauchy with default
        
        ggplot(data=CauQQ(),aes(x,y)) +
          xlab("Base Distribution") + ylab("Cauchy") +
          # geom_abline(aes(slope = input$Gamma, intercept = input$X0),colour="firebrick1") +
          geom_vline(xintercept = DNormQ(), colour="cyan4") +
          geom_hline(yintercept = CauQ(), colour="cyan4") +
          geom_point(colour="black",alpha=.4)
        
      }
      
      else if(input$dist1=="Skewed-C"){ # Create a QQ-plot comparing skewed-cauchy with default
        
        ggplot(data=SCauQQ(),aes(x,y)) +
          xlab("Base Distribution") + ylab("Skewed-Cauchy") +
          # geom_abline(aes(slope = sqrt(input$Omega_C ^ 2 * (1 - (2 * (input$Alpha_C/sqrt(1 + input$Alpha_C ^ 2)) ^ 2) / pi)),
          #                 intercept = input$Xi_C + input$Omega_C * (input$Alpha_C/sqrt(1 + input$Alpha_C ^ 2)) * sqrt(2 / pi)),
          #             colour="firebrick1") +
          geom_vline(xintercept = DNormQ(), colour="cyan4") +
          geom_hline(yintercept = SCauQ(), colour="cyan4") +
          geom_point(colour="black",alpha=.4)
        
      }
      
    }
    else if(input$sampled){ # Create a sampled version if checkbox is ticked
      
      if(input$dist1=="Normal"){ # Create a QQ-plot comparing sampled normal with default
        
        ggplot(data=RNormQQ(),aes(x,y)) +
          xlab("Base Distribution") + ylab("Sampled Normal") +
          # geom_abline(aes(slope = sqrt(input$Omega_N ^ 2 * (1 - (2 * (input$Alpha_N/sqrt(1 + input$Alpha_N ^ 2)) ^ 2) / pi)),
          #                 intercept = input$Xi_N + input$Omega_N * (input$Alpha_N/sqrt(1 + input$Alpha_N ^ 2)) * sqrt(2 / pi)),
          #             colour="firebrick1") +
          geom_vline(xintercept = DNormQ(), colour="cyan4") +
          geom_hline(yintercept = RNormQ(), colour="cyan4") +
          geom_point(colour="black",alpha=.4)
      
      }
      
      else if(input$dist1=="Skewed-N"){ # Create a QQ-plot comparing sampled skewed-normal with default
        
        ggplot(data=RSkewQQ(),aes(x,y)) +
          xlab("Base Distribution") + ylab("Sampled Skewed-Normal") +
          # geom_abline(aes(slope = input$Omega_N, intercept = input$Xi_N),colour="firebrick1") +
          geom_vline(xintercept = DNormQ(), colour="cyan4") +
          geom_hline(yintercept = RSkewQ(), colour="cyan4") +
          geom_point(colour="black",alpha=.4)
        
      }
      
      else if(input$dist1=="Cauchy"){ # Create a QQ-plot comparing sampled cauchy with default
        
        ggplot(data=RCauQQ(),aes(x,y)) +
          xlab("Base Distribution") + ylab("Sampled Cauchy") +
          # geom_abline(aes(slope = input$Gamma, intercept = input$X0),colour="firebrick1") +
          geom_vline(xintercept = DNormQ(), colour="cyan4") +
          geom_hline(yintercept = RCauQ(), colour="cyan4") +
          geom_point(colour="black",alpha=.4)
        
      }
      
      else if(input$dist1=="Skewed-C"){ # Create a QQ-plot comparing sampled skewed-cauchy with default
        
        ggplot(data=RSCauQQ(),aes(x,y)) +
          xlab("Base Distribution") + ylab("Sampled Skewed-Cauchy") +
          # geom_abline(aes(slope = sqrt(input$Omega_C ^ 2 * (1 - (2 * (input$Alpha_C/sqrt(1 + input$Alpha_C ^ 2)) ^ 2) / pi)),
          #                 intercept = input$Xi_C + input$Omega_C * (input$Alpha_C/sqrt(1 + input$Alpha_C ^ 2)) * sqrt(2 / pi)),
          #             colour="firebrick1") +
          geom_vline(xintercept = DNormQ(), colour="cyan4") +
          geom_hline(yintercept = RSCauQ(), colour="cyan4") +
          geom_point(colour="black",alpha=.4)
        
      }
    }
  },width=function() {session$clientData$output_qqplot_height*1.0})
}

# Run the application 
shinyApp(ui = ui, server = server)

