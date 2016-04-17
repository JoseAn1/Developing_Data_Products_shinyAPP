library(shiny);library(markdown)

shinyUI(pageWithSidebar(
   
    headerPanel ("Bias Variance Trade Off in Testing MSE"),
 
    sidebarPanel(includeHTML('documentation.html'), 
        
        
                 h3('1- Building the data set for simulation'),
                 h4('x -predictor- values in the Population'),
                 p('Uniform distribution assuption, with this selected limits:'),
                 numericInput('x_min','min value:',-2,step=1,width = 100),
                 numericInput('x_max','max value:',3,step=1,width = 100),
                 
                 h4('y -outcome- values in the Population'),
                 p('y = f(x) + Epsilon, where:'),
                 p('   True function : f(x)= cos (x)'),
                 p('   Epsilon -> N(0,sigma), where sigma is selected:'),
                 numericInput('sigma','sigma:',0.5,step=0.1,width = 100),
                 
                 h4('Sample size and simulation parameters'),
                 p('Selecting the sample size:'),
                 sliderInput('n','sample size',100,min=50,max=300,step=1),
                 p('Samples number to simulate: 50'),
                 p('Training set over the sample percent: 60 %'),
                 
                 h3('2- Statistical Method'),
                 h4('Selected Statistical Method: spline regression')
    ),
    
   
    mainPanel(  h4('Calculated knots number (flexibility) for the best model:'),
                verbatimTextOutput('kso'),
                sliderInput('ks','selected knots number:',1,min=1,max=12,step=1),
                submitButton("Apply Changes"),
                p('When you change any of the assumptions, press this button'),
                plotOutput('g1'),
                plotOutput('g3'),
                plotOutput('g4')
            
        )        
    )
)