library(shiny)
library(readxl)

#totDf <- read.csv("Data/2021 IMEB Data.csv")

possibleVars <- c("Age","Q2_gender","Q2_gender_other_specify","Q3_race","Q3a_amindalnat","Q3b_asian","Q3c_black","Q3d_hawaiipacisl","Q3e_white","Q3f_other","Q3g_biracial","Q3_raceother_specify","Q4_hispanic","Q6d_degree","Q7a_eddebt","Q7b_housedebt","Q8_learnertype","Q8_learnertype_1","Q8_learnertype_2","Q8_learnertype_3","Q8_learnertype_0","Q9_aftercomp","Q9_fellowship_specify","Q9_other_specify","Q10_visa","Q11a_location","Q11a_instate_outstate","Q11b_facname","Q11b_address","Q11b_city","Q11b_county","Q11b_state","Q11b_zip","Q11b_HPSA","Q11b_MUA","Q11b_rural","Q12_practice","Q12_solo","Q12_group","Q12_inpatient","Q12_outpatient","Q12_ambulatory","Q12_urgent","Q12_managed","Q12_freestanding","Q12_nursing","Q12_other","Q12_other_specify","Q13_accept","Q14","Q14a_always","Q14b_mal","Q14c_cost","Q14d_inclusive","Q14e_jobs","Q14f_opportunity.","Q14g_proxfam","Q14h_proxspouse","Q14i_proxrec","Q14j_mentor","Q14k_rotation","Q14l_salary","Q14m_weather","Q14n_other","Q14n_other_specify","Q15","Q15a_mal","Q15b_cost","Q15c_inadequate","Q15d_inclusive","Q15e_lackjobs","Q15f_never","Q15g_noopp","Q15h_proxfam","Q15i_proxspouse","Q15j_proxrec","Q15k_weather","Q15l_other","Q15l_other_specify","Q16_IN_position","Q17_grossincome","Q18_helpful","Q19a_patient","Q19b_medical","Q19c_practice","Q19d_interpersonal","Q19e_profess","Q19f_system","Q20a_rural","Q20a_underserved","Q20b_rural","Q20b_underserved","Q21a_team","Q21b_quality","Q21c_safety","Q21d_committee","Q21e_cultural","Q21f_disparities","Q22_competent","Q23_quality","Q24a_faculty","Q24b_residents","Q25a_balance","Q25b_burnout","Q25c_meaningful","Q26_haveresources","Q27_wellness")

ui <- fluidPage(

    # Application title
    titlePanel("Research In Medical Education Data Exploration"),

    sidebarLayout(
        sidebarPanel(
            actionButton(inputId = 'import',
                         label = 'Import Single Year Data'),
            selectInput(inputId = "var1",
                        label = "Select the First Variable",
                        choices = possibleVars,
                        selected = possibleVars[1]),
            selectInput(inputId = "var2",
                        label = "Select the Second Variable",
                        choices = possibleVars,
                        selected = possibleVars[2]),
            actionButton(inputId = "runContTbl",
                         label = "Create Contingency Table"),
            actionButton(inputId = 'runChiSq',
                         label = 'Run Chi Square Test'),
            
            selectInput(inputId = 'df',
                        label = 'Select Data to Normalize',
                        choices = c('chnAge','chnRace','chnGender','chnGI'),
                        selected = 'chnAge'),
            actionButton(inputId = 'runNorm',
                         label = 'Normalize Table'), 
            actionButton(inputId = 'update',
                        label = 'Get data ready for next year'),
            selectInput(inputId = 'stat',
                        label = 'Which Descriptive statistic?',
                        choices = c('total','average')),
            actionButton(inputId = "descs",
                         label = "Get Descriptive Statistics")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tableOutput('contTable'),
            br(), hr(), br(),
            tableOutput('xsq'),
            br(), hr(), br(),
            tableOutput('normalized'),
            br(), hr(), br(),
            tableOutput('updated'),
            br(), hr(), br(),
            tableOutput('descStats'),
            br(), hr(), br(),
            tableOutput('contents')
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    cont <- eventReactive(input$import, {
        totDf <<- read.csv("C:/Users/agbre/OneDrive - Indiana University/Desktop/GradSchool/YearTwo/PBHL B581/Final Project/Data/2021 IMEB Data.csv")
        #totDf[] <- lapply(totDf, function(x) as.numeric(as.character(x)))
    })
    
    ct <- eventReactive(input$runContTbl, {
        v1 <- totDf[names(totDf)==input$var1]
        v2 <- totDf[names(totDf)==input$var2]
        contTbl(df = totDf, var1 = v1, var2 = v2)
    })
    
    x2 <- eventReactive(input$runChiSq, {
        v1 <- totDf[names(totDf)==input$var1]
        v2 <- totDf[names(totDf)==input$var2]
        chiSqTest(df = totDf, var1 = v1, var2 = v2)
    })
    
    norm <- eventReactive(input$runNorm, {
        dat <- get(input$df)
        res <- normalize(dat)
        res$Normalized[1]
    })
    
    upd <- eventReactive(input$update, {
        dat <- get(input$df)
        addYear(dat)
    })
    
    descs <- eventReactive(input$descs, {
        dat <- get(input$df)
        descStats(dat, stat = input$stat)
    })
    
    output$contTable <- renderTable({
        ct()
    })
    
    output$normalized <- renderTable({
        norm()
    })
    
    output$updated <- renderTable({
        upd()
    })
    
    output$descStats <- renderTable({
        descs()
    })
    
    output$contents <- renderTable({
        cont()
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
