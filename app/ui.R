# to install all required packages and dependencies
if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny, tidyverse, stringr, bslib, ggplot2, shinyWidgets, data.table)

# for hot reload in development
# options(shiny.autoreload = TRUE)
# options(shiny.autoreload.pattern = glob2rx("ui.R"))

token <- read.delim("../secrets.txt", header = FALSE)

get_data <- function(as_data, delim){

    url <- "https://dmsc.mind.uci.edu/redcap/api/"
    formData <- list("token"=token,
        content='report',
        format=as_data,
        report_id='3176',
        csvDelimiter=delim,
        rawOrLabel='raw',
        rawOrLabelHeaders='raw',
        exportCheckboxLabel='false',
        returnFormat='json'
    )
    response <- httr::POST(url, body = formData, encode = "form")
    
    if (as_data == "csv"){
        # store data as character string
        result <- httr::content(response, as = "text")
    } else {
        result <- httr::content(response)
    }
    
    return (result)
}

# read csv as textConnection: textConnection is useful when you have data in the form of a character 
# string and you want to read it into R as if it were a file. It provides a way to treat character 
# strings as if they were files and to perform file-like operations on them.
dat <- read.csv(textConnection(get_data('csv', ',')))

# This function will get the Age ranges in our records
get_age_range <- function(){
    age_of_death <- levels(factor(dat$age))

    age_range <- list()
    for (x in age_of_death){
        if (as.numeric(x) >= 90){
            age_range <- append(age_range, "90+")
        }else{
            age_range <- append(age_range, x)
        }
    }
    return(age_range)
}

# This function will get the Gender ranges in our records
get_gender_range <- function(){
    sex <- levels(factor(dat$sex))
    sex_names <- replace(sex, sex == "", "Unknown")

    return(list(sex_names, sex))
}

get_race_range <- function(){
    races <- levels(factor(dat$race))
    races_names <- replace(races, races == "", "Unknown")

    return(list(races_names, races))
}

get_year_range <- function(){
    years <- levels(factor(dat$caseyear))

    return(years)
}

get_pmi_range <- function(){
    pmi <- levels(factor(dat$pmi))
}

remove_x_excess_help <- function(str){
    return(sub("x.*", "", str))
}

get_csf_range <- function(){
    csf <- levels(factor(dat$csf))
    csf <- modify_if(.x = csf, .p=is.character, .f=remove_x_excess_help)
    csf <- csf[!csf == "N/A" & !csf == ""]
    return(csf)
}

get_plasma_range <- function(){
    plasma <- levels(factor(dat$plasma))
    plasma <- plasma[!plasma == "N/A" & !plasma == ""]
    return(plasma)
}

get_serum_range <- function(){
    serum <- levels(factor(dat$serum))
    serum <- serum[!serum == "N/A" & !serum == ""]
    return(serum)
}

get_apoe_range <- function(){
    apoe <- levels(factor(dat$apoe))
    apoe <- c(apoe, "9"[! "9" %chin% apoe])
    # add Unknown value to the list
    return(apoe)
}

get_cdr_range <- function(){
    cdr <- levels(factor(dat$cdr))
    cdr <- cdr[!cdr == "N/A" & !cdr == ""]
    return(cdr)
}

get_cdr_sum_range <- function(){
    cdr_sum <- levels(factor(dat$cdr_sum))
    cdr_sum <- cdr_sum[!cdr_sum == "N/A" & !cdr_sum == ""]
    return(cdr_sum)
}

get_cdr_sum_range <- function(){
    cdr_sum <- levels(factor(dat$cdr_sum))
    cdr_sum <- cdr_sum[!cdr_sum == "N/A" & !cdr_sum == ""]
    return(cdr_sum)
}

get_synd_range <- function(){
    synd <- levels(factor(dat$synd))
    synd <- synd[!synd == "N/A" & !synd == ""]
    return(synd)
}

get_dx_p_range <- function(){
    dx_p_modified <- list()
    dx_p <- levels(factor(dat$dx_p))

    for (x in dx_p){
        if (grepl("][", x, fixed=TRUE)){
            buffer <- strsplit(x, "\\]")

            for (y in buffer){
                dx_p_modified <- append(dx_p_modified, paste(y, "]", sep="")) 
            }
        } else {
            dx_p_modified <- append(dx_p_modified, x)
        }
    }

    dx_p_modified <- unique(dx_p_modified[!dx_p_modified == ""])
    return(dx_p_modified)
}

# converting list to vector
gender_range <- unique(unlist(get_gender_range()[1]))
gender_value <- unique(unlist(get_gender_range()[2]))
race_range <- unique(unlist(get_race_range()[1]))
race_value <- unique(unlist(get_race_range()[2]))
year_range <- unique(unlist(get_year_range()))
pmi_range <- unique(unlist(get_pmi_range()))
age_range <- unique(unlist(get_age_range()))
np_dx_list <- c("Unknown", 
"Normal (no pathology detected)",
"Normal (mild Braak changes)",
"Normal (mild vascular changes)",
"Alzheimer's disease",
"Trisomy 21 (no AD present)",
"Trisomy 21 (AD present)",
"Pick's disease",
"Frontotemporal dementia",
"Huntington's disease",
"Vascular Dementia",
"Vascular Dementia (solitary infarct)",
"Vascular Dementia (multiple infarctions)",
"Vascular Dementia (amyloid angiopathy)",
"Vascular Dementia (amyloid angiopathy)",
"Parkinson's disease",
"Diffuse Lewy body disease",
"Progressive Supranuclear Palsy","Other Parkinsonism",
"Other Parkinsonism", "Hippocampal Sclerosis", "Multiple Sclerosis",
"Infection", "AIDS", "Metabolic Dementia", "Toxic/Drug-induced Dementia",
"Nutritional deficiency (e.g. B12 or ethanol)",
"Neoplasia (e.g. limbic encephalitis)",
"Trauma",
"Corticobasal Degeneration",
"Other: {np_dx_other}",
"None"
)
csf_range <- unlist(get_csf_range())
plasma_range <- unlist(get_plasma_range())
serum_range <- unlist(get_serum_range())
apoe_range <- unlist(get_apoe_range())
apoe_value <- c("e3,e3", "e3,e4", "e3,e2", "e4,e4", "e4,e2", "e2,e2", "Unknown")
cdr_range <- unlist(get_cdr_range())
cdr_sum_range <- unlist(get_cdr_sum_range())
synd_range <- unique(unlist(get_synd_range()))
np_dx_value <- c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,
17,18,19,20,21,22,23,24,25,26,27,28,29,88,99)
dx_p_range <- unique(unlist(get_dx_p_range()))

ui <- fluidPage(

    navbarPage(
    "Tissue Repository Discovery",
    id = "main_navbar"
    ),
    sidebarLayout(position = "left",
        sidebarPanel(
            actionButton("clear_selections", label = "Clear All"),
            downloadLink("export_results", label = "Export"),
                fluidRow(
                    column(8,
                        checkboxInput("age_plot", "Show overall age plot", value = FALSE),
                    ),
                    column(8,
                        checkboxInput("year_plot", "Show yearly plot", value = FALSE),
                    ),
                ),
                inputPanel(
                    checkboxInput("age_incl", "Include age", value = FALSE, width = NULL),
                    fluidRow(
                        HTML("<h5><b>Age range:</b></h5>"),
                        column(6, selectInput(
                        "age_min",
                        "Min",
                        age_range
                    )), 
                        column(6, selectInput(
                        "age_max",
                        "Max",
                        age_range,
                        selected = tail(age_range, 1)
                        ))
                    ),
                    checkboxGroupInput("gender", "Choose sex:",
                        choiceNames = gender_range,
                        choiceValues = gender_value
                    ),
                    checkboxGroupInput("race", "Choose race:",
                        choiceNames = race_range,
                        choiceValues = race_value
                    ),
                    fluidRow(
                        HTML("<h5><b>Choose ethnicity:</b></h5>"),
                        checkboxInput("hispanic", "Hispanic", value = FALSE),
                    ),
                    fluidRow(
                        HTML("<h5><b>Case Years:</b></h5>"),
                        column(6, selectInput(
                        "caseyear_min",
                        "Min",
                        year_range
                    )), 
                        column(6, selectInput(
                        "caseyear_max",
                        "Max",
                        year_range,
                        selected = tail(year_range, 1)
                    ))
                    ),
                    column(6,
                        HTML("<h5><b>Choose Neuropathological:</b></h5>"),
                        dropdown(
                            checkboxGroupInput("np_dx", "List of np_dx:",
                                choiceNames = np_dx_list,
                                choiceValues = np_dx_value
                            ),
                        ),
                        HTML("<h5><b>Choose APOE:</b></h5>"),
                        dropdown(
                            checkboxGroupInput("apoe", "APOE:",
                                    choiceNames = apoe_value,
                                    choiceValues = apoe_range
                            ),
                        ),
                    ),
                    column(6, 
                        HTML("<h5><b>UDS Diagnosis Primary:</b></h5>"),
                        dropdown(
                            checkboxGroupInput("dx_p", "UDS Diagnosis Primary:",
                                    choiceNames = dx_p_range,
                                    choiceValues = dx_p_range
                            ),
                        ),
                        HTML("<h5><b>UDS Syndrome:</b></h5>"),
                        dropdown(
                            checkboxGroupInput("uds_synd", "UDS Syndrome:",
                                    choiceNames = synd_range,
                                    choiceValues = synd_range
                            ),
                        ),
                    ),
                ),
                checkboxInput("pmi_incl", "Include PMI", value = FALSE, width = NULL),
                sliderInput("pmi", "PMI:",
                    min = 0, max = as.numeric(tail(pmi_range, n = 1)),
                    value = c(0, mean(as.numeric(pmi_range))),
                    width = '100%'
                ),
                checkboxInput("csf_incl", "Include CSF", value = FALSE, width = NULL),
                sliderInput("csf", "CSF:",
                    min = 0, max = max(as.numeric(csf_range)),
                    value = c(0, mean(as.numeric(csf_range))),
                    width = "100%"
                ),
                checkboxInput("plasma_incl", "Include Plasma", value = FALSE, width = NULL),
                sliderInput("plasma", "Plasma:",
                    min = 0, max = max(as.numeric(plasma_range)),
                    value = c(0, mean(as.numeric(plasma_range))),
                    width = '100%'
                ),
                checkboxInput("serum_incl", "Include Serum", value = FALSE, width = NULL),
                sliderInput("serum", "Serum:",
                    min = 0, max = max(as.numeric(serum_range)),
                    value = c(0, mean(as.numeric(serum_range))),
                    width = '100%'
                ),
                checkboxInput("cdr_incl", "Include CDR", value = FALSE, width = NULL),
                sliderInput("cdr", "CDR:",
                    min = 0, max = max(as.numeric(cdr_range)),
                    value = c(0, mean(as.numeric(cdr_range))),
                    width = '100%'
                ),
                checkboxInput("sum_incl", "Include CDR Sum", value = FALSE, width = NULL),
                sliderInput("cdr_sum", "CDR Sum:",
                    min = 0, max = max(as.numeric(cdr_sum_range)),
                    value = c(0, mean(as.numeric(cdr_sum_range))),
                    width = '100%'
                ),
        ),
    mainPanel(
        id = "main_panel",
        fluidRow(
            tags$h3("Query Results"),
            column(4,            
                textOutput("age_records"),
                textOutput("gender_records"),
                textOutput("race_records"),
                textOutput("hispanic_records"),
                textOutput("year_records"),
            ),
            column(4,
                textOutput("pmi_records"),
                textOutput("csf_records"),
                textOutput("plasma_records"),
                textOutput("serum_records"),
                textOutput("cdr_records"),
                textOutput("cdr_sum_records"),

            ),
            column(4,
                textOutput("np_dx_records"),
                textOutput("apoe_records"),
                textOutput("dx_p_records"),
                textOutput("uds_synd_records"),
            ),
            ),
        fluidRow(
            tags$h3("Demographics"),
            textOutput("common_records"),
            conditionalPanel(condition = "output.common_cond == true",
                tags$h4("Common Records Demographics:"),
                plotOutput("plot_common_records")
            ),
            conditionalPanel(condition = "output.age_cond == true",
                tags$h4("Age Demographics:"),
                plotOutput("plot_age_count")
            ),
            conditionalPanel(condition = "output.years_cond == true",
                tags$h4("Year Records Demographics:"),
                plotOutput("plot_year_records")
            ),
        ),
    )
    )
)