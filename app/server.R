# for hosting locally
options(shiny.host = "0.0.0.0")
options(shiny.port = 5000)
source("ui.R", local = TRUE)

server <- function(input, output, session) {

    observe({
        updateCheckboxGroupInput(session, "gender",
                                choiceNames = gender_range,
                                choiceValues = gender_value,
                                selected = input$gender_add)
    })

    observe({
        updateCheckboxGroupInput(session, "gender",
                                choiceNames = gender_range,
                                choiceValues = gender_value,
                                selected = input$gender_clinical)
    })

    observe({
        updateCheckboxGroupInput(session, "race",
                                choiceNames = race_range,
                                choiceValues = race_value,
                                selected = input$race_add)
    })

    observe({
        updateCheckboxGroupInput(session, "race",
                                choiceNames = race_range,
                                choiceValues = race_value,
                                selected = input$race_clinical)
    })

    # clear button
    observe({

        if (input$clear_selections > 0){

            updateCheckboxInput(session, "age_incl",
                                    value = FALSE)
            updateCheckboxGroupInput(session, "gender",
                                    choices = gender_range,
                                    selected = NULL)
            updateCheckboxGroupInput(session, "race",
                                    choices = race_range,
                                    selected = NULL)
            updateCheckboxInput(session, "age_plot",
                                    value = FALSE)
            updateCheckboxInput(session, "year_plot",
                                    value = FALSE)
            updateCheckboxInput(session, "hispanic",
                                    value = FALSE)
            updateCheckboxInput(session, "pmi_incl",
                                    value = FALSE)
            updateCheckboxInput(session, "csf_incl",
                                    value = FALSE)
            updateCheckboxInput(session, "plasma_incl",
                                    value = FALSE)
            updateCheckboxInput(session, "serum_incl",
                                    value = FALSE)
            updateCheckboxInput(session, "cdr_incl",
                                    value = FALSE)
            updateCheckboxInput(session, "sum_incl",
                                    value = FALSE)
            updateCheckboxGroupInput(session, "np_dx",
                                    choices = np_dx_list,
                                    selected = NULL)
            updateCheckboxGroupInput(session, "apoe",
                                    choices = apoe_value,
                                    selected = NULL)
            updateCheckboxGroupInput(session, "dx_p",
                                    choices = dx_p_range,
                                    selected = NULL)
            updateCheckboxGroupInput(session, "uds_synd",
                                    choices = synd_range,
                                    selected = NULL)
        }

    })

    get_age <- reactive({

        result <- get_data('json', '')

        if (input$age_max != "90+"){
            low <- as.numeric(input$age_min)
            high <- as.numeric(input$age_max)
        }else{
            if (input$age_min != "90+"){
                low <- as.numeric(input$age_min)
                high <- Inf
            }else {
                low <- 90
                high <- Inf
            }
        }
        age_records <- data.frame(matrix(ncol = 2, nrow = 0))
        age_unknown_records <- data.frame(matrix(ncol = 2, nrow = 0))
        colnames(age_records) <- c("record_id", "age")
        colnames(age_unknown_records) <- c("record_id", "age")

        for (x in result){

            if (x$age != "" && (as.numeric(x$age) >= low) && (as.numeric(x$age) <= high)){
                age_records[nrow(age_records) + 1,] = c(x$record_id,
                x$age)
            }else if (x$age == "") {
                age_unknown_records[nrow(age_unknown_records) + 1,] = c(x$record_id, "NA")
            }
        }

        age_response_list <- list(age_records, low, high, age_unknown_records)
        return(age_response_list)
    
    })

    get_age_count <- reactive({

        age_records <- get_age()[1][[1]]
        
        age_list <- c("0-10",
                    "10-20",
                    "20-30",
                    "30-40",
                    "40-50",
                    "50-60",
                    "60-70",
                    "70-80",
                    "80-90",
                    "90+")

        age_count_records <- data.frame(matrix(ncol = 2, nrow = 0))
        colnames(age_count_records) <- c("input_range", "count")

        if(nrow(age_records) > 0){

            for (x in age_list){
                if (length(x) > 0){
                    if (x != "90+") {
                        low_limit <- str_split(x, "-")
                        high_limit <- str_split(x, "-")
                        low <- as.numeric(unlist(low_limit[[1]][1]))
                        high <- as.numeric(unlist(high_limit[[1]][2]))
                        count <- sum(as.numeric(age_records$age) >= low & as.numeric(age_records$age) <= high)
                    }else {
                        low <- 90
                        high <- Inf
                        count <- sum(as.numeric(age_records$age) >= low & as.numeric(age_records$age) <= high)

                    }
                    age_count_records[nrow(age_count_records) + 1,] = c(x,
                    count)
                }
            }
            return(age_count_records)
        }
    })

     get_gender_count <- reactive({

        gender_records <- get_age()[1][[1]]

        gender_count_records <- data.frame(matrix(ncol = 2, nrow = 0))
        colnames(age_count_records) <- c("gender", "count")

        for (x in input$age){
            if (length(x) > 0){
                if (x != "90+") {
                    low_limit <- str_split(x, "-")
                    high_limit <- str_split(x, "-")
                    low <- as.numeric(unlist(low_limit[[1]][1]))
                    high <- as.numeric(unlist(high_limit[[1]][2]))
                    count <- sum(age_records$age > low & age_records$age < high)
                }else {
                    low <- 90
                    high <- Inf
                    count <- sum(age_records$age > low & age_records$age < high)

                }
                age_count_records[nrow(age_count_records) + 1,] = c(x,
                count)
            }
        }
        return(age_count_records)

    })

    get_gender <- reactive({

        genders <- input$gender
        genders <- replace(genders, genders == "Unknown", "")
        result <- get_data('json', '')

        gender_records <- data.frame(matrix(ncol = 2, nrow = 0))
        colnames(gender_records) <- c("record_id", "sex")

        for (x in result){ 
            if (x$sex %in% genders){
                gender_records[nrow(gender_records) + 1,] = c(x$record_id,
                x$sex)
            }
        }
        gender_response_list <- list(gender_records, genders)
        return(gender_response_list)

    })

    get_race <- reactive({

        races <- input$race
        races <- replace(races, races == "Unknown", "")
        result <- get_data('json', '')

        race_records <- data.frame(matrix(ncol = 2, nrow = 0))
        colnames(race_records) <- c("record_id", "race")

        for (x in result){
            if (x$race %in% races){
                race_records[nrow(race_records) + 1,] <- c(x$record_id,
                x$race)
            }
        }
        race_response_list <- list(race_records, races)

        return(race_response_list)

    })

    get_year <- reactive({

        years_min <- input$caseyear_min
        years_max <- input$caseyear_max
        result <- get_data('json', '')

        year_records <- data.frame(matrix(ncol = 3, nrow = 0))
        colnames(year_records) <- c("record_id", "patient_id", "caseyear")

        for (x in result){
            if (length(years_min) > 0 && length(years_max) > 0) {
                if (x$caseyear != "" && (x$caseyear >= years_min && x$caseyear <= years_max)) {
                    year_records[nrow(year_records) + 1,] <- c(x$record_id, x$patientid, x$caseyear)
                }
            }

        }
        year_response_list <- list(year_records, years_min, years_max)

        return(year_response_list)

    })

    get_hispanic <- reactive({
        
        result <- get_data('json', '')

        hisp_records <- data.frame(matrix(ncol = 2, nrow = 0))
        colnames(hisp_records) <- c("record_id", "hispanic")

        for (x in result){
            if (input$hispanic) {
                if (x$hispanic == "Yes") {
                    hisp_records[nrow(hisp_records) + 1,] <- c(x$record_id,
                    x$hispanic)
                }
            }

        }
        return(hisp_records)
    })
    
    get_pmi <- reactive({

        pmi_min <- input$pmi[1]
        pmi_max <- input$pmi[2]
        result <- get_data('json', '')

        pmi_records <- data.frame(matrix(ncol = 2, nrow = 0))
        colnames(pmi_records) <- c("record_id", "pmi")

        for (x in result){
            if (x$pmi >= pmi_min && x$pmi <= pmi_max) {
                pmi_records[nrow(pmi_records) + 1,] <- c(x$record_id, x$pmi)
            }

        }
        pmi_response <- list(pmi_records, pmi_min, pmi_max)

        return(pmi_response)
    })

    get_csf <- reactive({

        csf_min <- input$csf[1]
        csf_max <- input$csf[2]
        result <- get_data('json', '')

        csf_records <- data.frame(matrix(ncol = 2, nrow = 0))
        colnames(csf_records) <- c("record_id", "csf")

        for (x in result){
            if (x$csf >= csf_min && x$csf <= csf_max) {
                csf_records[nrow(csf_records) + 1,] <- c(x$record_id, x$csf)
            }

        }
        csf_response <- list(csf_records, csf_min, csf_max)

        return(csf_response)
    })

    get_plasma <- reactive({

        plasma_min <- input$plasma[1]
        plasma_max <- input$plasma[2]
        result <- get_data('json', '')

        plasma_records <- data.frame(matrix(ncol = 2, nrow = 0))
        colnames(plasma_records) <- c("record_id", "plasma")

        for (x in result){
            if (x$plasma >= plasma_min && x$plasma <= plasma_max) {
                plasma_records[nrow(plasma_records) + 1,] <- c(x$record_id, x$plasma)
            }

        }
        plasma_response <- list(plasma_records, plasma_min, plasma_max)

        return(plasma_response)
    })

    get_serum <- reactive({

        serum_min <- input$serum[1]
        serum_max <- input$serum[2]
        result <- get_data('json', '')

        serum_records <- data.frame(matrix(ncol = 2, nrow = 0))
        colnames(serum_records) <- c("record_id", "serum")

        for (x in result){
            if (x$serum >= serum_min && x$serum <= serum_max) {
                serum_records[nrow(serum_records) + 1,] <- c(x$record_id, x$serum)
            }

        }
        serum_response <- list(serum_records, serum_min, serum_max)

        return(serum_response)
    })
    
    get_cdr <- reactive({

        cdr_min <- input$cdr[1]
        cdr_max <- input$cdr[2]
        result <- get_data('json', '')

        cdr_records <- data.frame(matrix(ncol = 2, nrow = 0))
        colnames(cdr_records) <- c("record_id", "cdr")

        for (x in result){
            if (x$cdr >= cdr_min && x$cdr <= cdr_max) {
                cdr_records[nrow(cdr_records) + 1,] <- c(x$record_id, x$cdr)
            }

        }
        cdr_response <- list(cdr_records, cdr_min, cdr_max)

        return(cdr_response)
    })

    get_cdr_sum <- reactive({
        
        cdr_sum_min <- input$cdr_sum[1]
        cdr_sum_max <- input$cdr_sum[2]
        result <- get_data('json', '')

        cdr_sum_records <- data.frame(matrix(ncol = 2, nrow = 0))
        colnames(cdr_sum_records) <- c("record_id", "cdr_sum")

        for (x in result){
            if (x$cdr_sum >= cdr_sum_min && x$cdr_sum <= cdr_sum_max) {
                cdr_sum_records[nrow(cdr_sum_records) + 1,] <- c(x$record_id, x$cdr_sum)
            }
        }
        cdr_sum_response <- list(cdr_sum_records, cdr_sum_min, cdr_sum_max)

        return(cdr_sum_response)
    })

    get_np_dx <- reactive({

        np_dx_input <- input$np_dx
        result <- get_data('json', '')

        np_dx_records <- data.frame(matrix(ncol = 2, nrow = 0))
        colnames(np_dx_records) <- c("record_id", "np_dx")
        
        for (x in result){
            if (grepl(",", x$np_dx)){
                buffer <- unlist(strsplit(x$np_dx, ","))
            } else{
                buffer <- x$np_dx
            }
            ifelse (buffer %chin% np_dx_input,
            np_dx_records[nrow(np_dx_records) + 1,] <- c(x$record_id, x$np_dx),
            "")
        }
        np_dx_response <- list(np_dx_records, np_dx_input)

        return(np_dx_response)
    })

    get_apoe <- reactive({

        apoe_input <- input$apoe
        apoe_input <- replace(apoe_input, apoe_input == "Unknown", "")
        result <- get_data('json', '')

        apoe_records <- data.frame(matrix(ncol = 2, nrow = 0))
        colnames(apoe_records) <- c("record_id", "apoe")
        
        for (x in result){
            if (x$apoe %chin% apoe_input){
                 apoe_records[nrow(apoe_records) + 1,] <- c(x$record_id, x$apoe)
            }
        }
        apoe_response <- list(apoe_records, apoe_input)

        return(apoe_response)
    })

    get_dx_p <- reactive({

        dx_p_input <- input$dx_p
        result <- get_data('json', '')

        dx_p_records <- data.frame(matrix(ncol = 2, nrow = 0))
        colnames(dx_p_records) <- c("record_id", "dx_p")
        
        for (x in result){
            if (x$dx_p %chin% dx_p_input){
                 dx_p_records[nrow(dx_p_records) + 1,] <- c(x$record_id, x$dx_p)
            }
        }
        dx_p_response <- list(dx_p_records, dx_p_input)

        return(dx_p_response)
    })

    get_uds_synd <- reactive({

        uds_synd_input <- input$uds_synd
        result <- get_data('json', '')

        uds_synd_records <- data.frame(matrix(ncol = 2, nrow = 0))
        colnames(uds_synd_records) <- c("record_id", "uds_synd")
        
        for (x in result){
            if (x$synd %chin% uds_synd_input){
                 uds_synd_records[nrow(uds_synd_records) + 1,] <- c(x$record_id, x$synd)
            }
        }
        uds_synd_response <- list(uds_synd_records, uds_synd_input)
        
        return(uds_synd_response)
        
    })

    get_common_records <- reactive({

        age_records <- get_age()[1][[1]]
        gender_records <- get_gender()[1][[1]]
        race_records <- get_race()[1][[1]]
        year_records <- get_year()[1][[1]]
        hispanic_records <- get_hispanic()
        pmi_records <- get_pmi()[1][[1]]
        csf_records <- get_csf()[1][[1]]
        plasma_records <- get_plasma()[1][[1]]
        serum_records <- get_serum()[1][[1]]
        cdr_records <- get_cdr()[1][[1]]
        cdr_sum_records <- get_cdr_sum()[1][[1]]
        age_records <- get_age()[1][[1]]
        np_dx_records <- get_np_dx()[1][[1]]
        apoe_records <- get_apoe()[1][[1]]
        dx_p_records <- get_dx_p()[1][[1]]
        uds_synd_records <- get_uds_synd()[1][[1]]
        common_des_vector <- c()

        df_list <- list()
        if (input$age_incl & nrow(age_records) > 0){
            df_list <- append(df_list, list(age_records))
            age_criteria <- paste("ages: ", get_age()[2], "-", get_age()[3])
            common_des_vector <- append(common_des_vector, age_criteria)
        }
        if (nrow(gender_records) > 0){
            df_list <- append(df_list, list(gender_records))
            gender_criteria <- paste("gender: ", get_gender()[2])
            common_des_vector <- append(common_des_vector, gender_criteria)
        }
        if (nrow(race_records) > 0){
            df_list <- append(df_list, list(race_records))
            race_criteria <- paste("races: ", get_race()[2])
            common_des_vector <- append(common_des_vector, race_criteria)
        }
        if (nrow(year_records) > 0){
            df_list <- append(df_list, list(year_records))
            year_criteria <- paste("years: ", get_year()[2], "-", get_year()[3])
            common_des_vector <- append(common_des_vector, year_criteria)
        }
        if (nrow(hispanic_records) > 0){
            df_list <- append(df_list, list(hispanic_records))
            hisp_criteria <- paste("hispanic: ", input$hispanic)
            common_des_vector <- append(common_des_vector, hisp_criteria)
        }
        if (input$pmi_incl & nrow(pmi_records) > 0){
            df_list <- append(df_list, list(pmi_records))
            pmi_criteria <- paste("pmi: ", get_pmi()[2], "-", get_pmi()[3])
            common_des_vector <- append(common_des_vector, pmi_criteria)
        }
        if (input$csf_incl & nrow(csf_records) > 0){
            df_list <- append(df_list, list(csf_records))
            csf_criteria <- paste("csf: ", get_csf()[2], "-", get_csf()[3])
            common_des_vector <- append(common_des_vector, csf_criteria)
        }
        if (input$plasma_incl & nrow(plasma_records) > 0){
            df_list <- append(df_list, list(plasma_records))
            plasma_criteria <- paste("plasma: ", get_plasma()[2], "-", get_plasma()[3])
            common_des_vector <- append(common_des_vector, plasma_criteria)
        }
        if (input$serum_incl & nrow(serum_records) > 0){
            df_list <- append(df_list, list(serum_records))
            serum_criteria <- paste("serum: ", get_serum()[2], "-", get_serum()[3])
            common_des_vector <- append(common_des_vector, serum_criteria)
        }
        if (input$cdr_incl & nrow(cdr_records) > 0){
            df_list <- append(df_list, list(cdr_records))
            cdr_criteria <- paste("cdr: ", get_cdr()[2], "-", get_cdr()[3])
            common_des_vector <- append(common_des_vector, cdr_criteria)
        }
        if (input$sum_incl & nrow(cdr_sum_records) > 0){
            df_list <- append(df_list, list(cdr_sum_records))
            sum_criteria <- paste("cdr_sum: ", get_cdr_sum()[2], "-", get_cdr_sum()[3])
            common_des_vector <- append(common_des_vector, sum_criteria)
        }
        if (nrow(np_dx_records) > 0){
            df_list <- append(df_list, list(np_dx_records))
            np_dx_criteria <- paste("np_dx: ", get_np_dx()[2])
            common_des_vector <- append(common_des_vector, np_dx_criteria)
        }
        if (nrow(apoe_records) > 0){
            df_list <- append(df_list, list(apoe_records))
            apoe_criteria <- paste("apoe: ", get_apoe()[2])
            common_des_vector <- append(common_des_vector, apoe_criteria)
        }
        if (nrow(dx_p_records) > 0){
            df_list <- append(df_list, list(dx_p_records))
            dx_p_criteria <- paste("dx_p: ", get_dx_p()[2])
            common_des_vector <- append(common_des_vector, dx_p_criteria)
        }
        if (nrow(uds_synd_records) > 0){
            df_list <- append(df_list, list(uds_synd_records))
            uds_criteria <- paste("uds_synd: ", get_uds_synd()[2])
            common_des_vector <- append(common_des_vector, uds_criteria)
        }

        if (length(df_list) > 1){
            common_records <- Reduce(function(x, y){
                merge(x, y, by="record_id")
                }, df_list)
            
            common_response <- list(common_records, common_des_vector)
            return(common_response)
        }
    })

    output$age_records <- renderText({
        age_response <- get_age()
        if (length(age_response)) {
            paste(nrow(age_response[1][[1]]),
            "number of records of age",
            age_response[2], "-",
            age_response[3]
            )
        }else {
            paste("")
        }
    })

    output$gender_records <- renderText({
        gender_response <- get_gender()
        paste(nrow(gender_response[1][[1]]),
        "number of records of sex",
        sapply(gender_response[2], paste, collapse=', ')
        )
    })

    output$race_records <- renderText({
        race_response <- get_race()
        paste(nrow(race_response[1][[1]]),
        "number of records of race",
        sapply(race_response[2], paste, collapse=', ')
        )
    })

    output$hispanic_records <- renderText({
        hispanic_response <- get_hispanic()
        paste(nrow(hispanic_response),
        "number of records of hispanics"
        )
    })

    output$year_records <- renderText({
        year_response <- get_year()
        paste(nrow(year_response[1][[1]]),
        "number of records of year",
        year_response[2],
        "-", year_response[3]
        )
    })

    output$pmi_records <- renderText({
        pmi_response <- get_pmi()
        paste(nrow(pmi_response[1][[1]]),
        "number of pmi from",
        pmi_response[2],
        "-",
        pmi_response[3])
    })

    output$csf_records <- renderText({
        csf_response <- get_csf()
        paste(nrow(csf_response[1][[1]]),
        "number of csf from",
        csf_response[2],
        "-",
        csf_response[3])
    })

    output$plasma_records <- renderText({
        plasma_response <- get_plasma()
        paste(nrow(plasma_response[1][[1]]),
        "number of plasma from",
        plasma_response[2],
        "-",
        plasma_response[3])
    })

    output$serum_records <- renderText({
        serum_response <- get_serum()
        paste(nrow(serum_response[1][[1]]),
        "number of serum from",
        serum_response[2],
        "-",
        serum_response[3])
    })

    output$cdr_records <- renderText({
        cdr_response <- get_cdr()
        paste(nrow(cdr_response[1][[1]]),
        "number of cdr from",
        cdr_response[2],
        "-",
        cdr_response[3])
    })

    output$cdr_sum_records <- renderText({
        cdr_sum_response <- get_cdr_sum()
        paste(nrow(cdr_sum_response[1][[1]]),
        "number of cdr_sum from",
        cdr_sum_response[2],
        "-",
        cdr_sum_response[3])
    })

    output$np_dx_records <- renderText({
        np_dx_response <- get_np_dx()
        paste(nrow(np_dx_response[1][[1]]), 
        "of Neuropathological Diagnosis ", 
        sapply(np_dx_response[2], paste, collapse=', ')
        )
    })

    output$apoe_records <- renderText({
        apoe_response <- get_apoe()
        paste(nrow(apoe_response[1][[1]]), 
        "of  APOE ", 
        sapply(apoe_response[2], paste, collapse=', ')
        )
    })

    output$dx_p_records <- renderText({
        dx_p_response <- get_dx_p()
        paste(nrow(dx_p_response[1][[1]]), 
        "of UDS Diagnosis Primary ", 
        sapply(dx_p_response[2], paste, collapse=', ')
        )
    })

    output$uds_synd_records <- renderText({
        uds_synd_response <- get_uds_synd()
        paste(nrow(uds_synd_response[1][[1]]), 
        "of UDS Syndrome ", 
        sapply(uds_synd_response[2], paste, collapse=', ')
        )
    })

    output$common_records <- renderText({

        common_records <- get_common_records()[1][[1]]
        common_description <- get_common_records()[2]

        if (!is.null(common_records) && nrow(common_records) > 0){
            paste(nrow(common_records), "number of common records of criteria: ", common_description)
        } else{
            paste("No common records found.")
        }
    })

    output$plot_common_records <- renderPlot({

        common_records <- get_common_records()[1][[1]]                

        if (!is.null(common_records)){
            if (nrow(common_records) > 0){
                if ("caseyear" %in% colnames(common_records)){
                    ggplot(data=common_records, aes(caseyear, fill=caseyear)) +
                    geom_bar(position="dodge", stat = "count") +
                    geom_text(aes(label = after_stat(count)), stat = "count")

                } else if ("race" %in% colnames(common_records)){
                    ggplot(data=common_records, aes(race, fill=race)) +
                    geom_bar(position="dodge", stat = "count") +
                    geom_text(aes(label = after_stat(count)), stat = "count")

                } else if ("sex" %in% colnames(common_records)){
                    ggplot(data=common_records, aes(sex, fill=sex)) +
                    geom_bar(position="dodge",  stat = "count") +
                    geom_text(aes(label = after_stat(count)), stat = "count")
                }
            }
        }

    })

    output$plot_age_count <- renderPlot({

        age_count <- get_age_count()

        if (!is.null(age_count) && nrow(age_count) > 0){
            ggplot(data=age_count, aes(x = input_range,
            y = as.numeric(count),
            fill = input_range)) +
            geom_bar(position = "dodge", stat = "identity") +
            geom_text(aes(label=as.numeric(count)))
        } 

    }
    )

    output$plot_year_records <- renderPlot({
        year_records <- get_year()[1][[1]]

        if (!is.null(year_records) && nrow(year_records) > 0){
            ggplot(data=year_records, aes(caseyear, fill=caseyear)) +
                geom_bar(position="dodge", stat = "count") +
                geom_text(aes(label = after_stat(count)), stat = "count")
        }
    })

    output$age_cond <- reactive({
        input$age_plot & (nrow(get_age()[1][[1]]) > 0)

    })

    output$years_cond <- reactive({
        input$year_plot & (nrow(get_year()[1][[1]]) > 0)
    })

    outputOptions(output, "age_cond", suspendWhenHidden = FALSE)

    output$common_cond <- reactive({
        (nrow(get_common_records()[1][[1]]) > 0)
    })

    outputOptions(output, "common_cond", suspendWhenHidden = FALSE)

    outputOptions(output, "years_cond", suspendWhenHidden = FALSE)

    output$export_results <- downloadHandler(
        filename = function() {
            paste('data-', Sys.Date(), '.csv', sep='')
        },
        content = function(file) {
            common_records <- get_common_records()[1][[1]]
            write.csv(common_records, file)
        }
    )

}

##DKH 2023-02-13 
##commenting out this line allows app to work on a Shiny Server; also works when using Run App in RStudio
shinyApp(ui = ui, server = server)
