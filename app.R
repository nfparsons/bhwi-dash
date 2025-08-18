#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# =============================================================================
# 1. SETUP/DEPENDENCIES
# =============================================================================

if (!require("pacman")) {
    install.packages("pacman", repos = "https://cran.rstudio.org")
}
library(pacman)

p_load(
    here,
    conflicted,
    rio,
    googledrive,
    googlesheets4,
    shiny,
    flexdashboard,
    DT,
    lubridate,
    stringr,
    showtext,
    treemap,
    tidyverse,
    janitor,
    bslib, 
    bsicons
)

# Conflict Resolution
conflicts_prefer(
    dplyr::filter,
    dplyr::first,
    dplyr::summarize,
    dplyr::select,
    janitor::clean_names,
    janitor::chisq.test,
    lubridate::year,
    rio::export,
    tidyselect::starts_with
)

# Global Options, Themes, and Table Settings
set.seed(13)
options(scipen = 9999)

gtsummary::theme_gtsummary_journal(journal = "jama")
gtsummary::theme_gtsummary_compact()

flextable::set_flextable_defaults(
    table.layout = "autofit",
    font.size = 10,
    font.family = "Times New Roman",
    padding.top = 0,
    padding.bottom = 0
)

# Custom color palette for bslib themes
mchd_county_logo_blue = "#326195"
mchd_county_logo_green = "#48773C"
mchd_green = "#385D2F"
mchd_claret = "#8C183E"
mchd_deep_saffron = "#F79232"
mchd_copper_rose = "#9b6167"
mchd_light_cerulean = "#72CCD4"

perma_list <- c(
    "data_repo",
    "gis_repo",
    "mchd_claret",
    "mchd_copper_rose",
    "mchd_county_logo_blue",
    "mchd_county_logo_green",
    "mchd_deep_saffron",
    "mchd_green",
    "mchd_light_cerulean"
)

showtext::showtext_auto()

# =============================================================================
# 2. DATA PROCESSING (FROM QUARTO DATA CHUNK)
# =============================================================================

sheets_url <- "https://docs.google.com/spreadsheets/d/1lRMyvGBAXcUaQtmBxXDFyrs889L505OFRRyvC0zQJtg/edit?usp=drive_link"
my_sheet <- googledrive::drive_get(sheets_url)

raw_data <- googlesheets4::read_sheet(my_sheet) %>%
    mutate(Race = str_replace(Race, "Black of African American", "Black or African American"))

program_clean <- raw_data %>%
    mutate(program_clean = str_trim(str_extract(`Which program do you work in, or which program will this funding support? If it is funding for/training for multiple programs, please select all that apply. If it is funding for/training for an entire unit, just select that unit.`, "^[^,]+"))) %>%
    pull(program_clean)

top_9_programs <- raw_data %>%
    mutate(program_clean = str_trim(str_extract(`Which program do you work in, or which program will this funding support? If it is funding for/training for multiple programs, please select all that apply. If it is funding for/training for an entire unit, just select that unit.`, "^[^,]+"))) %>%
    count(program_clean) %>%
    arrange(desc(n)) %>%
    slice(1:9) %>%
    pull(program_clean)

top_5_races <- raw_data %>%
    mutate(race_clean = str_trim(str_extract(`Race`, "^[^,]+"))) %>%
    count(race_clean) %>%
    arrange(desc(n)) %>%
    slice(1:5) %>%
    pull(race_clean)

top_5_genders <- raw_data %>%
    mutate(gender_clean = str_trim(str_extract(`Gender Identity`, "^[^,]+"))) %>%
    count(gender_clean) %>%
    arrange(desc(n)) %>%
    slice(1:5) %>%
    pull(gender_clean)

top_5_orientations <- raw_data %>%
    mutate(orientation_clean = str_trim(str_extract(`Sexual Orientation`, "^[^,]+"))) %>%
    count(orientation_clean) %>%
    arrange(desc(n)) %>%
    slice(1:5) %>%
    pull(orientation_clean)

data <- raw_data %>%
    mutate(
        date = as.Date(Timestamp),
        fiscal_year_calc = if_else(month(date) < 7, year(date), year(date) + 1),
        fiscal_quarter = factor(
            paste0("FY", fiscal_year_calc, " Q", quarter(date, fiscal_start = 7)),
            levels = unique(paste0("FY", fiscal_year_calc, " Q", quarter(date, fiscal_start = 7)))
        ),
        program_group = if_else(
            str_trim(str_extract(`Which program do you work in, or which program will this funding support? If it is funding for/training for multiple programs, please select all that apply. If it is funding for/training for an entire unit, just select that unit.`, "^[^,]+")) %in% top_9_programs,
            str_trim(str_extract(`Which program do you work in, or which program will this funding support? If it is funding for/training for multiple programs, please select all that apply. If it is funding for/training for an entire unit, just select that unit.`, "^[^,]+")),
            "Other"
        ),
        race_group = if_else(
            str_trim(str_extract(`Race`, "^[^,]+")) %in% top_5_races,
            str_trim(str_extract(`Race`, "^[^,]+")),
            "Other"
        ),
        gender_group = if_else(
            str_trim(str_extract(`Gender Identity`, "^[^,]+")) %in% top_5_genders,
            str_trim(str_extract(`Gender Identity`, "^[^,]+")),
            "Other"
        ),
        sexual_orientation_group = if_else(
            str_trim(str_extract(`Sexual Orientation`, "^[^,]+")) %in% top_5_orientations,
            str_trim(str_extract(`Sexual Orientation`, "^[^,]+")),
            "Other"
        )
    ) %>%
    filter(!is.na(date)) %>%
    select(
        status = `Approval Status`,
        date,
        fiscal_quarter,
        race = `Race`,
        race_group,
        gender_identity = `Gender Identity`,
        gender_group,
        sexual_orientation = `Sexual Orientation`,
        sexual_orientation_group,
        funding_for = `Please select the item you are requesting funding for:`,
        program = `Which program do you work in, or which program will this funding support? If it is funding for/training for multiple programs, please select all that apply. If it is funding for/training for an entire unit, just select that unit.`,
        program_group,
        managers_approval = `Have you received your supervisor's or manager's approval for this request?`,
        represented_status = `Union/Representation Status of staff receiving/using the funding`,
        amount_approved = `Amount Approved`
    )

funding <- googlesheets4::read_sheet(my_sheet,
                                     sheet = "Funding Tracking",
                                     range = "A5:E",
                                     col_types = "ccccn") %>%
    select(
        date = `Date Funding was approved or Invoice received`,
        staff = `Staff using funding`,
        represented_status = `Represented Status`,
        approved_for = `Approved for`,
        amount_approved = `Amount Approved`
    ) %>%
    filter(!is.na(date)) %>%
    mutate(
        date = mdy(date),
        fiscal_year_calc = if_else(month(date) < 7, year(date), year(date) + 1),
        fiscal_quarter = factor(
            paste0("FY", fiscal_year_calc, " Q", quarter(date, fiscal_start = 7)),
            levels = unique(paste0("FY", fiscal_year_calc, " Q", quarter(date, fiscal_start = 7)))
        )
    )

# =============================================================================
# 3. UI DEFINITION
# =============================================================================

ui <- fluidPage(
    
    theme = bslib::bs_theme(
        version = 5,
        bootswatch = "cosmo"
    ),
    
    titlePanel("BHWI Report Dashboard"),
    
    sidebarLayout(
        sidebarPanel(
            selectizeInput(
                inputId = "fiscal_quarter",
                label = "Select Fiscal Quarter(s):",
                choices = c("All", as.character(sort(unique(data$fiscal_quarter)))),
                selected = "All",
                multiple = TRUE
            )
        ),
        
        mainPanel(
            fluidRow(
                column(4,
                       bslib::value_box(
                           title = "Total Applications",
                           value = textOutput("valuebox_total"),
                           showcase = bsicons::bs_icon("file-text"),
                           theme = "primary"
                       )
                ),
                column(4,
                       bslib::value_box(
                           title = "Approval Rate",
                           value = textOutput("valuebox_approval_rate"),
                           showcase = bsicons::bs_icon("check-square"),
                           theme = "success"
                       )
                ),
                column(4,
                       bslib::value_box(
                           title = "Total Funding",
                           value = textOutput("valuebox_funding"),
                           showcase = bsicons::bs_icon("currency-dollar"),
                           theme = "secondary"
                       )
                )
            ),
            
            fluidRow(
                column(12,
                       plotOutput("chart-approved")
                )
            )
        )
    )
)

# =============================================================================
# 4. SERVER LOGIC
# =============================================================================

server <- function(input, output, session) {
    
    data_filtered <- reactive({
        req(input$fiscal_quarter)
        if ("All" %in% input$fiscal_quarter) {
            return(data)
        } else {
            data %>%
                filter(fiscal_quarter %in% input$fiscal_quarter)
        }
    })
    
    funding_filtered <- reactive({
        req(input$fiscal_quarter)
        if ("All" %in% input$fiscal_quarter) {
            return(funding)
        } else {
            funding %>%
                filter(fiscal_quarter %in% input$fiscal_quarter)
        }
    })
    
    # We now render the dynamic value for the textOutput
    output$valuebox_total <- shiny::renderText({
        total_applications <- nrow(data_filtered())
        total_applications
    })
    
    output$valuebox_approval_rate <- shiny::renderText({
        approved_applications <- data_filtered() %>% filter(status == "Approved")
        if (nrow(data_filtered()) > 0) {
            approval_rate <- nrow(approved_applications) / nrow(data_filtered()) * 100
        } else {
            approval_rate <- 0
        }
        paste0(round(approval_rate, 1), "%")
    })
    
    output$valuebox_funding <- shiny::renderText({
        total_funding <- sum(funding_filtered()$amount_approved, na.rm = TRUE)
        paste0("$", formatC(total_funding, format = "f", big.mark = ",", digits = 0))
    })
    
    output$`chart-approved` <- renderPlot({
        approved_data <- data_filtered() %>%
            filter(status == "Approved") %>%
            mutate(
                program_abb = case_when(
                    program_group == "Adult Protective Services and Risk Case Management" ~ "APS&RCM",
                    program_group == "Call Center/Crisis Services" ~ "CC/CS",
                    program_group == "CMHP - Safety Net Services Unit" ~ "CMHP: SNS",
                    program_group == "Direct Clinical Services Unit" ~ "DCS",
                    program_group == "Early Assessment and Support Alliance (EASA)" ~ "EASA",
                    program_group == "Early Childhood Services (EC)" ~ "EC",
                    program_group == "Other" ~ "Other",
                    program_group == "Quality Management Unit" ~ "QM",
                    program_group == "School Based Mental Health (SBMH)" ~ "SBMH",
                    program_group == "Wraparound" ~ "Wrap"
                )
            ) %>%
            group_by(program_abb) %>%
            summarise(count = n(), .groups = 'drop') %>%
            mutate(total_count = sum(count)) %>%
            mutate(percentage = (count / total_count) * 100) %>%
            mutate(program_pct = str_glue("{program_abb} \n({round(percentage, 1)}%)")) %>%
            select(-total_count) %>%
            filter(count > 0)
        
        treemap(
            dtf = approved_data,
            index = "program_pct",
            vSize = "count",
            type = "index",
            title = "Application Approvals by Program",
            palette = adjustcolor(
                col = c("#326195", "#48773C", "#8C183E", "#F79232", "#9b6167", "#72CCD4"),
                alpha.f = 0.6),
            border.col = "white",
            border.lwds = 2,
            fontsize.labels = 14,
            fontcolor.labels = "white",
            fontface.labels = 2,
            fontfamily.labels = "sans",
            bg.labels = "transparent",
            align.labels = c("center", "center"),
            inflate.labels = FALSE,
            aspRatio = 1.5
        )
    })
}

# Run the application
shinyApp(ui, server)
