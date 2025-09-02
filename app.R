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
# 2. GOOGLE SHEETS AUTHENTICATION
# =============================================================================

# Clear token cache
# googlesheets4::gs4_deauth()

# Authenticate with Google Drive & Google Sheets
# googlesheets4::gs4_auth()

# =============================================================================
# 3. DATA PROCESSING
# =============================================================================

# sheets_url <- "https://docs.google.com/spreadsheets/d/1lRMyvGBAXcUaQtmBxXDFyrs889L505OFRRyvC0zQJtg/edit?usp=drive_link"
# my_sheet <- googledrive::drive_get(sheets_url)

# raw_data <- googlesheets4::read_sheet(my_sheet) %>%
#     mutate(
#         Race = str_replace(
#             Race,
#             "Black of African American",
#             "Black or African American"
#         )
#     )

raw_data <- rio::import(
    "data/_Funding Request (Responses) and Tracker(BH Workforce Initiative Funding Request Form).xlsx",
    sheet = "Form Responses 1"
) %>%
    janitor::clean_names() %>%
    mutate(
        race = str_replace(
            race,
            "Black of African American",
            "Black or African American"
        ),
        # Use parse_date_time for flexible date parsing
        date = coalesce(
            lubridate::mdy_hms(timestamp),
            lubridate::ymd_hms(timestamp)
        ),
        # Clean the `how_much_money...` column by removing non-numeric characters
        amount_requested = case_when(
            str_detect(
                how_much_money_are_you_requesting_what_is_the_cost_of_this_service,
                "Registration"
            ) ~
                sum(
                    as.numeric(str_extract_all(
                        how_much_money_are_you_requesting_what_is_the_cost_of_this_service,
                        "\\d+\\.?\\d*"
                    )[[1]]),
                    na.rm = TRUE
                ),
            TRUE ~
                as.numeric(gsub(
                    "[^0-9.]",
                    "",
                    how_much_money_are_you_requesting_what_is_the_cost_of_this_service
                ))
        )
    )

program_clean <- raw_data %>%
    mutate(
        program_clean = str_trim(str_extract(
            `which_program_do_you_work_in_or_which_program_will_this_funding_support_if_it_is_funding_for_training_for_multiple_programs_please_select_all_that_apply_if_it_is_funding_for_training_for_an_entire_unit_just_select_that_unit`,
            "^[^,]+"
        ))
    ) %>%
    pull(program_clean)

top_9_programs <- raw_data %>%
    mutate(
        program_clean = str_trim(str_extract(
            `which_program_do_you_work_in_or_which_program_will_this_funding_support_if_it_is_funding_for_training_for_multiple_programs_please_select_all_that_apply_if_it_is_funding_for_training_for_an_entire_unit_just_select_that_unit`,
            "^[^,]+"
        ))
    ) %>%
    count(program_clean) %>%
    arrange(desc(n)) %>%
    slice(1:9) %>%
    pull(program_clean)

top_5_races <- raw_data %>%
    mutate(race_clean = str_trim(str_extract(`race`, "^[^,]+"))) %>%
    count(race_clean) %>%
    arrange(desc(n)) %>%
    slice(1:5) %>%
    pull(race_clean)

top_5_genders <- raw_data %>%
    mutate(
        gender_clean = str_trim(str_extract(`gender_identity`, "^[^,]+"))
    ) %>%
    count(gender_clean) %>%
    arrange(desc(n)) %>%
    slice(1:5) %>%
    pull(gender_clean)

top_5_orientations <- raw_data %>%
    mutate(
        orientation_clean = str_trim(str_extract(
            `sexual_orientation`,
            "^[^,]+"
        ))
    ) %>%
    count(orientation_clean) %>%
    arrange(desc(n)) %>%
    slice(1:5) %>%
    pull(orientation_clean)

top_9_funding_items <- raw_data %>%
    count(please_select_the_item_you_are_requesting_funding_for) %>%
    arrange(desc(n)) %>%
    slice(1:9) %>%
    pull(please_select_the_item_you_are_requesting_funding_for)

data <- raw_data %>%
    mutate(
        fiscal_year_calc = if_else(month(date) < 7, year(date), year(date) + 1),
        fiscal_quarter = factor(
            paste0(
                "FY",
                fiscal_year_calc,
                " Q",
                quarter(date, fiscal_start = 7)
            ),
            levels = unique(paste0(
                "FY",
                fiscal_year_calc,
                " Q",
                quarter(date, fiscal_start = 7)
            ))
        ),
        program_group = if_else(
            str_trim(str_extract(
                `which_program_do_you_work_in_or_which_program_will_this_funding_support_if_it_is_funding_for_training_for_multiple_programs_please_select_all_that_apply_if_it_is_funding_for_training_for_an_entire_unit_just_select_that_unit`,
                "^[^,]+"
            )) %in%
                top_9_programs,
            str_trim(str_extract(
                `which_program_do_you_work_in_or_which_program_will_this_funding_support_if_it_is_funding_for_training_for_multiple_programs_please_select_all_that_apply_if_it_is_funding_for_training_for_an_entire_unit_just_select_that_unit`,
                "^[^,]+"
            )),
            "Other"
        ),
        race_group = if_else(
            str_trim(str_extract(`race`, "^[^,]+")) %in% top_5_races,
            str_trim(str_extract(`race`, "^[^,]+")),
            "Other"
        ),
        gender_group = if_else(
            str_trim(str_extract(`gender_identity`, "^[^,]+")) %in%
                top_5_genders,
            str_trim(str_extract(`gender_identity`, "^[^,]+")),
            "Other"
        ),
        sexual_orientation_group = if_else(
            str_trim(str_extract(`sexual_orientation`, "^[^,]+")) %in%
                top_5_orientations,
            str_trim(str_extract(`sexual_orientation`, "^[^,]+")),
            "Other"
        ), 
        funding_for_group = if_else(
            `please_select_the_item_you_are_requesting_funding_for` %in% top_9_funding_items,
            `please_select_the_item_you_are_requesting_funding_for`,
            "Other"
        )
    ) %>%
    filter(!is.na(date)) %>%
    select(
        status = approval_status,
        date,
        fiscal_quarter,
        race,
        race_group,
        gender_identity,
        gender_group,
        sexual_orientation,
        sexual_orientation_group,
        funding_for = please_select_the_item_you_are_requesting_funding_for,
        program = which_program_do_you_work_in_or_which_program_will_this_funding_support_if_it_is_funding_for_training_for_multiple_programs_please_select_all_that_apply_if_it_is_funding_for_training_for_an_entire_unit_just_select_that_unit,
        program_group,
        managers_approval = have_you_received_your_supervisors_or_managers_approval_for_this_request,
        represented_status = union_representation_status_of_staff_receiving_using_the_funding,
        amount_requested,
        amount_approved
    )


funding <- rio::import(
    "data/_Funding Request (Responses) and Tracker(BH Workforce Initiative Funding Request Form).xlsx",
    sheet = "Funding Tracking",
    skip = 4,
    col_types = c(
        "date", # Date Funding was approved or Invoice received
        "text", # Staff using funding
        "text", # Represented Status
        "text", # Approved for
        "numeric", # Amount Approved
        rep("skip", 22) # Skip the remaining 22 columns
    )
) %>%
    janitor::clean_names() %>%
    select(
        date = `date_funding_was_approved_or_invoice_received`,
        staff = `staff_using_funding`,
        represented_status = `represented_status`,
        approved_for = `approved_for`,
        amount_approved = `amount_approved`
    ) %>%
    filter(!is.na(date)) %>%
    mutate(
        date = lubridate::mdy(date),
        fiscal_year_calc = if_else(month(date) < 7, year(date), year(date) + 1),
        fiscal_quarter = factor(
            paste0(
                "FY",
                fiscal_year_calc,
                " Q",
                quarter(date, fiscal_start = 7)
            ),
            levels = unique(paste0(
                "FY",
                fiscal_year_calc,
                " Q",
                quarter(date, fiscal_start = 7)
            ))
        )
    )

# =============================================================================
# 4. UI DEFINITION
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
                choices = c(
                    "All",
                    as.character(sort(unique(data$fiscal_quarter)))
                ),
                selected = "All",
                multiple = TRUE
            )
        ),

        mainPanel(
            fluidRow(
                column(
                    4,
                    bslib::value_box(
                        title = "Total Applications",
                        value = textOutput("valuebox_total"),
                        showcase = bsicons::bs_icon("file-text"),
                        theme = "primary"
                    )
                ),
                column(
                    4,
                    bslib::value_box(
                        title = "Approval Rate",
                        value = textOutput("valuebox_approval_rate"),
                        showcase = bsicons::bs_icon("check-square"),
                        theme = "success"
                    )
                ),
                column(
                    4,
                    bslib::value_box(
                        title = "Total Funding",
                        value = textOutput("valuebox_funding"),
                        showcase = bsicons::bs_icon("currency-dollar"),
                        theme = "secondary"
                    )
                )
            ),

            hr(), # Add a horizontal line to separate value boxes from the plots

            tabsetPanel(
                id = "demographicTabs",
                type = "tabs",
                tabPanel(
                    "Program",
                    plotOutput("program_chart", height = "600px")
                ),
                tabPanel(
                    "Race",
                    plotOutput("race_chart", height = "600px")
                ),
                tabPanel(
                    "Gender",
                    plotOutput("gender_chart", height = "600px")
                ),
                tabPanel(
                    "Sexual Orientation",
                    plotOutput("orientation_chart", height = "600px")
                ), 
                tabPanel(
                    "Funding Item",
                    plotOutput("funding_chart", height = "600px")
                )
            )
        )
    )
)

# =============================================================================
# 5. SERVER LOGIC
# =============================================================================

server <- function(input, output, session) {
    # Existing data_filtered and funding_filtered reactives...
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

    # Existing value box outputs...
    output$valuebox_total <- shiny::renderText({
        total_applications <- nrow(data_filtered())
    })

    output$valuebox_approval_rate <- shiny::renderText({
        approved_applications <- data_filtered() %>%
            filter(status == "Approved")
        if (nrow(data_filtered()) > 0) {
            approval_rate <- nrow(approved_applications) /
                nrow(data_filtered()) *
                100
        } else {
            approval_rate <- 0
        }
        paste0(round(approval_rate, 1), "%")
    })

    output$valuebox_funding <- shiny::renderText({
        total_funding <- sum(funding_filtered()$amount_approved, na.rm = TRUE)
        paste0(
            "$",
            formatC(total_funding, format = "f", big.mark = ",", digits = 0)
        )
    })

    # Prepare data for the demographic charts
    demographic_data <- reactive({
        req(input$demographicTabs)
        data_to_plot <- data_filtered() %>%
            filter(status == "Approved")

        if (input$demographicTabs == "Race") {
            data_to_plot %>%
                count(race_group) %>%
                mutate(total_count = sum(n)) %>%
                mutate(
                    percentage = (n / total_count) * 100,
                    label = str_glue("{race_group} ({round(percentage, 1)}%)"),
                    bar_label = n
                ) %>%
                arrange(desc(n))
        } else if (input$demographicTabs == "Gender") {
            data_to_plot %>%
                count(gender_group) %>%
                mutate(total_count = sum(n)) %>%
                mutate(
                    percentage = (n / total_count) * 100,
                    label = str_glue(
                        "{gender_group} ({round(percentage, 1)}%)"
                    ),
                    bar_label = n
                ) %>%
                arrange(desc(n))
        } else if (input$demographicTabs == "Sexual Orientation") {
            data_to_plot %>%
                count(sexual_orientation_group) %>%
                mutate(total_count = sum(n)) %>%
                mutate(
                    percentage = (n / total_count) * 100,
                    label = str_glue(
                        "{sexual_orientation_group} ({round(percentage, 1)}%)"
                    ),
                    bar_label = n
                ) %>%
                arrange(desc(n))
        } else if (input$demographicTabs == "Program") {
            data_to_plot %>%
                count(program_group) %>%
                mutate(total_count = sum(n)) %>%
                mutate(
                    percentage = (n / total_count) * 100,
                    label = str_wrap(
                        str_glue(
                            "{program_group} \n({round(percentage, 1)}%)"
                        ),
                        width = 20
                    ),
                    bar_label = n
                ) %>%
                arrange(desc(n))
        } else if (input$demographicTabs == "Funding Item") {
            data_to_plot %>%
                count(funding_for_group) %>%
                mutate(total_count = sum(n)) %>%
                mutate(
                    percentage = (n / total_count) * 100,
                    label = str_wrap(
                        str_glue(
                            "{funding_for_group} \n({round(percentage, 1)}%)"
                        ),
                        width = 20
                    ),
                    bar_label = n
                ) %>%
                arrange(desc(n))
        }
    })

    # Create a reusable function for the ggplot chart
    create_bar_chart <- function(data_input) {
        # Calculate a dynamic x-axis limit for the plot
        max_count <- max(data_input$n)
        x_limit <- max_count * 1.1 # Add a 10% buffer

        ggplot(data_input, aes(x = n, y = fct_reorder(label, n))) +
            geom_bar(stat = "identity", fill = mchd_county_logo_blue) +
            geom_text(
                aes(label = bar_label),
                hjust = -0.5,
                size = 5,
                color = mchd_county_logo_blue
            ) +
            labs(
                title = "Application Approvals by Demographic Group",
                x = "",
                y = ""
            ) +
            theme_minimal() +
            theme(
                axis.text.x = element_blank(),
                axis.text.y = element_text(lineheight = 1.2),
                panel.grid.major.x = element_blank(),
                panel.grid.minor.y = element_blank(),
                plot.title = element_text(
                    hjust = 0.5,
                    size = 16,
                    face = "bold"
                ),
                plot.margin = margin(t = 20, r = 50, b = 20, l = 20)
            ) +
            # Extend the x-axis to make sure labels fit
            coord_cartesian(xlim = c(0, x_limit), clip = "off")
    }

    # Render the plots for each tab
    output$program_chart <- renderPlot({
        data_to_plot <- demographic_data() %>%
            filter(program_group != "Other")

        create_bar_chart(data_to_plot) +
            labs(title = "Application Approvals by Program")
    })

    output$race_chart <- renderPlot({
        data_to_plot <- demographic_data() %>%
            filter(race_group != "Other")

        create_bar_chart(data_to_plot) +
            labs(title = "Application Approvals by Race")
    })

    output$gender_chart <- renderPlot({
        data_to_plot <- demographic_data() %>%
            filter(gender_group != "Other")

        create_bar_chart(data_to_plot) +
            labs(title = "Application Approvals by Gender")
    })

    output$orientation_chart <- renderPlot({
        data_to_plot <- demographic_data() %>%
            filter(sexual_orientation_group != "Other")

        create_bar_chart(data_to_plot) +
            labs(title = "Application Approvals by Sexual Orientation")
    })
}

# Run the application
shinyApp(ui, server)
