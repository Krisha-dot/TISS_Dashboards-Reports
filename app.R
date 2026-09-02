library(shiny)
library(tidyverse)
library(plotly)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# ── Load data ──────────────────────────────────────────────────────────────────
library(readxl)

methane_data <- read_excel("merged_5yr_mean.xlsx")
carbon_data  <- read_excel("final_cleaned_merged_5yr.xlsx")

carbon_data <- carbon_data %>%
  rename(
    Entity       = country,
    Period       = period,
    co2_total    = `annual co2‚ emissions`,
    co2_pc       = `co2‚ emissions per capita`,
    gdp_pc       = `gdp per capita`,
    share_global = `share of global annual co2‚ emissions`
  ) %>%
  mutate(Period = factor(Period, levels = sort(unique(Period))))

ghg_data  <- methane_data %>%
  rename(
    Entity = country,
    prod_methane = `Annual methane emissions in CO2 equivalents`,
    cons_methane = `Annual consumption-based CO2 equivalents`,
    methane_pc   = `Annual CO2 emissions (per capita)`
  ) %>%
  mutate(
    Period = factor(Period, levels = sort(unique(Period))),
    Responsibility_Gap = cons_methane - prod_methane
  )

methane_sector_long <- methane_data %>%
  pivot_longer(
    cols = c(
      `Methane emissions from agriculture`,
      `Methane emissions from land-use change and forestry`,
      `Methane emissions from waste`,
      `Methane emissions from buildings`,
      `Methane emissions from industry`,
      `Methane emissions from manufacturing and construction`,
      `Methane emissions from transport`,
      `Methane emissions from electricity and heat`,
      `Fugitive emissions of methane from energy production`,
      `Methane emissions from other fuel combustion`
    ),
    names_to = "Sector",
    values_to = "Emissions"
  ) %>%
  filter(!is.na(Emissions))

world_map <- ne_countries(scale = "medium", returnclass = "sf")
# Clean continent + international transport entities
CONTINENT_ENTITIES <- c(
  "Africa", "Asia", "Europe",
  "North America", "South America", "Oceania",
  "International aviation", "International shipping"
)

# Country-to-continent lookup (countries only, no regional aggregates)
country_continent_map <- list(
  "Africa"         = c("Algeria","Angola","Benin","Botswana","Burkina Faso",
                       "Burundi","Cameroon","Cape Verde","Central African Republic",
                       "Chad","Comoros","Congo","Cote d'Ivoire",
                       "Democratic Republic of Congo","Djibouti","Egypt",
                       "Equatorial Guinea","Eritrea","Eswatini","Ethiopia",
                       "Gabon","Gambia","Ghana","Guinea","Guinea-Bissau",
                       "Kenya","Lesotho","Liberia","Libya","Madagascar",
                       "Malawi","Mali","Mauritania","Mauritius","Morocco",
                       "Mozambique","Namibia","Niger","Nigeria","Rwanda",
                       "Sao Tome and Principe","Senegal","Seychelles",
                       "Sierra Leone","Somalia","South Africa","South Sudan",
                       "Sudan","Tanzania","Togo","Tunisia","Uganda",
                       "Zambia","Zimbabwe"),
  "Asia"           = c("Afghanistan","Armenia","Azerbaijan","Bahrain",
                       "Bangladesh","Bhutan","Brunei","Cambodia","China",
                       "Cyprus","Georgia","Hong Kong","India","Indonesia",
                       "Iran","Iraq","Israel","Japan","Jordan","Kazakhstan",
                       "Kuwait","Kyrgyzstan","Laos","Lebanon","Macao",
                       "Malaysia","Maldives","Mongolia","Myanmar","Nepal",
                       "North Korea","Oman","Pakistan","Palestine","Philippines",
                       "Qatar","Russia","Saudi Arabia","Singapore","South Korea",
                       "Sri Lanka","Syria","Taiwan","Tajikistan","Thailand",
                       "Timor-Leste","Turkey","Turkmenistan","United Arab Emirates",
                       "Uzbekistan","Vietnam","Yemen"),
  "Europe"         = c("Albania","Andorra","Austria","Belarus","Belgium",
                       "Bosnia and Herzegovina","Bulgaria","Croatia","Czechia",
                       "Denmark","Estonia","Finland","France","Germany",
                       "Greece","Hungary","Iceland","Ireland","Italy",
                       "Kosovo","Latvia","Liechtenstein","Lithuania","Luxembourg",
                       "Malta","Moldova","Montenegro","Netherlands","North Macedonia",
                       "Norway","Poland","Portugal","Romania","San Marino",
                       "Serbia","Slovakia","Slovenia","Spain","Sweden",
                       "Switzerland","Ukraine","United Kingdom"),
  "North America"  = c("Antigua and Barbuda","Bahamas","Barbados","Belize",
                       "Canada","Costa Rica","Cuba","Dominica","Dominican Republic",
                       "El Salvador","Grenada","Guatemala","Haiti","Honduras",
                       "Jamaica","Mexico","Nicaragua","Panama","Puerto Rico",
                       "Saint Kitts and Nevis","Saint Lucia",
                       "Saint Vincent and the Grenadines","Trinidad and Tobago",
                       "United States"),
  "South America"  = c("Argentina","Bolivia","Brazil","Chile","Colombia",
                       "Ecuador","Guyana","Paraguay","Peru","Suriname",
                       "Uruguay","Venezuela"),
  "Oceania"        = c("Australia","Fiji","Kiribati","Marshall Islands",
                       "Micronesia (country)","Nauru","New Zealand",
                       "Papua New Guinea","Samoa","Solomon Islands",
                       "Tonga","Tuvalu","Vanuatu")
)

# ── UI ─────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  
  tags$head(tags$style(HTML("
   body { background-color:#e8f5e9; font-family:'Segoe UI'; }

    .sidebar { background-color:#a8d5a2 !important; color:white; }
    .sidebar .control-label { color:#ecf0f1; font-weight:bold; }
    .sidebar .selectize-input,
    .sidebar .form-control { background-color:#2e8b57; color:white; border:none; }
    .sidebar .radio label  { color:#ecf0f1; }

    .panel-box {
      background:white; padding:15px;
      border-radius:12px;
      box-shadow:0px 3px 8px rgba(0,0,0,0.2);
    }
    .panel-title {
      font-size:18px; font-weight:bold;
      color:white; padding:10px; border-radius:8px;
    }
    .blue   { background:#32cd32; }
    .red    { background:#32cd32; }
    .green  { background:#32cd32; }
    .purple { background:#32cd32; }

    .subtitle { color:#555; margin:10px 0 15px 0; }

    .nav-tabs > li > a {
      background-color:#6b8e23; color:white;
      border-radius:8px; margin-right:5px;
    }
    .nav-tabs > li > a:hover         { background-color:#1abc9c; color:white; }
    .nav-tabs > li.active > a {
      background-color:#e74c3c !important;
      color:white !important; font-weight:bold;
    }

    /* Make plotly outputs taller */
    .plotly-output { height:600px !important; }
    /* Slider bar - Sea Green 2 */
    .irs--shiny .irs-bar { background:#2e8b57; border-top:1px solid #2e8b57; border-bottom:1px solid #2e8b57; }
    .irs--shiny .irs-bar--single { background:#2e8b57; }
    .irs--shiny .irs-handle { background:#2e8b57; border:2px solid #1a5c38; }
  "))),
  
  titlePanel("Emissions and Development in Nigeria: A Data-Driven Analysis"),
  
  sidebarLayout(
    
    sidebarPanel(
      class = "sidebar",
      
      selectInput("country", "Select Country:",
                  choices  = sort(unique(ghg_data$Entity)),
                  selected = "Nigeria"),
      selectizeInput(
        "share_countries",
        "Add Countries to Emissions Donut (Max 5):",
        choices  = sort(setdiff(
          unique(carbon_data$Entity),
          c(CONTINENT_ENTITIES, "World", "Nigeria",
            grep("GCP|WB|excl|income|OECD|Antarctica|International",
                 unique(carbon_data$Entity), value = TRUE))
        )),
        multiple = TRUE,
        options  = list(maxItems = 5)
      ),
      
      selectizeInput("sector_countries",
                     "Select Countries for Per Capita Comparison (Max 8):",
                     choices  = sort(unique(ghg_data$Entity)),
                     multiple = TRUE,
                     options  = list(maxItems = 8)),
      
      selectInput("sector", "Select Methane Emission Sector:",
                  choices = unique(methane_sector_long$Sector)),
      
      radioButtons("metric", "Emission Accounting Type:",
                   choices  = c("Production vs Consumption"              = "both",
                                "Responsibility Gap (Consumption \u2212 Production)" = "gap"),
                   selected = "both"),
      
      selectInput("rank_type", "Country Ranking:",
                  choices  = c("Top 10 Countries"       = "top10",
                               "Bottom 10 Countries"    = "bottom10",
                               "Custom (Top / Bottom N)" = "custom"),
                  selected = "top10"),
      
      conditionalPanel(
        condition = "input.rank_type == 'custom'",
        
        radioButtons(
          "custom_direction",
          "Custom Ranking Direction:",
          choices  = c("Top Countries"    = "top",
                       "Bottom Countries" = "bottom"),
          selected = "top"
        ),
        
        numericInput(
          "custom_n",
          "Number of Countries (N):",
          value = 5, min = 1, max = 50, step = 1
        )
      ),
      
      radioButtons("value_format", "Display Values As:",
                   choices  = c("Absolute Values"   = "absolute",
                                "Percentage Share"  = "percent"),
                   selected = "absolute"),
      
      sliderInput("period", "Select Period:",
                  min     = 1,
                  max     = length(levels(ghg_data$Period)),
                  value   = length(levels(ghg_data$Period)),
                  step    = 1,
                  animate = TRUE)
    ),
    
    mainPanel(
      tabsetPanel(
        
        tabPanel("Per Capita Comparison",
                 div(class = "panel-title blue", "Per Capita Comparison"),
                 div(class = "panel-box",
                     plotlyOutput("pc_plot", height = "600px"))),
        
        tabPanel("GDP vs Emissions",
                 div(class = "panel-title red", "GDP vs Emissions"),
                 div(class = "panel-box",
                     plotlyOutput("scatter_plot", height = "600px"))),
        
        tabPanel("Emissions Share",
                 div(class = "panel-title green", "Emissions Share"),
                 div(class = "panel-box",
                     actionButton("share_back", 
                                  "\u2190 Back to Continents",
                                  style = "margin-bottom:10px; background:#2c3e50; 
                                   color:white; border:none; border-radius:6px;
                                   padding:6px 14px;"),
                     plotlyOutput("share_plot", height = "600px"))),
        
        tabPanel("Time Series",
                 div(class="panel-title blue",
                     "Nigeria CO\u2082 Emissions Over Time"),
                 div(class="panel-box",
                     plotlyOutput("ts_total_plot", height="380px"),
                     br(),
                     plotlyOutput("ts_pc_plot",    height="380px"))),
        
        tabPanel("Cross-Country Comparison",
                 div(class = "panel-title red",
                     "Cross-Country Climate Responsibility"),
                 div(class = "panel-box",
                     plotlyOutput("cc_plot", height = "600px"))),
        
        
        tabPanel("Outlier Detection",
                 div(class = "panel-title purple", "Outlier Detection: Who Breaks the Pattern?"),
                 div(class = "panel-box",
                     
                     fluidRow(
                       column(4,
                              radioButtons("outlier_metric", "Outlier Type:",
                                           choices = c(
                                             "Emission Efficiency (GDP vs CO2)"     = "efficiency",
                                             "Responsibility Gap (Prod vs Cons)"    = "gap"
                                           ), selected = "efficiency")
                       ),
                       column(4,
                              sliderInput("outlier_threshold", "Sensitivity (how extreme):",
                                          min = 0.5, max = 2.0, value = 1.0, step = 0.1)
                       ),
                       column(4,
                              radioButtons("outlier_scope", "Show:",
                                           choices = c(
                                             "Both"                    = "both",
                                             "Overperformers only"     = "over",
                                             "Underperformers only"    = "under"
                                           ), selected = "both")
                       )
                     ),
                     
                     hr(),
                     
                     # Main scatter plot
                     plotlyOutput("outlier_scatter", height = "500px"),
                     
                     hr(),
                     
                     # Summary cards row
                     fluidRow(
                       column(3,
                              div(style = "background:#e8f5e9; border-left:5px solid #2ca02c;
                            padding:12px; border-radius:8px;",
                                  tags$b("Cleanest for their wealth"),
                                  uiOutput("outlier_clean_list")
                              )
                       ),
                       column(3,
                              div(style = "background:#ffebee; border-left:5px solid #d62728;
                            padding:12px; border-radius:8px;",
                                  tags$b("Dirtiest for their wealth"),
                                  uiOutput("outlier_dirty_list")
                              )
                       ),
                       column(3,
                              div(style = "background:#e3f2fd; border-left:5px solid #1f77b4;
                            padding:12px; border-radius:8px;",
                                  tags$b("Hidden emission importers"),
                                  uiOutput("outlier_importers_list")
                              )
                       ),
                       column(3,
                              div(style = "background:#fff3e0; border-left:5px solid #ff7f0e;
                            padding:12px; border-radius:8px;",
                                  tags$b("Pollution exporters"),
                                  uiOutput("outlier_exporters_list")
                              )
                       )
                     ),
                     
                     br(),
                     
                    
                     )
                 )
        ),
      )
    )
  )


# ── Server ─────────────────────────────────────────────────────────────────────
server <- function(input, output) {
  
  # Tracks which continent the user has clicked in the donut
  selected_continent <- reactiveVal(NULL)
  
  # Helper: resolve the current period label
  selected_period <- reactive({
    levels(ghg_data$Period)[input$period]
  })
  
  # ── Per Capita ──────────────────────────────────────────────────────────────
  output$pc_plot <- renderPlotly({
    
    # Uses the same country selection as Sector Comparison
    countries_to_show <- unique(c("Nigeria", input$sector_countries))
    
    df <- carbon_data %>%
      filter(
        Period == selected_period(),
        Entity %in% countries_to_show
      ) %>%
      mutate(highlight = ifelse(Entity == "Nigeria", "Nigeria", "Others"))
    
    # Fallback: if no countries selected yet, show top 8
    if (length(input$sector_countries) == 0) {
      df <- carbon_data %>%
        filter(Period == selected_period()) %>%
        arrange(desc(co2_pc)) %>%
        slice_head(n = 8) %>%
        bind_rows(
          carbon_data %>%
            filter(Period == selected_period(), Entity == "Nigeria")
        ) %>%
        distinct(Entity, .keep_all = TRUE) %>%
        mutate(highlight = ifelse(Entity == "Nigeria", "Nigeria", "Others"))
    }
    
    p <- ggplot(df, aes(
      x    = reorder(Entity, co2_pc),
      y    = co2_pc,
      fill = highlight,
      text = paste0("Country: ", Entity,
                    "<br>Per Capita CO2: ", round(co2_pc, 2))
    )) +
      geom_col() +
      coord_flip() +
      scale_fill_manual(values = c("Nigeria" = "red", "Others" = "steelblue")) +
      labs(x = "", y = "CO2 per capita", fill = "") +
      theme_minimal(base_size = 14)
    
    ggplotly(p, tooltip = "text") %>%
      layout(height = 600)
  })
  
  # ── GDP vs Emissions ────────────────────────────────────────────────────────
  output$scatter_plot <- renderPlotly({
    
    df <- carbon_data %>%
      filter(Period == selected_period()) %>%
      mutate(highlight = ifelse(Entity == "Nigeria", "Nigeria", "Others"))
    
    p <- ggplot(df, aes(
      x     = gdp_pc,
      y     = co2_pc,
      color = highlight,
      text  = paste0("Country: ", Entity,
                     "<br>GDP per capita: ", round(gdp_pc, 2),
                     "<br>CO2 per capita: ", round(co2_pc, 2))
    )) +
      geom_point(data = filter(df, highlight == "Others"),
                 size = 3, alpha = 0.6) +
      # Nigeria plotted on top so it is never hidden
      geom_point(data = filter(df, highlight == "Nigeria"),
                 size = 5, shape = 18) +
      scale_color_manual(values = c("Nigeria" = "red", "Others" = "steelblue")) +
      labs(x = "GDP per capita", y = "CO2 per capita", color = "") +
      theme_minimal(base_size = 14)
    
    ggplotly(p, tooltip = "text") %>%
      layout(height = 600)
  })
  
  # ── Emissions Share (pie / donut) ───────────────────────────────────────────
  output$share_plot <- renderPlotly({
    
    period_now <- selected_period()
    
    # ── CONTINENT VIEW (default) ─────────────────────────────────────────────
    if (is.null(selected_continent())) {
      
      # Countries always shown individually (Nigeria + user selection)
      pinned_countries <- unique(c("Nigeria", input$share_countries))
      
      # Get continent totals
      df_cont <- carbon_data %>%
        filter(Period == period_now, Entity %in% CONTINENT_ENTITIES)
      
      # Get pinned country rows
      df_pinned <- carbon_data %>%
        filter(Period == period_now, Entity %in% pinned_countries)
      
      # Subtract each pinned country from its continent
      for (ctry in pinned_countries) {
        belongs_to <- names(Filter(function(x) ctry %in% x,
                                   country_continent_map))
        if (length(belongs_to) == 0) next
        cont_name <- belongs_to[1]
        ctry_row <- df_pinned %>% filter(Entity == ctry)
        if (nrow(ctry_row) == 0) next
        df_cont <- df_cont %>%
          mutate(
            co2_total    = ifelse(Entity == cont_name,
                                  co2_total - ctry_row$co2_total[1],
                                  co2_total),
            share_global = ifelse(Entity == cont_name,
                                  share_global - ctry_row$share_global[1],
                                  share_global)
          )
      }
      
      # Rename continents that had countries carved out
      pinned_conts <- names(Filter(function(x)
        any(pinned_countries %in% x), country_continent_map))
      df_cont <- df_cont %>%
        mutate(Entity = ifelse(
          Entity %in% pinned_conts,
          paste0(Entity, " (rest)"),
          Entity
        ))
      
      # ── ORDER FIX: each country sits next to its continent ──────────────────
      continent_order <- c(
        "Asia", "Asia (rest)",
        "North America", "North America (rest)",
        "Europe", "Europe (rest)",
        "Africa", "Africa (rest)",
        "South America", "South America (rest)",
        "Oceania", "Oceania (rest)",
        "International shipping",
        "International aviation"
      )
      
      df_combined <- bind_rows(df_cont, df_pinned) %>%
        filter(co2_total > 0 | Entity %in% pinned_countries)
      
      final_order <- c()
      for (slot in continent_order) {
        if (slot %in% df_combined$Entity) {
          final_order <- c(final_order, slot)
        }
        base_cont <- gsub(" \\(rest\\)", "", slot)
        if (grepl("rest", slot) && base_cont %in% names(country_continent_map)) {
          pinned_here <- intersect(pinned_countries,
                                   country_continent_map[[base_cont]])
          final_order <- c(final_order,
                           pinned_here[pinned_here %in% df_combined$Entity])
        }
      }
      remaining <- setdiff(df_combined$Entity, final_order)
      final_order <- c(final_order, remaining)
      
      df <- df_combined %>%
        mutate(Entity = factor(Entity, levels = final_order)) %>%
        arrange(Entity) %>%
        mutate(Entity = as.character(Entity))
      # ── END ORDER FIX ────────────────────────────────────────────────────────
      
      # Value format
      if (input$value_format == "absolute") {
        df$plot_value <- df$co2_total
        df$label_text <- paste0(scales::comma(round(df$co2_total / 1e9, 2)), " Gt")
      } else {
        df$plot_value <- df$share_global
        df$label_text <- paste0(round(df$share_global, 2), "%")
      }
      
      # Colors
      base_colors <- c(
        "Africa"                 = "#2ca02c",
        "Africa (rest)"          = "#2ca02c",
        "Asia"                   = "#d62728",
        "Asia (rest)"            = "#d62728",
        "Europe"                 = "#1f77b4",
        "Europe (rest)"          = "#1f77b4",
        "North America"          = "#ff7f0e",
        "North America (rest)"   = "#ff7f0e",
        "South America"          = "#9467bd",
        "South America (rest)"   = "#9467bd",
        "Oceania"                = "#8c564b",
        "Oceania (rest)"         = "#8c564b",
        "International aviation" = "#7f7f7f",
        "International shipping" = "#bcbd22",
        "Nigeria"                = "#e74c3c"
      )
      extra_pinned   <- setdiff(pinned_countries, "Nigeria")
      extra_palette  <- c("#f39c12","#1abc9c","#8e44ad","#2980b9","#27ae60")
      for (i in seq_along(extra_pinned)) {
        base_colors[extra_pinned[i]] <- extra_palette[i]
      }
      slice_colors <- base_colors[df$Entity]
      slice_colors[is.na(slice_colors)] <- "#cccccc"
      
      plot_ly(
        df,
        labels        = ~Entity,
        values        = ~plot_value,
        type          = "pie",
        hole          = 0.5,
        textinfo      = "label+text",
        text          = ~label_text,
        marker        = list(colors = unname(slice_colors)),
        hovertemplate = paste0(
          "<b>%{label}</b><br>",
          ifelse(input$value_format == "absolute",
                 "CO2: %{text}", "Share: %{text}"),
          "<extra></extra>"
        ),
        source = "share_donut"
      ) %>%
        layout(
          height      = 600,
          title       = list(text = paste0(
            "Global Emissions by Continent — ", period_now)),
          showlegend  = TRUE,
          annotations = list(list(
            text      = "Nigeria<br>always shown",
            x = 0.5, y = 0.5,
            font      = list(size = 12, color = "#e74c3c"),
            showarrow = FALSE
          ))
        )
      
      # ── COUNTRY VIEW (after clicking a continent) ────────────────────────────
    } else {
      
      cont <- selected_continent()
      
      # Countries in this continent from our map
      cont_countries <- country_continent_map[[cont]]
      
      if (is.null(cont_countries)) {
        # For International aviation/shipping — no drill-down
        selected_continent(NULL)
        return(NULL)
      }
      
      # Always include Nigeria + sidebar selection within this continent
      sidebar_sel <- input$share_countries
      extra <- intersect(sidebar_sel, cont_countries)
      focus  <- unique(c("Nigeria", extra))
      
      df_countries <- carbon_data %>%
        filter(Period == period_now, Entity %in% cont_countries)
      
      # Named countries to show individually
      df_named <- df_countries %>% filter(Entity %in% focus)
      
      # Rest lumped as "Rest of [Continent]"
      df_rest <- df_countries %>%
        filter(!Entity %in% focus) %>%
        summarise(
          co2_total    = sum(co2_total,    na.rm = TRUE),
          share_global = sum(share_global, na.rm = TRUE)
        ) %>%
        mutate(Entity = paste0("Rest of ", cont))
      
      df <- bind_rows(df_named, df_rest)
      
      if (input$value_format == "absolute") {
        df$plot_value <- df$co2_total
        df$label_text <- paste0(scales::comma(round(df$co2_total / 1e6, 1)), " Mt")
      } else {
        df$plot_value <- df$share_global
        df$label_text <- paste0(round(df$share_global, 2), "%")
      }
      
      # Nigeria always red
      n_slices   <- nrow(df)
      base_cols  <- RColorBrewer::brewer.pal(max(3, n_slices), "Set2")
      slice_cols <- base_cols[seq_len(n_slices)]
      vn_idx     <- which(df$Entity == "Nigeria")
      if (length(vn_idx) > 0) slice_cols[vn_idx] <- "thistle"
      
      plot_ly(
        df,
        labels        = ~Entity,
        values        = ~plot_value,
        type          = "pie",
        hole          = 0.5,
        textinfo      = "label+text",
        text          = ~label_text,
        marker        = list(colors = slice_cols),
        hovertemplate = paste0(
          "<b>%{label}</b><br>",
          ifelse(input$value_format == "absolute",
                 "CO2: %{text}", "Share: %{text}"),
          "<extra></extra>"
        )
      ) %>%
        layout(
          height     = 600,
          title      = list(text = paste0(cont, " — Country Breakdown — ",
                                          period_now)),
          showlegend = TRUE,
          annotations = list(list(
            text      = paste0("<b>", cont, "</b><br><a href='#'>← Back</a>"),
            x         = 0.5, y = 0.5,
            font      = list(size = 12),
            showarrow = FALSE
          ))
        )
    }
  })
  
  # ── Handle continent click to drill down ─────────────────────────────────────
  observeEvent(event_data("plotly_click", source = "share_donut"), {
    click <- event_data("plotly_click", source = "share_donut")
    if (!is.null(click)) {
      label <- click$customdata
      if (is.null(label)) {
        # fallback: use pointNumber to get entity name
        period_now    <- selected_period()
        df_check <- carbon_data %>%
          filter(Period == period_now, Entity %in% CONTINENT_ENTITIES)
        idx <- (click$pointNumber + 1)
        if (idx <= nrow(df_check)) {
          chosen <- df_check$Entity[idx]
          if (chosen %in% names(country_continent_map)) {
            selected_continent(chosen)
          }
        }
      } else if (label %in% names(country_continent_map)) {
        selected_continent(label)
      }
    }
  })
  
  # ── Back button — reset to continent view ────────────────────────────────────
  observeEvent(input$share_back, {
    selected_continent(NULL)
  })
  
  # ── Time Series — Total CO2 ─────────────────────────────────────────────────
  output$ts_total_plot <- renderPlotly({
    
    df <- carbon_data %>%
      filter(Entity == input$country) %>%
      arrange(Period) %>%
      mutate(
        Period_label = as.character(Period),
        is_nigeria   = Entity == "Nigeria",
        co2_bn       = co2_total / 1e9
      )
    
    req(nrow(df) > 0)
    
    p <- ggplot(df, aes(x = Period_label, y = co2_bn, group = 1,
                        text = paste0(
                          "<b>", Entity, "</b><br>",
                          "Period: ",      Period_label, "<br>",
                          "Total CO\u2082: ", round(co2_bn, 3), " Gt"
                        ))) +
      geom_area(fill = "skyblue", alpha = 0.15) +
      geom_line(color = "darkblue", linewidth = 1.8) +
      geom_point(
        aes(color = ifelse(Entity == "Nigeria", "Nigeria", "Other")),
        size = 4) +
      scale_color_manual(values = c("Nigeria" = "pink",
                                    "Other"   = "cyan"),
                         guide = "none") +
      scale_y_continuous(labels = scales::comma) +
      labs(
        title    = paste0("Total CO\u2082 Emissions \u2014 ", input$country),
        subtitle = "Billion tonnes per year",
        x = "Period", y = "CO\u2082 (Gt)"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title    = element_text(face = "bold"),
        plot.subtitle = element_text(color = "#777"),
        axis.text.x   = element_text(angle = 45, hjust = 1)
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(height = 380)
  })
  
  # ── Time Series — Per Capita CO2 ────────────────────────────────────────────
  output$ts_pc_plot <- renderPlotly({
    
    df <- carbon_data %>%
      filter(Entity == input$country) %>%
      arrange(Period) %>%
      mutate(Period_label = as.character(Period))
    
    req(nrow(df) > 0)
    
    p <- ggplot(df, aes(x = Period_label, y = co2_pc, group = 1,
                        text = paste0(
                          "<b>", Entity, "</b><br>",
                          "Period: ",            Period_label, "<br>",
                          "CO\u2082 per capita: ", round(co2_pc, 2), " tonnes/person"
                        ))) +
      geom_area(fill = "powderblue", alpha = 0.12) +
      geom_line(color = "thistle", linewidth = 1.8) +
      geom_point(
        aes(color = ifelse(Entity == "Nigeria", "Nigeria", "Other")),
        size = 4) +
      scale_color_manual(values = c("Nigeria" = "lightblue",
                                    "Other"   = "palevioletred"),
                         guide = "none") +
      scale_y_continuous(labels = scales::comma) +
      labs(
        title    = paste0("CO\u2082 per Capita \u2014 ", input$country),
        subtitle = "Tonnes of CO\u2082 per person per year",
        x = "Period", y = "CO\u2082 per capita (t)"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title    = element_text(face = "bold"),
        plot.subtitle = element_text(color = "#777"),
        axis.text.x   = element_text(angle = 45, hjust = 1)
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(height = 380)
  })
  # ── Cross-Country ────────────────────────────────────────────────────────────
  output$cc_plot <- renderPlotly({
    
    df <- ghg_data %>%
      filter(Period == selected_period()) %>%
      mutate(
        Value   = Responsibility_Gap,
        Percent = (abs(Value) / sum(abs(Value), na.rm = TRUE)) * 100
      )
    
    n <- if (input$rank_type == "top10") 10
    else if (input$rank_type == "bottom10") 10
    else input$custom_n
    
    go_top <- (input$rank_type == "top10") ||
      (input$rank_type == "custom" && input$custom_direction == "top")
    
    df_ranked <- if (go_top) {
      df %>% arrange(desc(Value)) %>% slice_head(n = n)
    } else {
      df %>% arrange(Value) %>% slice_head(n = n)
    }
    
    # Always include Nigeria
    df_Nigeria <- df %>% filter(Entity == "Nigeria")
    df_final   <- bind_rows(df_ranked, df_Nigeria) %>%
      distinct(Entity, .keep_all = TRUE)
    
    plot_value <- if (input$value_format == "percent") df_final$Percent else df_final$Value
    
    p <- ggplot(df_final, aes(
      x    = reorder(Entity, plot_value),
      y    = plot_value,
      fill = ifelse(Entity == "Nigeria", "Nigeria", "Others"),
      text = paste0(Entity,
                    "<br>Responsibility Gap: ", scales::comma(Value),
                    "<br>Share: ", round(Percent, 2), "%")
    )) +
      geom_col() +
      coord_flip() +
      scale_fill_manual(values = c("Nigeria" = "red", "Others" = "steelblue")) +
      labs(
        y    = ifelse(input$value_format == "percent",
                      "Percentage Share (%)", "Methane Responsibility (CO\u2082 eq)"),
        x    = "",
        fill = ""
      ) +
      theme_minimal(base_size = 14)
    
    ggplotly(p, tooltip = "text") %>% layout(height = 600)
  })
  
  # ── OUTLIER DETECTION ──────────────────────────────────────────────────────
  
  # Reactive: clean dataset for outlier analysis
  outlier_df <- reactive({
    
    # Aggregates to exclude
    agg_patterns <- c("GCP","WB","excl","income","OECD","Antarctica",
                      "International")
    agg_exact    <- c("World","Asia","Europe","Africa",
                      "North America","South America","Oceania")
    
    df <- carbon_data %>%
      filter(Period == selected_period()) %>%
      filter(!Entity %in% agg_exact) %>%
      filter(!grepl(paste(agg_patterns, collapse="|"), Entity)) %>%
      filter(!is.na(gdp_pc), !is.na(co2_pc),
             co2_pc > 0, gdp_pc > 0)
    
    # Log-log regression residual: how dirty/clean vs expected for their GDP
    log_gdp <- log(df$gdp_pc)
    log_co2 <- log(df$co2_pc)
    fit      <- lm(log_co2 ~ log_gdp)
    df$residual      <- residuals(fit)
    df$predicted_co2 <- exp(fitted(fit))
    
    # Responsibility gap per capita (from ghg_data)
    gap_df <- ghg_data %>%
      filter(Period == selected_period()) %>%
      select(Entity, Responsibility_Gap, prod_methane, cons_methane)
    
    df <- df %>% left_join(gap_df, by = "Entity")
    
    # Classify each country
    thresh <- input$outlier_threshold
    df <- df %>%
      mutate(
        efficiency_label = case_when(
          residual < -thresh  ~ "Cleaner than expected",
          residual >  thresh  ~ "Dirtier than expected",
          TRUE                ~ "Typical"
        ),
        gap_label = case_when(
          !is.na(Responsibility_Gap) &
            Responsibility_Gap >  abs(mean(Responsibility_Gap, na.rm=TRUE)) +
            thresh * sd(Responsibility_Gap, na.rm=TRUE) ~ "Hidden importer",
          !is.na(Responsibility_Gap) &
            Responsibility_Gap < -(abs(mean(Responsibility_Gap, na.rm=TRUE)) +
                                     thresh * sd(Responsibility_Gap, na.rm=TRUE)) ~ "Pollution exporter",
          TRUE ~ "Balanced"
        ),
        is_Nigeria = Entity == "Nigeria"
      )
    df
  })
  
  # ── Scatter plot ────────────────────────────────────────────────────────────
  output$outlier_scatter <- renderPlotly({
    
    df <- outlier_df()
    req(nrow(df) > 0)
    
    if (input$outlier_metric == "efficiency") {
      
      # Filter by scope
      df_plot <- switch(input$outlier_scope,
                        "over"  = df %>% filter(efficiency_label == "Cleaner than expected"),
                        "under" = df %>% filter(efficiency_label == "Dirtier than expected"),
                        df
      )
      
      color_map <- c(
        "Cleaner than expected" = "#2ca02c",
        "Dirtier than expected" = "#d62728",
        "Typical"               = "#aaaaaa"
      )
      
      # Trend line points
      gdp_seq    <- seq(min(df$gdp_pc), max(df$gdp_pc), length.out = 100)
      log_fit    <- lm(log(co2_pc) ~ log(gdp_pc), data = df)
      trend_co2  <- exp(predict(log_fit,
                                newdata = data.frame(gdp_pc = gdp_seq)))
      
      p <- ggplot() +
        # Trend line
        geom_line(
          data = data.frame(gdp_pc = gdp_seq, co2_pc = trend_co2),
          aes(x = gdp_pc, y = co2_pc),
          color = "#333333", linewidth = 1, linetype = "dashed"
        ) +
        # All countries
        geom_point(
          data = df_plot %>% filter(!is_Nigeria),
          aes(x       = gdp_pc,
              y       = co2_pc,
              color   = efficiency_label,
              size    = co2_total,
              text    = paste0(
                "<b>", Entity, "</b><br>",
                "GDP per capita: $", scales::comma(round(gdp_pc)),  "<br>",
                "CO2 per capita: ",  round(co2_pc, 2), " t<br>",
                "Expected CO2: ",    round(predicted_co2, 2), " t<br>",
                "Deviation: ",       round(residual, 2), "<br>",
                "Status: ",          efficiency_label
              )),
          alpha = 0.75
        ) +
        # Nigeria always on top
        geom_point(
          data  = df_plot %>% filter(is_Nigeria),
          aes(x = gdp_pc, y = co2_pc,
              text = paste0(
                "<b>Nigeria</b><br>",
                "GDP per capita: $", scales::comma(round(gdp_pc)),  "<br>",
                "CO2 per capita: ",  round(co2_pc, 2), " t<br>",
                "Expected CO2: ",    round(predicted_co2, 2), " t<br>",
                "Deviation: ",       round(residual, 2), "<br>",
                "Status: ",          efficiency_label
              )),
          color = "red", size = 5, shape = 18
        ) +
        scale_color_manual(values = color_map) +
        scale_size_continuous(range = c(2, 10), guide = "none") +
        scale_x_log10(labels = scales::dollar_format()) +
        scale_y_log10(labels = scales::comma) +
        labs(
          title  = "Emission Efficiency: GDP vs CO2 per Capita",
          x      = "GDP per Capita (log scale)",
          y      = "CO2 per Capita — tonnes (log scale)",
          color  = "Country Type",
          caption = "Dashed line = expected emissions given GDP. Points above = dirtier than expected."
        ) +
        theme_minimal(base_size = 13) +
        theme(plot.title = element_text(face = "bold"))
      
    } else {
      
      # Responsibility gap view
      df_plot <- df %>% filter(!is.na(Responsibility_Gap))
      
      df_plot <- switch(input$outlier_scope,
                        "over"  = df_plot %>% filter(gap_label == "Hidden importer"),
                        "under" = df_plot %>% filter(gap_label == "Pollution exporter"),
                        df_plot
      )
      
      color_map2 <- c(
        "Hidden importer"   = "#1f77b4",
        "Pollution exporter"= "#ff7f0e",
        "Balanced"          = "#aaaaaa"
      )
      
      p <- ggplot(df_plot,
                  aes(x    = prod_methane / 1e9,
                      y    = cons_methane / 1e9,
                      color = gap_label,
                      text  = paste0(
                        "<b>", Entity, "</b><br>",
                        "Production Methane: ", round(prod_methane/1e9, 2), " Gt<br>",
                        "Consumption Methane: ", round(cons_methane/1e9, 2), " Gt<br>",
                        "Responsibility Gap: ",
                        scales::comma(round(Responsibility_Gap/1e6)), " Mt<br>",
                        "Type: ", gap_label
                      ))) +
        geom_abline(slope = 1, intercept = 0,
                    color = "#333333", linewidth = 1, linetype = "dashed") +
        geom_point(data = df_plot %>% filter(!is_Nigeria),
                   aes(size = abs(Responsibility_Gap)), alpha = 0.75) +
        geom_point(data = df_plot %>% filter(is_Nigeria),
                   color = "red", size = 5, shape = 18) +
        scale_color_manual(values = color_map2) +
        scale_size_continuous(range = c(2, 10), guide = "none") +
        scale_x_log10(labels = scales::comma) +
        scale_y_log10(labels = scales::comma) +
        labs(
          title   = "Responsibility Gap: Production vs Consumption Emissions",
          x       = "Production-based Methane (Gt, log scale)",
          y       = "Consumption-based Methane (Gt, log scale)",
          color   = "Country Type",
          caption = "Above diagonal = imports emissions (hidden importer). Below = exports emissions."
        ) +
        theme_minimal(base_size = 13) +
        theme(plot.title = element_text(face = "bold"))
    }
    
    ggplotly(p, tooltip = "text") %>% layout(height = 500)
  })
  
  # ── Summary cards ───────────────────────────────────────────────────────────
  output$outlier_clean_list <- renderUI({
    top <- outlier_df() %>%
      filter(efficiency_label == "Cleaner than expected") %>%
      arrange(residual) %>%
      slice_head(n = 5)
    tags$ul(style = "padding-left:16px; margin:6px 0;",
            lapply(seq_len(nrow(top)), function(i) {
              tags$li(paste0(top$Entity[i],
                             " ($", scales::comma(round(top$gdp_pc[i])),
                             ", ", round(top$co2_pc[i], 1), "t)"))
            })
    )
  })
  
  output$outlier_dirty_list <- renderUI({
    top <- outlier_df() %>%
      filter(efficiency_label == "Dirtier than expected") %>%
      arrange(desc(residual)) %>%
      slice_head(n = 5)
    tags$ul(style = "padding-left:16px; margin:6px 0;",
            lapply(seq_len(nrow(top)), function(i) {
              tags$li(paste0(top$Entity[i],
                             " ($", scales::comma(round(top$gdp_pc[i])),
                             ", ", round(top$co2_pc[i], 1), "t)"))
            })
    )
  })
  
  output$outlier_importers_list <- renderUI({
    top <- outlier_df() %>%
      filter(!is.na(Responsibility_Gap)) %>%
      arrange(desc(Responsibility_Gap)) %>%
      slice_head(n = 5)
    tags$ul(style = "padding-left:16px; margin:6px 0;",
            lapply(seq_len(nrow(top)), function(i) {
              tags$li(paste0(top$Entity[i], " (+",
                             scales::comma(round(top$Responsibility_Gap[i]/1e6)),
                             " Mt)"))
            })
    )
  })
  
  output$outlier_exporters_list <- renderUI({
    top <- outlier_df() %>%
      filter(!is.na(Responsibility_Gap)) %>%
      arrange(Responsibility_Gap) %>%
      slice_head(n = 5)
    tags$ul(style = "padding-left:16px; margin:6px 0;",
            lapply(seq_len(nrow(top)), function(i) {
              tags$li(paste0(top$Entity[i], " (",
                             scales::comma(round(top$Responsibility_Gap[i]/1e6)),
                             " Mt)"))
            })
    )
  })
  
  # ── Nigeria context box ─────────────────────────────────────────────────────
  output$Nigeria_outlier_context <- renderUI({
    vn <- outlier_df() %>% filter(Entity == "Nigeria")
    req(nrow(vn) > 0)
    
    eff_status <- vn$efficiency_label[1]
    gap_val    <- if (!is.na(vn$Responsibility_Gap[1]))
      paste0(scales::comma(round(vn$Responsibility_Gap[1]/1e6)), " Mt")
    else "N/A"
    deviation  <- round(vn$residual[1], 3)
    
    eff_color <- if (eff_status == "Cleaner than expected") "#2ca02c"
    else if (eff_status == "Dirtier than expected") "#d62728"
    else "#888888"
    
    tagList(
      tags$p(
        "Emission Efficiency: ",
        tags$b(style = paste0("color:", eff_color), eff_status),
        paste0(" (deviation from trend: ", deviation, ")")
      ),
      tags$p(
        "GDP per capita: ",
        tags$b(paste0("$", scales::comma(round(vn$gdp_pc[1])))),
        " | CO2 per capita: ",
        tags$b(paste0(round(vn$co2_pc[1], 2), " tonnes"))
      ),
      tags$p(
        "Responsibility Gap: ",
        tags$b(gap_val),
        if (!is.na(vn$Responsibility_Gap[1]) && vn$Responsibility_Gap[1] > 0)
          " → Nigeria consumes more than it produces (net importer of emissions)"
        else
          " → Nigeria produces more than it consumes (net exporter of emissions)"
      )
    )
  })
  
 
}

shinyApp(ui, server)
