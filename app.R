# app.R  — DATA2002 Shiny app (Xi Huang 540223060)
suppressPackageStartupMessages({
  library(shiny); library(tidyverse); library(readxl); library(janitor); library(stringr)
  library(bslib); library(DT)
})

num_or_mean <- function(x){
  out <- str_extract_all(as.character(x), "\\d+(?:\\.\\d+)?")
  vapply(out, function(v) if(length(v)==0) NA_real_ else mean(as.numeric(v)), numeric(1))
}

clean_survey <- function(path = "DATA2x02_survey_2025_Responses.xlsx"){
  raw <- readxl::read_excel(path) |> janitor::clean_names()
  sleep_num <- num_or_mean(raw$how_much_sleep_do_you_get_on_avg_per_day)
  sleep_hours <- dplyr::case_when(
    sleep_num > 24 & sleep_num <= 24*60 ~ sleep_num/60,
    sleep_num > 24*60 & sleep_num <= 24*60*60 ~ sleep_num/3600,
    sleep_num > 0 & sleep_num <= 24 ~ sleep_num,
    TRUE ~ NA_real_
  )
  dat <- raw |>
    mutate(
      sleep_hours = sleep_hours,
      pick_random = as.integer(readr::parse_number(as.character(pick_a_number_at_random_between_1_and_10_inclusive))),
      fav_number  = as.integer(readr::parse_number(as.character(what_is_your_favourite_number_between_1_and_10_inclusive))),
      unit        = factor(which_unit_are_you_enrolled_in, levels = c("DATA2002","DATA2902")),
      used_r      = case_when(
        str_to_lower(have_you_ever_used_r_before_starting_data2x02) %in% c("yes","y") ~ "Yes",
        str_to_lower(have_you_ever_used_r_before_starting_data2x02) %in% c("no","n") ~ "No",
        TRUE ~ NA_character_
      ),
      wam         = suppressWarnings(readr::parse_number(as.character(what_is_your_wam_weighted_average_mark))),
      on_time     = do_you_submit_assignments_on_time
    )
  dat
}

is_categorical <- function(x) is.character(x) || is.factor(x) || (is.numeric(x) && dplyr::n_distinct(x, na.rm=TRUE) <= 10)

cramers_v <- function(tab){
  chi <- suppressWarnings(chisq.test(tab, correct = FALSE))
  n <- sum(tab); r <- nrow(tab); c <- ncol(tab)
  v <- sqrt(as.numeric(chi$statistic)/(n * (min(r-1, c-1))))
  as.numeric(v)
}

dat0 <- clean_survey()
cat_vars <- names(dat0)[vapply(dat0, is_categorical, logical(1))]
num_vars <- names(dat0)[vapply(dat0, is.numeric, logical(1))]

ui <- navbarPage(
  title = "DATA2002 – Survey tests",
  theme = bslib::bs_theme(bootswatch = "flatly"),
  tabPanel("Two categorical",
           sidebarLayout(
             sidebarPanel(width = 4,
               selectInput("cat1", "Categorical variable A", choices = cat_vars, selected = "used_r"),
               selectInput("cat2", "Categorical variable B", choices = cat_vars, selected = "unit"),
               checkboxInput("prop", "Show proportions (stacked 100%)", TRUE)
             ),
             mainPanel(
               fluidRow(
                 column(6, plotOutput("catPlot", height = 350)),
                 column(6, plotOutput("residPlot", height = 350))
               ),
               fluidRow(
                 column(6, DTOutput("catTable")),
                 column(6, verbatimTextOutput("catTest"))
               )
             )
           )),
  tabPanel("Numeric vs categorical",
           sidebarLayout(
             sidebarPanel(width = 4,
               selectInput("num", "Numeric variable", choices = num_vars, selected = "sleep_hours"),
               selectInput("cat", "Grouping variable", choices = cat_vars, selected = "unit"),
               uiOutput("levelPick"),
               checkboxInput("jitter", "Show points", TRUE)
             ),
             mainPanel(
               fluidRow(
                 column(6, plotOutput("boxPlot", height = 350)),
                 column(6, plotOutput("qqPlot", height = 350))
               ),
               fluidRow(
                 column(6, verbatimTextOutput("tAssumptions")),
                 column(6, verbatimTextOutput("tOutput"))
               )
             )
           )),
  tabPanel("About",
           div(class="p-3",
               h4("DATA2002 Shiny app"),
               p("Loads the class survey, lets you select variables, shows visuals, and runs suitable hypothesis tests."),
               p("Built by Xi Huang (540223060)."))
  )
)

server <- function(input, output, session){
  dat <- reactive({ dat0 })

  cat_data <- reactive({
    req(input$cat1, input$cat2)
    d <- dat() |> select(all_of(c(input$cat1, input$cat2))) |> drop_na()
    names(d) <- c("A","B")
    d
  })

  output$catPlot <- renderPlot({
    d <- cat_data()
    if (input$prop) {
      d |> ggplot(aes(x = A, fill = B)) +
        geom_bar(position = "fill") +
        scale_y_continuous(labels = scales::percent) +
        labs(x = "A", y = "Proportion", fill = "B", title = "A by B (proportions)") +
        theme_minimal(base_size = 13)
    } else {
      d |> ggplot(aes(x = A, fill = B)) +
        geom_bar(position = "dodge") +
        labs(x = "A", y = "Count", fill = "B", title = "A by B (counts)") +
        theme_minimal(base_size = 13)
    }
  })

  output$residPlot <- renderPlot({
    d <- cat_data()
    tab <- table(d$A, d$B)
    chi <- suppressWarnings(chisq.test(tab, correct = FALSE))
    res <- chi$stdres
    df <- as.data.frame(as.table(res))
    names(df) <- c("A","B","std_resid")
    ggplot(df, aes(A, B, fill = std_resid)) +
      geom_tile() +
      scale_fill_gradient2() +
      labs(title = "Standardised residuals (chi-square)", x = "A", y = "B") +
      theme_minimal(base_size = 13)
  })

  output$catTable <- renderDT({
    d <- cat_data()
    datatable(table(d$A, d$B), options = list(dom='tip', pageLength = 5))
  })

  output$catTest <- renderPrint({
    d <- cat_data()
    tab <- table(d$A, d$B)
    n <- sum(tab)
    exp <- suppressWarnings(chisq.test(tab, correct = FALSE)$expected)
    small <- any(exp < 5)
    if (small) {
      test <- fisher.test(tab)
      cat("Fisher's exact test (expected counts < 5 in some cells)\n")
      print(test)
    } else {
      test <- chisq.test(tab, correct = FALSE)
      cat("Chi-squared test of independence\n")
      print(test)
    }
    cat("\nCramer's V:", round(cramers_v(tab), 3), " (n =", n, ")\n")
  })

  output$levelPick <- renderUI({
    req(input$cat)
    levs <- dat()[[input$cat]] |> as.factor() |> levels()
    if (length(levs) <= 2) return(NULL)
    tagList(
      selectInput("lvlA", "Level 1", choices = levs, selected = levs[1]),
      selectInput("lvlB", "Level 2", choices = levs, selected = levs[2])
    )
  })

  two_group_data <- reactive({
    req(input$num, input$cat)
    d <- dat() |> select(all_of(c(input$num, input$cat))) |> drop_na()
    names(d) <- c("y","g")
    if (!is.null(input$lvlA) && !is.null(input$lvlB)) {
      d <- d |> filter(g %in% c(input$lvlA, input$lvlB)) |> droplevels()
    }
    if (dplyr::n_distinct(d$g) > 2) {
      keep <- names(sort(table(d$g), decreasing = TRUE))[1:2]
      d <- d |> filter(g %in% keep) |> droplevels()
    }
    d
  })

  output$boxPlot <- renderPlot({
    d <- two_group_data()
    ggplot(d, aes(x = g, y = y)) +
      { if (isTRUE(input$jitter)) geom_jitter(width = .12, alpha=.4) } +
      geom_boxplot(outlier.shape = NA, fill = NA) +
      labs(x = "", y = input$num, title = paste(input$num, "by", input$cat)) +
      theme_minimal(base_size = 13)
  })

  output$qqPlot <- renderPlot({
    d <- two_group_data()
    glev <- levels(as.factor(d$g))
    par(mfrow=c(1,2), mar=c(4,4,2,1))
    for (lv in glev){
      qqnorm(d$y[d$g==lv], main = paste("Q-Q:", lv)); qqline(d$y[d$g==lv])
    }
  })

  output$tAssumptions <- renderPrint({
    d <- two_group_data()
    glev <- levels(as.factor(d$g))
    a <- list()
    for (lv in glev){
      x <- d$y[d$g==lv]
      if (length(x) >= 3 && length(x) <= 5000) {
        a[[lv]] <- tryCatch(shapiro.test(x), error=function(e) NULL)
      } else a[[lv]] <- NULL
    }
    bt <- tryCatch(var.test(y ~ g, data=d), error=function(e) NULL)
    cat("Normality (Shapiro) by group:\n"); print(a)
    cat("\nVariance test (F-test):\n"); print(bt)
  })

  output$tOutput <- renderPrint({
    d <- two_group_data()
    d <- droplevels(d)
    glev <- levels(d$g)
    x <- d$y[d$g==glev[1]]; y <- d$y[d$g==glev[2]]
    tt <- t.test(x, y, var.equal = FALSE)
    mn1 <- mean(x); mn2 <- mean(y)
    s1 <- sd(x); s2 <- sd(y)
    sp <- sqrt(((length(x)-1)*s1^2 + (length(y)-1)*s2^2) / (length(x)+length(y)-2))
    d_cohen <- (mn1 - mn2)/sp
    out <- list(test = tt, effect_size_d = d_cohen, n = c(length(x), length(y)), means = c(mn1, mn2))
    print(out)
  })
}

shinyApp(ui, server)