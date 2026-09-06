## ============================================================================
## What Makes an Online Shopper Buy? -- Interactive Companion App
## Advanced Analytics Group Project
##
## To run:  install.packages(c("shiny","shinythemes","ggplot2","dplyr",
##                              "glmnet","DT","plotly","scales"))
##          shiny::runApp("app.R")
##
## To deploy: rsconnect::deployApp() after setting up a shinyapps.io account,
##            with online_shoppers_intention.csv in the same folder as app.R.
## ============================================================================

library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(glmnet)
library(DT)
library(plotly)
library(scales)

# ---------------------------------------------------------------------------
# 1. DATA LOAD AND FEATURE ENGINEERING (mirrors analysis_final.R exactly)
# ---------------------------------------------------------------------------
d <- read.csv("online_shoppers_intention.csv", stringsAsFactors = FALSE)

d$lAdmin <- log1p(d$Administrative)
d$lInfo  <- log1p(d$Informational)
d$lProd  <- log1p(d$ProductRelated)
d$sExit  <- sqrt(d$ExitRates)
d$Visitor <- factor(d$VisitorType)
d$Weekend <- as.logical(d$Weekend)
d$Month   <- factor(d$Month)
d$OS      <- factor(d$OperatingSystems)
d$Br      <- factor(d$Browser)
d$TT      <- factor(d$TrafficType)
d$Region_f <- factor(d$Region)
d$y       <- as.integer(d$Revenue)

MONTH_LEVELS   <- levels(d$Month)
VISITOR_LEVELS <- levels(d$Visitor)

# ---------------------------------------------------------------------------
# 2. TRAIN / TEST SPLIT (70/30, stratified by class, same seed as the report)
# ---------------------------------------------------------------------------
set.seed(42)
i1 <- which(d$y == 1); i0 <- which(d$y == 0)
tr <- c(sample(i1, round(.7 * length(i1))), sample(i0, round(.7 * length(i0))))
d$train <- FALSE
d$train[tr] <- TRUE

d_tr <- d[d$train, ]
d_te <- d[!d$train, ]

# ---------------------------------------------------------------------------
# 3. METHOD 1 -- MLR (fit on ALL 12,330 sessions, as in the report)
# ---------------------------------------------------------------------------
m_lm <- lm(sExit ~ lAdmin + lInfo + lProd + SpecialDay + Visitor + Weekend + Month, data = d)

# ---------------------------------------------------------------------------
# 4. METHOD 2 -- Logistic Regression (fit on the 70% training split)
# ---------------------------------------------------------------------------
m_glm <- glm(y ~ lProd + lAdmin + lInfo + ExitRates + PageValues + SpecialDay +
               Visitor + Weekend + Month, data = d_tr, family = binomial())

pA <- predict(m_glm, newdata = d_te, type = "response")
yte <- d_te$y

roc_auc <- function(p, y) {
  o <- order(-p); y <- y[o]
  tpr <- cumsum(y) / sum(y); fpr <- cumsum(1 - y) / sum(1 - y)
  sum(diff(c(0, fpr)) * (c(0, tpr)[-1] + head(c(0, tpr), -1)) / 2)
}
pr_auc <- function(p, y) {
  o <- order(-p); y <- y[o]
  tp <- cumsum(y); fp <- cumsum(1 - y)
  prec <- tp / (tp + fp); rec <- tp / sum(y)
  keep <- !duplicated(rec, fromLast = TRUE)
  rec <- c(0, rec[keep]); prec <- c(prec[keep][1], prec[keep])
  sum(diff(rec) * (prec[-1] + head(prec, -1)) / 2)
}

# ---------------------------------------------------------------------------
# 5. METHOD 3 -- LASSO (glmnet, alpha = 1, fit on the 70% training split)
# ---------------------------------------------------------------------------
f_x <- ~ lProd + lAdmin + lInfo + BounceRates + ProductRelated_Duration + ExitRates +
  PageValues + SpecialDay + Weekend + Month + Visitor + OS + Br + TT + Region_f
X    <- model.matrix(f_x, data = d)[, -1]
Xtr  <- X[d$train, ]; Xte <- X[!d$train, ]
ytr  <- d$y[d$train]

set.seed(42)
cvL <- cv.glmnet(Xtr, ytr, family = "binomial", alpha = 1, type.measure = "auc", nfolds = 10)
fit_path <- glmnet(Xtr, ytr, family = "binomial", alpha = 1)

pL1 <- predict(cvL, Xte, s = "lambda.1se", type = "response")[, 1]

# Coefficient path, standardized so scale doesn't distort which line looks "biggest"
betas_raw  <- as.matrix(t(as.matrix(coef(fit_path)))[, -1])
x_sd       <- apply(Xtr, 2, sd)
betas_std  <- sweep(betas_raw, 2, x_sd[colnames(betas_raw)], `*`)
lambda_seq <- fit_path$lambda
log_lambda_seq <- log(lambda_seq)

TOP7 <- union(c("PageValues", "ExitRates", "BounceRates"),
              names(sort(colSums(abs(betas_std)), decreasing = TRUE)))[1:7]

# ---------------------------------------------------------------------------
# 6. EDA HELPERS
# ---------------------------------------------------------------------------
CAT_VARS <- c("Month", "VisitorType", "TrafficType", "OperatingSystems", "Browser", "Weekend", "Region")

eta_sq_table <- function(varname) {
  f <- as.formula(paste("y ~ factor(", varname, ")"))
  a <- aov(f, data = d)
  s <- summary(a)[[1]]
  ss_between <- s$`Sum Sq`[1]; ss_total <- sum(s$`Sum Sq`)
  data.frame(
    F_stat = round(s$`F value`[1], 2),
    df1 = s$Df[1], df2 = s$Df[2],
    p_value = signif(s$`Pr(>F)`[1], 3),
    eta_sq = round(ss_between / ss_total, 4)
  )
}

purchase_rate_by <- function(varname) {
  d %>% group_by(.data[[varname]]) %>%
    summarise(sessions = n(), purchase_rate = mean(y), .groups = "drop") %>%
    arrange(desc(purchase_rate))
}

NUM_VARS <- c("Administrative", "Informational", "ProductRelated", "ProductRelated_Duration",
              "BounceRates", "ExitRates", "PageValues", "SpecialDay")
CORR_MAT <- round(cor(d[, NUM_VARS]), 2)

# ---------------------------------------------------------------------------
# 7. UI
# ---------------------------------------------------------------------------
ui <- navbarPage(
  title = "What Makes an Online Shopper Buy?",
  theme = shinytheme("flatly"),
  collapsible = TRUE,

  # ---- Tab 1: Welcome ----
  tabPanel("Welcome",
    fluidPage(
      br(),
      h2("What Makes an Online Shopper Buy?"),
      h4(em("From Friction to Conversion: A Three-Method Analysis of Online Shopper Behaviour"), style="color:#666;"),
      hr(),
      fluidRow(
        column(3, wellPanel(h3(textOutput("kpi_sessions")), p("Sessions"))),
        column(3, wellPanel(h3(textOutput("kpi_attrs")), p("Attributes"))),
        column(3, wellPanel(h3(textOutput("kpi_purchase")), p("Purchase Rate"))),
        column(3, wellPanel(h3(textOutput("kpi_months")), p("Months Missing")))
      ),
      hr(),
      h3("Three Linked Business Questions"),
      fluidRow(
        column(4, wellPanel(style="background:#eaf2fb;",
          h4("BQ1"), strong("Method 1: MLR"),
          p("What browsing behaviour lowers a visitor's exit rate?"))),
        column(4, wellPanel(style="background:#e8f6ef;",
          h4("BQ2 (main question)"), strong("Method 2: Logistic Regression"),
          p("Does that behaviour predict an actual purchase?"))),
        column(4, wellPanel(style="background:#fdf1e7;",
          h4("BQ3"), strong("Method 3: LASSO"),
          p("Does the data agree with the variables we picked by hand?")))
      ),
      hr(),
      h3("Team Members"),
      DTOutput("team_table")
    )
  ),

  # ---- Tab 2: EDA ----
  tabPanel("Explore the Data",
    sidebarLayout(
      sidebarPanel(
        selectInput("catvar", "Pick a category variable:", choices = CAT_VARS, selected = "Month"),
        helpText("This redraws the purchase-rate chart and the ANOVA test below."),
        hr(),
        h5("One-way ANOVA result"),
        tableOutput("eta_table")
      ),
      mainPanel(
        plotlyOutput("purchase_rate_plot", height = "320px"),
        hr(),
        h4("Correlation Heatmap of Numeric Variables"),
        plotlyOutput("corr_heatmap", height = "420px"),
        hr(),
        h4("Descriptive Statistics"),
        DTOutput("stats_table")
      )
    )
  ),

  # ---- Tab 3: Method 1 MLR ----
  tabPanel("Method 1: MLR",
    sidebarLayout(
      sidebarPanel(
        h4("Simulate a Session"),
        sliderInput("m1_prod", "Product pages viewed:", 0, 100, 20),
        sliderInput("m1_admin", "Administrative pages viewed:", 0, 30, 2),
        sliderInput("m1_info", "Informational pages viewed:", 0, 20, 1),
        sliderInput("m1_special", "SpecialDay closeness (0-1):", 0, 1, 0, step = 0.1),
        selectInput("m1_visitor", "Visitor type:", choices = VISITOR_LEVELS),
        checkboxInput("m1_weekend", "Weekend session", FALSE),
        selectInput("m1_month", "Month:", choices = MONTH_LEVELS, selected = "Nov"),
        hr(),
        h4("Predicted Exit Rate"),
        h2(textOutput("m1_prediction"), style = "color:#1F4E78;")
      ),
      mainPanel(
        h4("What Drives Exit Rate: Coefficients"),
        plotOutput("m1_coef_plot", height = "320px"),
        p(em("R-squared = 0.493, Adjusted R-squared = 0.492, n = 12,330 (every session).")),
        hr(),
        h4("Is This the Leanest Model? AIC / BIC Check"),
        DTOutput("m1_aicbic_table")
      )
    )
  ),

  # ---- Tab 4: Method 2 Logistic ----
  tabPanel("Method 2: Logistic Regression",
    sidebarLayout(
      sidebarPanel(
        sliderInput("threshold", "Classification threshold:", 0, 1, 0.5, step = 0.01),
        helpText("Drag to see the confusion matrix and metrics update live, on the same 30% holdout used in the report."),
        hr(),
        h4("At this threshold"),
        tableOutput("cm_metrics")
      ),
      mainPanel(
        h4("Confusion Matrix (Holdout Set)"),
        tableOutput("cm_table"),
        hr(),
        fluidRow(
          column(6, plotOutput("roc_plot", height = "320px")),
          column(6, plotOutput("pr_plot", height = "320px"))
        ),
        hr(),
        h4("Logistic Regression Coefficients"),
        DTOutput("m2_coef_table")
      )
    )
  ),

  # ---- Tab 5: Method 3 LASSO ----
  tabPanel("Method 3: LASSO",
    sidebarLayout(
      sidebarPanel(
        sliderInput("lambda_pos", "Penalty strength (low to high):",
                    min = 1, max = length(lambda_seq), value = which.min(abs(lambda_seq - cvL$lambda.1se)), step = 1),
        helpText("As the penalty rises, weaker predictors shrink to zero."),
        hr(),
        h4("Predictors Remaining"),
        h2(textOutput("lasso_nonzero"), style = "color:#1F4E78;"),
        p(textOutput("lasso_which"))
      ),
      mainPanel(
        h4("LASSO Coefficient Path"),
        plotOutput("lasso_path_plot", height = "420px"),
        p(em("Dashed lines mark lambda.min and lambda.1se, the two penalty strengths reported in the write-up.")),
        hr(),
        h4("Predictors Surviving at the Selected Penalty"),
        DTOutput("lasso_surv_table")
      )
    )
  ),

  # ---- Tab 6: Try It Yourself ----
  tabPanel("Try It Yourself",
    sidebarLayout(
      sidebarPanel(
        h4("Describe a Browsing Session"),
        sliderInput("t_prod", "Product pages viewed:", 0, 100, 25),
        sliderInput("t_admin", "Administrative pages viewed:", 0, 30, 2),
        sliderInput("t_info", "Informational pages viewed:", 0, 20, 1),
        sliderInput("t_pagevalues", "PageValues:", 0, 200, 20),
        sliderInput("t_special", "SpecialDay closeness (0-1):", 0, 1, 0, step = 0.1),
        selectInput("t_visitor", "Visitor type:", choices = VISITOR_LEVELS),
        checkboxInput("t_weekend", "Weekend session", FALSE),
        selectInput("t_month", "Month:", choices = MONTH_LEVELS, selected = "Nov"),
        actionButton("t_go", "Predict", class = "btn-primary")
      ),
      mainPanel(
        h4("Step 1: Method 1 predicts how much friction this session carries"),
        h3(textOutput("t_exitrate"), style="color:#B5501C;"),
        hr(),
        h4("Step 2: Method 2 uses that friction, plus PageValues, to predict a purchase"),
        h3(textOutput("t_purchase_logit"), style="color:#0E7C5A;"),
        hr(),
        h4("For comparison: Method 3's lean 3-variable model says"),
        h3(textOutput("t_purchase_lasso"), style="color:#1F4E78;"),
        hr(),
        p(em("This is the same narrative thread as the written report: Method 1's output (exit rate) feeds directly into Method 2's input, and Method 3 checks the answer with an independent, automatic variable selection."))
      )
    )
  ),

  # ---- Tab 7: Summary ----
  tabPanel("Summary",
    fluidPage(
      br(),
      h3("Comparing the Three Methods"),
      tableOutput("summary_table"),
      hr(),
      h3("The Practical Conclusion"),
      p("PageValues, ExitRates and product-page depth carry almost all of the useful signal in this dataset. A retailer does not need to track every browsing metric to spot a likely buyer. A small set of page-quality and friction signals does most of the work, and three independent methods support this conclusion, not just one."),
      p(em("Limits: the classes are imbalanced (84.5% vs. 15.5%), which limits recall at the default threshold. PageValues is partly derived from completed transactions, so its live availability should be confirmed before deployment. The dataset is also missing January and April, so seasonal claims should be read with that gap in mind."))
    )
  )
)

# ---------------------------------------------------------------------------
# 8. SERVER
# ---------------------------------------------------------------------------
server <- function(input, output, session) {

  # ---- Tab 1 ----
  output$kpi_sessions <- renderText(format(nrow(d), big.mark = ","))
  output$kpi_attrs    <- renderText("18")
  output$kpi_purchase <- renderText(percent(mean(d$y), accuracy = 0.1))
  output$kpi_months   <- renderText("2 (Jan, Apr)")
  output$team_table <- renderDT({
    data.frame(
      `Roll No.` = c("2613054","2613017","2613060","2613040","2613011"),
      Name = c("Sudarsan S","Divyansh Sengar","Vibhu Verma","Saimanasa Chadalavada","Ashish Ranjan"),
      `IIMU Email ID` = c("sudarsans.dem2026@iimu.ac.in","divyanshsengar.dem2026@iimu.ac.in",
                           "vibhuverma.dem2026@iimu.ac.in","saimanasachadalavada.dem2026@iimu.ac.in",
                           "ashishranjan.dem2026@iimu.ac.in"),
      check.names = FALSE
    )
  }, options = list(dom = 't', paging = FALSE), rownames = FALSE)

  # ---- Tab 2: EDA ----
  output$purchase_rate_plot <- renderPlotly({
    dat <- purchase_rate_by(input$catvar)
    dat[[input$catvar]] <- factor(dat[[input$catvar]], levels = dat[[input$catvar]][order(-dat$purchase_rate)])
    p <- ggplot(dat, aes(x = .data[[input$catvar]], y = purchase_rate, text = paste0("Sessions: ", sessions))) +
      geom_col(fill = "#1F4E78") +
      scale_y_continuous(labels = percent) +
      labs(title = paste("Purchase Rate by", input$catvar), x = NULL, y = "Purchase rate") +
      theme_minimal()
    ggplotly(p, tooltip = c("y", "text"))
  })

  output$eta_table <- renderTable({
    eta_sq_table(input$catvar)
  }, digits = 4)

  output$corr_heatmap <- renderPlotly({
    m <- CORR_MAT
    plot_ly(x = colnames(m), y = rownames(m), z = m, type = "heatmap",
            colors = colorRamp(c("#2a78d6", "#f2f2f2", "#e34948")), zmin = -1, zmax = 1) %>%
      layout(xaxis = list(tickangle = -45))
  })

  output$stats_table <- renderDT({
    s <- d[, NUM_VARS]
    data.frame(
      Variable = NUM_VARS,
      Mean = round(sapply(s, mean), 2),
      Median = round(sapply(s, median), 2),
      SD = round(sapply(s, sd), 2),
      `% Zero` = round(sapply(s, function(x) mean(x == 0) * 100), 1),
      check.names = FALSE
    )
  }, options = list(dom = 't', paging = FALSE), rownames = FALSE)

  # ---- Tab 3: Method 1 MLR ----
  m1_newdata <- reactive({
    data.frame(
      lAdmin = log1p(input$m1_admin), lInfo = log1p(input$m1_info), lProd = log1p(input$m1_prod),
      SpecialDay = input$m1_special,
      Visitor = factor(input$m1_visitor, levels = VISITOR_LEVELS),
      Weekend = input$m1_weekend,
      Month = factor(input$m1_month, levels = MONTH_LEVELS)
    )
  })
  output$m1_prediction <- renderText({
    pred_sqrt <- predict(m_lm, newdata = m1_newdata())
    pred_exit <- max(0, pred_sqrt)^2
    percent(pred_exit, accuracy = 0.1)
  })
  output$m1_coef_plot <- renderPlot({
    cf <- summary(m_lm)$coefficients
    dat <- data.frame(term = rownames(cf), estimate = cf[, 1])
    dat <- dat[dat$term != "(Intercept)" & !grepl("^Month", dat$term), ]
    ggplot(dat, aes(x = reorder(term, estimate), y = estimate, fill = estimate > 0)) +
      geom_col() + coord_flip() +
      scale_fill_manual(values = c("TRUE" = "#0E7C5A", "FALSE" = "#B5501C"), guide = "none") +
      labs(x = NULL, y = "Coefficient (on sqrt(ExitRates))") + theme_minimal()
  })
  output$m1_aicbic_table <- renderDT({
    data.frame(
      Criterion = c("AIC", "BIC"),
      `Full model` = c(round(AIC(m_lm), 1), round(BIC(m_lm), 1)),
      check.names = FALSE
    )
  }, options = list(dom = 't', paging = FALSE), rownames = FALSE)

  # ---- Tab 4: Method 2 Logistic ----
  cm_reactive <- reactive({
    pred <- ifelse(pA >= input$threshold, 1, 0)
    tp <- sum(pred == 1 & yte == 1); fp <- sum(pred == 1 & yte == 0)
    fn <- sum(pred == 0 & yte == 1); tn <- sum(pred == 0 & yte == 0)
    list(tp = tp, fp = fp, fn = fn, tn = tn)
  })
  output$cm_table <- renderTable({
    cm <- cm_reactive()
    data.frame(
      ` ` = c("Predicted: No Purchase", "Predicted: Purchase"),
      `Actual: No Purchase` = c(cm$tn, cm$fp),
      `Actual: Purchase` = c(cm$fn, cm$tp),
      check.names = FALSE
    )
  })
  output$cm_metrics <- renderTable({
    cm <- cm_reactive()
    acc <- (cm$tp + cm$tn) / (cm$tp + cm$tn + cm$fp + cm$fn)
    prec <- if ((cm$tp + cm$fp) > 0) cm$tp / (cm$tp + cm$fp) else NA
    rec <- if ((cm$tp + cm$fn) > 0) cm$tp / (cm$tp + cm$fn) else NA
    f1 <- if (!is.na(prec) && !is.na(rec) && (prec + rec) > 0) 2 * prec * rec / (prec + rec) else NA
    data.frame(Metric = c("Accuracy", "Precision", "Recall", "F1"),
               Value = percent(c(acc, prec, rec, f1), accuracy = 0.1))
  }, colnames = FALSE)
  output$roc_plot <- renderPlot({
    o <- order(-pA); ys <- yte[o]
    tpr <- cumsum(ys) / sum(ys); fpr <- cumsum(1 - ys) / sum(1 - ys)
    df <- data.frame(fpr = c(0, fpr), tpr = c(0, tpr))
    cm <- cm_reactive()
    cur_fpr <- cm$fp / (cm$fp + cm$tn); cur_tpr <- cm$tp / (cm$tp + cm$fn)
    ggplot(df, aes(fpr, tpr)) + geom_line(color = "#1F4E78", linewidth = 1) +
      geom_abline(linetype = "dashed", color = "grey60") +
      geom_point(aes(x = cur_fpr, y = cur_tpr), color = "#B5501C", size = 4) +
      labs(title = paste0("ROC curve (AUC = ", round(roc_auc(pA, yte), 3), ")"), x = "False positive rate", y = "True positive rate") +
      theme_minimal()
  })
  output$pr_plot <- renderPlot({
    o <- order(-pA); ys <- yte[o]
    tp <- cumsum(ys); fp <- cumsum(1 - ys)
    prec <- tp / (tp + fp); rec <- tp / sum(ys)
    df <- data.frame(rec = rec, prec = prec)
    cm <- cm_reactive()
    cur_rec <- cm$tp / (cm$tp + cm$fn)
    cur_prec <- if ((cm$tp + cm$fp) > 0) cm$tp / (cm$tp + cm$fp) else 0
    ggplot(df, aes(rec, prec)) + geom_line(color = "#B5501C", linewidth = 1) +
      geom_hline(yintercept = mean(yte), linetype = "dashed", color = "grey60") +
      geom_point(aes(x = cur_rec, y = cur_prec), color = "#1F4E78", size = 4) +
      labs(title = paste0("Precision-Recall (PR-AUC = ", round(pr_auc(pA, yte), 3), ")"), x = "Recall", y = "Precision") +
      theme_minimal()
  })
  output$m2_coef_table <- renderDT({
    cf <- summary(m_glm)$coefficients
    data.frame(Predictor = rownames(cf), Coefficient = round(cf[, 1], 3), `p-value` = signif(cf[, 4], 3), check.names = FALSE)
  }, options = list(pageLength = 10), rownames = FALSE)

  # ---- Tab 5: Method 3 LASSO ----
  output$lasso_nonzero <- renderText({
    b <- coef(fit_path, s = lambda_seq[input$lambda_pos])[-1]
    sum(b != 0)
  })
  output$lasso_which <- renderText({
    b <- coef(fit_path, s = lambda_seq[input$lambda_pos])[-1, 1]
    nm <- names(b)[b != 0]
    if (length(nm) == 0) "none" else paste(head(nm, 6), collapse = ", ")
  })
  output$lasso_path_plot <- renderPlot({
    path_df <- reshape2::melt(data.frame(log_lambda = log_lambda_seq, betas_std[, TOP7]), id.vars = "log_lambda")
    ggplot(path_df, aes(log_lambda, value, color = variable)) +
      geom_line(linewidth = 0.9) +
      geom_vline(xintercept = log(cvL$lambda.min), linetype = "dashed", color = "grey50") +
      geom_vline(xintercept = log(cvL$lambda.1se), linetype = "dashed", color = "grey50") +
      geom_vline(xintercept = log(lambda_seq[input$lambda_pos]), color = "#B5501C", linewidth = 1) +
      labs(title = "Standardized coefficient path", x = "log(lambda)", y = "Standardized coefficient", color = NULL) +
      theme_minimal()
  })
  output$lasso_surv_table <- renderDT({
    b <- coef(fit_path, s = lambda_seq[input$lambda_pos])[-1, 1]
    b <- b[b != 0]
    if (length(b) == 0) return(data.frame(Predictor = character(0), Coefficient = numeric(0)))
    data.frame(Predictor = names(b), Coefficient = round(b, 4))[order(-abs(b)), ]
  }, options = list(dom = 't', paging = FALSE), rownames = FALSE)

  # ---- Tab 6: Try It Yourself ----
  observeEvent(input$t_go, {
    nd1 <- data.frame(
      lAdmin = log1p(input$t_admin), lInfo = log1p(input$t_info), lProd = log1p(input$t_prod),
      SpecialDay = input$t_special,
      Visitor = factor(input$t_visitor, levels = VISITOR_LEVELS),
      Weekend = input$t_weekend,
      Month = factor(input$t_month, levels = MONTH_LEVELS)
    )
    pred_exit <- max(0, predict(m_lm, newdata = nd1))^2

    nd2 <- data.frame(
      lProd = log1p(input$t_prod), lAdmin = log1p(input$t_admin), lInfo = log1p(input$t_info),
      ExitRates = pred_exit, PageValues = input$t_pagevalues, SpecialDay = input$t_special,
      Visitor = factor(input$t_visitor, levels = VISITOR_LEVELS),
      Weekend = input$t_weekend,
      Month = factor(input$t_month, levels = MONTH_LEVELS)
    )
    p_purchase <- predict(m_glm, newdata = nd2, type = "response")

    surv <- coef(cvL, s = "lambda.1se")[-1, 1]; surv <- surv[surv != 0]
    intercept <- coef(cvL, s = "lambda.1se")[1, 1]
    month_nov_flag <- as.numeric(input$t_month == "Nov")
    lin <- intercept
    if ("MonthNov" %in% names(surv)) lin <- lin + surv["MonthNov"] * month_nov_flag
    if ("lProd" %in% names(surv)) lin <- lin + surv["lProd"] * log1p(input$t_prod)
    if ("PageValues" %in% names(surv)) lin <- lin + surv["PageValues"] * input$t_pagevalues
    p_lasso <- 1 / (1 + exp(-lin))

    output$t_exitrate <- renderText(paste0("Predicted exit rate: ", percent(pred_exit, accuracy = 0.1)))
    output$t_purchase_logit <- renderText(paste0("Predicted purchase probability (Logistic): ", percent(p_purchase, accuracy = 0.1)))
    output$t_purchase_lasso <- renderText(paste0("Predicted purchase probability (LASSO, 3 variables): ", percent(p_lasso, accuracy = 0.1)))
  }, ignoreNULL = FALSE)

  # ---- Tab 7: Summary ----
  output$summary_table <- renderTable({
    data.frame(
      ` ` = c("Predictors used", "Target", "R2 / Pseudo-R2", "Test ROC-AUC", "Test PR-AUC"),
      `Method 1: MLR` = c("16 (evidence-based)", "Exit rate (continuous)", "0.493", "n/a", "n/a"),
      `Method 2: Logistic` = c("18 (evidence-based)", "Purchase (binary)", "0.336 (McFadden)", "0.895", "0.625"),
      `Method 3: LASSO` = c("3 (auto-selected)", "Purchase (binary)", "n/a", "0.900", "0.641"),
      check.names = FALSE
    )
  })
}

shinyApp(ui, server)
