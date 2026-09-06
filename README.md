# What Makes an Online Shopper Buy? — Interactive Companion App

This is the R Shiny companion to the written report. It rebuilds all three
models (MLR, Logistic Regression, LASSO) live from `online_shoppers_intention.csv`,
using the exact same formulas, train/test split, and random seed as the report,
so every number you see here matches the write-up.

## How to run it

1. Install R (4.0 or later) and RStudio if you don't already have them.
2. Open `app.R` in RStudio. Keep `online_shoppers_intention.csv` in the
   same folder as `app.R` (it already is, in this folder).
3. Install the required packages, one time, by running this in the R console:

   ```r
   install.packages(c("shiny", "shinythemes", "ggplot2", "dplyr",
                       "glmnet", "DT", "plotly", "scales", "reshape2"))
   ```

4. Click the "Run App" button in RStudio, or run:

   ```r
   shiny::runApp("app.R")
   ```

The app opens in a browser window or the RStudio Viewer pane.

## What's inside

- **Welcome** — dataset overview, the three business questions, and the team.
- **Explore the Data** — pick any category variable and see its purchase-rate
  chart and one-way ANOVA test update live; correlation heatmap; descriptive stats.
- **Method 1: MLR** — sliders to simulate a browsing session and see the
  predicted exit rate; coefficient plot; AIC/BIC check.
- **Method 2: Logistic Regression** — a threshold slider that redraws the
  confusion matrix, accuracy, precision, recall and F1 live, plus ROC and
  PR curves with the current threshold marked.
- **Method 3: LASSO** — a penalty-strength slider that shows predictors
  shrinking to zero one by one, landing on the report's 3-predictor model.
- **Try It Yourself** — describe a hypothetical session and get a live
  predicted exit rate (Method 1), fed into a live predicted purchase
  probability (Method 2), compared against Method 3's lean 3-variable model.
  This tab is the clearest demonstration of how the three methods connect.
- **Summary** — the final side-by-side comparison table from the report.

## Deploying it so others can open it without RStudio

The easiest option is [shinyapps.io](https://www.shinyapps.io) (free tier
available):

```r
install.packages("rsconnect")
rsconnect::setAccountInfo(name = "<your account>", token = "<token>", secret = "<secret>")
rsconnect::deployApp("path/to/shopper_shiny_app")
```

This gives you a public URL you can share with your professor or open in
front of the class, no local R installation needed on their end.

## Notes

- Model fitting happens once when the app starts (not on every click), so
  the sliders and dropdowns feel instant.
- The `glm.fit: fitted probabilities numerically 0 or 1 occurred` message
  you may see in the R console on startup is expected. It comes from
  PageValues and ExitRates being very strong predictors and is already
  noted in the written report; it does not affect the app's behaviour.
