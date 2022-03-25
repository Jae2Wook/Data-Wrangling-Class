library(tidyverse)

# getting data from the web: tq_get()
# manipulating financial data: tq_transmute() and tq_mutate()
# performance anlysis and portfolio analysis: tq_performance()

library(tidyquant)

# dygraphs
library(dygraphs)

lungDeath <- cbind(mdeaths, fdeaths)
dygraph(lungDeath)

dygraph(lungDeath) %>% dyRangeSelector()

dygraph(lungDeath) %>% 
  dySeries("mdeaths", label = "Male") %>% 
  dySeries("fdeaths", label = "Female") %>% 
  dyRangeSelector(height = 20) %>% 
  dyOptions(stackedGraph = TRUE)

hw <- HoltWinters(ldeaths)
predicted <- predict(hw, n.ahead = 72, prediction.interval = TRUE)

dygraph(predicted, main = "Predicted Lung Deaths (UK)") %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dySeries(c("lwr", "fit", "upr"), label = "Deaths") %>%
  dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1"))


# DT: An R interface to the Data Tables library

library(DT)
xfun::session_info("DT")

# datatable(): creates an HTML widget to display R data objects with Data Tables

datatable(iris)

datatable(head(iris), class = 'cell-orber stripe')

DT:::DT2BSClass('display')
DT:::DT2BSClass(c('compact', 'cell-border'))

DT::datatable(head(iris), editable = "cell")

DT::datatable(head(iris), editable = list(target = 'row', disable = list(columns = c(1, 3, 4))))

# display row names
datatable(head(mtcars))

datatable(head(mtcars), rownames = FALSE)

datatable(head(mtcars), rownames = head(LETTERS))

datatable(head(iris), colnames = c("here", "are", "some", "new", "names"))

datatable(head(iris), colnames = c("a better" = "Sepal.Width"))

datatable(head(iris), colnames = c("another" =  2, "yet another" = 4))

datatable(head(iris), colnames = c("ID" = 1))

# custom table container
sketch = htmltools::withTags(table(
  class = "display",
  thead(
    tr(
      th(rowspan = 2, "Species"),
      th(colspan = 2, "Sepal"),
      th(colspan = 2, "Petal")
    ),
    tr(lapply(rep(c("Length", "Width"), 2), th))
  )
))

print(sketch)

datatable(iris[1:20, c(5, 1:4)], container = sketch, rownames = FALSE)

# custom table with both header and footer
sketch = htmltools::withTags(table(
  tableHeader(iris),
  tableFooter(iris)
))
print(sketch)

datatable(head(iris, 10),
          container = sketch, options = list(pageLength = 5, dom = 'tip'), rownames = FALSE)

# table caption
datatable(head(iris),
          caption = 'Table 1: this is a simple caption for the table')

datatable(head(iris),
          caption = htmltools::tags$caption(
            style = "caption-side: bottom; text-align: center;",
            "Table 2", htmltools::em("This is a simple caption for the table.")
          ))

# column filters
iris2 <- iris[c(1:10, 51:60, 101:110),]
datatable(iris2, filter = "top", options = list(pageLength = 5, autoWidth = TRUE))

d <-  data.frame(
  names = rownames(mtcars),
  date = as.Date('2015-03-23') + 1:32,
  time = as.POSIXct('2015-03-23 12:00:00', tz = 'UTC') + (1:32) * 5000,
  stringsAsFactors = FALSE
)
str(d)
datatable(d, filter = 'bottom', options = list(pageLength = 5))

datatable(head(iris, 30), callback = JS('table.page("next").draw(false);'))

# callback function
function(table){
  table.page("next").draw(false);
}    

# escaping table content
m <-  matrix(c(
  '<b>Bold</b>', '<em>Emphasize</em>', '<a href="http://rstudio.com">RStudio</a>',
  '<a href="#" onclick="alert(\'Hello World\');">Hello</a>'
), 2)

colnames(m) <- c('<span style="color:red">Column 1</span>', '<em>Column 2</em>')

datatable(m)

datatable(m, escape = FALSE)

datatable(m, escape = 1) # escape the first column
datatable(m, escape = 2)
datatable(m, escape = c(TRUE, FALSE))
colnames(m) <- c("V1", "V2")
datatable(m, escape = "V1")

rownames(m) <- seq_len(nrow(m))
datatable(m, escape = 1 + 1)



# Task

# library(quantmod)

KR <- tq_get("KR")

KR %>% class()

KR <- KR %>% filter(date >= today() - years(5)) %>% filter(date < today()) %>% select(-symbol,)

KR_xts <- timetk::tk_xts(data = KR, select = c(open, high, low, close, adjusted),  date_var = date)

dygraph(KR_xts) %>% 
  dyRebase(value = 10000) %>% 
  dyRangeSelector(dateWindow = c(today() - years(2), today())) %>% 
  dyAnnotation("2020-03-18", text = "A", tooltip = "Kroger Board of Directors Declares Quarterly Dividend") %>% 
  dyAnnotation("2021-01-27", text = "B", tooltip = "Kevin Brown and Amanda Sourry Elected to Kroger Board of Directors")
  


