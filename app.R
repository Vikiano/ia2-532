library(tidyverse)
library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashBootstrapComponents)
library(ggplot2)
library(plotly)

app <- Dash$new(meta_tags=list(list("name"= "viewport", "content"= "width=device-width")))


crime <- readr::read_csv("data/processed/crime_clean.csv")

crime <- crime %>%
    na.omit()

week_l <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday" )

neighbourhood_l <- unique(crime[c("NEIGHBOURHOOD")])$NEIGHBOURHOOD
neighbourhood_l

neigbourhood_lab_val <- c()
for (val in neighbourhood_l)
{
    a <- list(list(label = val, value = val))
    neigbourhood_lab_val <- append(neigbourhood_lab_val, a)
}

crime_c <- crime %>%
    add_count(crime_category)




app$layout(htmlDiv(list(
    dccGraph(id='plot-area'),
    # htmlIframe(id="hist",
    #            style=list("border-width"="0", "width"= "400px", "height"= "475px")
    #            ),
    dccDropdown(id="weekday_dropdown",
                value="All",
                options=list(list(label="Sunday", value="Sunday"),
                             list(label="Monday", value="Monday"),
                             list(label="Tuesday", value="Tuesday"),
                             list(label="Wednesday", value="Wednesday"),
                             list(label="Thursday", value="Thursday"),
                             list(label="Friday", value="Friday"),
                             list(label="Saturday", value="Saturday")
                             )
                ),
    dccDropdown(id="neighbourhood_dropdown",
                value="West End",
                options = neigbourhood_lab_val
                )
    ), className="row flex-display"
    )
    )

app$callback(
    output('plot-area', 'figure'),
    list(input('weekday_dropdown', 'value'),
         input('neighbourhood_dropdown', 'value')),
    function(weekday, neighbourhood) {
        if (weekday=="All") {
            crime_c_new = crime_c %>%
                filter(NEIGHBOURHOOD==neighbourhood)
            p <- ggplot(crime_c_new) +
                aes(x = reorder(crime_category, n)) +
                geom_bar(stat="count", fill = "skyblue3") +
                labs(title= paste("Total Reported Crimes in", neighbourhood), x="Crime Category", y="Number of crime cases") +
                theme(
                    plot.title = element_text(size = 9, face = "bold", color="#FFFFFF"),
                    axis.text = element_text(size = 12, color="#FFFFFF"),
                    axis.title = element_text(size = 12, color="#FFFFFF"),
                    axis.text.x = element_text(angle = 45),
                    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_rect(fill = "#010915", colour = "#010915"),
                    plot.background = element_rect(fill = "#010915")
                )
            
        } else {
            crime_c_new = crime_c %>%
                filter(day_of_week==weekday & NEIGHBOURHOOD==neighbourhood)
            p <- ggplot(crime_c_new) +
                aes(x = reorder(crime_category, n)) +
                geom_bar(stat="count", fill = "skyblue3") +
                labs(title= paste("Total Reported Crimes in", neighbourhood, "on", weekday), x="Crime Category", y="Number of crime cases") +
                theme(
                    plot.title = element_text(size = 9, face = "bold", color="#FFFFFF"),
                    axis.text = element_text(size = 12, color="#FFFFFF"),
                    axis.title = element_text(size = 12, color="#FFFFFF"),
                    axis.text.x = element_text(angle = 45),
                    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_rect(fill = "#010915", colour = "#010915"),
                    plot.background = element_rect(fill = "#010915")
                )
            
        }
        
        ggplotly(p, height = 625, width=475) %>% layout(dragmode = 'select')
        
    }
    
)



app$run_server(host = '0.0.0.0')
