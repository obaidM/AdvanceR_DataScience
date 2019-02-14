#################
## Test the tidy where instead of termDocumentMatrix ( of corpus) we use tokens
################

library(tidyr)
library(dplyr)

rm(list=ls(all=TRUE))

text <- c("Review the business scenario outlined for the Town of Cary. Identify the facts in the case that identify directly with the phases of the Analytics lifecycle. Outline no more than one fact per lifecycle phase.
          
          Analytics Lifecycle model:
            Identify Problem ??? identify available data sources (data gathering mechanism) ??? Statistical Analysis ??? Implementation ??? Communicate results  ??? Maintenance 
          Examples of each stage:
            Identify problem: Water wastage is costly and can go undetected for a long time
          Data sources (data gathering): Use AquaWireless water meters that will gather date every hour instead of once a month. This will be 8.760 data points per customer per year instead of just 12
          Statistical Analysis: Use SAS to perform statistical analysis for every customer
          Implementation:  Display results of statistical analysis by data visualization techniques to understand customer trends. This will make spikes in water usage accentuate. 
          Communicate Results: Communicate spikes with customers. Through the online portal, one business in the Town of Cary saw a spike in water consumption on weekends, when employees are away. This seemed odd, and the unusual reading helped the company learn that a commercial dishwasher was malfunctioning, running continuously over week- ends. 
         Without the wireless water-meter data and the customer-accessible portal, this problem could have gone unnoticed, continuing to waste water and money.
          Economic results: The town estimates that by just removing the need for manual readings, the Aquastar system will save more than $10 million above the cost of the project.
         But the analytics component could provide even bigger savings. Already, both the town and individual citizens have saved money by catching water leaks early
          Maintenance: Keep system running perfectly and look for cost cutting opportunities. Like using open source tools to cut license cost. having accurate information on water usage will help it invest in the right amount of infrastructure at the right time. In addition, 
         under- standing water usage will help the town if it experiences something detrimental like a drought."
)  
df <-  data_frame(text = text)

library(tidytext)

### this creates the tidy format. Much better with a chapter number, row number and preferably document name
### see example at tidytextmining.com

df_tidy <- df %>%
  unnest_tokens(word, text)

df_tidy <- df_tidy %>%
  anti_join(stop_words)
