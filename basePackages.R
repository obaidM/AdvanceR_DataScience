###
#
# libraries
#
###
if (! require(tidyverse)  ) 
{ install.packages('tidyverse');  library(tidyverse)  }
if (! require(data.table) )
{ install.packages('data.table'); library(data.table) }
if (! require(broom)      )
{ install.packages('broom');      library(broom)      }
if (! require(RSQLite)    )
{ install.packages('RSQLite');     library(RSQLite)    }