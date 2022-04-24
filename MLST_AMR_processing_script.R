#Load packages
library(readxl)
library(ggplot2)
library(ggpubr)

#Read file and format as data frame
d <- read_excel('Pasteurella multocida (38).xlsx')
d <- as.data.frame(d)

#Make functions
createplot <- function(dataframe, antibiotic, name) {
  
  #obtain AMR ratio
  dataframe <- as.data.frame(dataframe)
  
  aggregated_by_year <- aggregate(antibiotic, by=list(dataframe$year), FUN=sum)
  occurences_of_each_year <- table(dataframe$year)
  occurences_of_each_year
  
  ratio <- as.data.frame(100*(aggregated_by_year$x/occurences_of_each_year))
  
  #Create plot
  #png(paste(name, 'plot.png'))
  plot <- ggplot(ratio, aes(x = Var1, y=Freq, group=1)) +
    geom_smooth(formula = y ~ x, method = 'lm') +
    geom_point() +
    theme(plot.title=element_text(hjust=0.5, vjust=0.5)) +
    ylim(0,100)+
    theme(axis.text.x = element_text(angle = 60, hjust =1)) +
    labs(x = 'Year', y = '% of resistant isolates',
         title = paste('Resistance per year to', name))
  print(plot)
  #dev.off()
}

### Use function to get plots of AMR for each antibiotic class
#for Aminoglycosides
a <- createplot(dataframe = d, antibiotic = d$amino, name = 'aminoglycosides') 

#for Betalactams
b <- createplot(dataframe = d, antibiotic = d$betalactamics, name = 'betalactams')

#For Colistins
c <- createplot(dataframe = d, antibiotic = d$colistins, name = 'colistins')

#for Disinfectants
dis <- createplot(dataframe = d, antibiotic = d$desinfectant, name = 'disinfectants')

#For Fosfomycins
fos <- createplot(dataframe = d, antibiotic = d$fosfomycin, name = 'fosfomycins')

#For Fusidic acids
fus <- createplot(dataframe = d, antibiotic = d$fusidic_acid, name = 'fusidic acids')

#For Glycopeptides
g <- createplot(dataframe = d, antibiotic = d$glycopeptide, name = 'glycopeptides')

#For Macrolides
m <- createplot(dataframe = d, antibiotic = d$macrolide, name = 'macrolides')

#For Nitroimidazoles
n <- createplot(dataframe = d, antibiotic = d$nitroimidazole, name = 'nitroimidazoles')

#For Oxazolidinones
o <- createplot(dataframe = d, antibiotic = d$oxazolidinone, name = 'oxazolidinones')

#For Phenicols
phe <- createplot(dataframe = d, antibiotic = d$phenicol, name = 'phenicols')

#For Pseudomonic acids
pse <- createplot(dataframe = d, antibiotic = d$pseudomonic_acid, name = 'pseudomonic acids')

#For Quinolones
q <- createplot(dataframe = d, antibiotic = d$quinolone, name = 'quinolones')

#For Rifampicins
r <- createplot(dataframe = d, antibiotic = d$rifampicin, name = 'rifampicins')

#For Sulfonamides
s <- createplot(dataframe = d, antibiotic = d$sulfonamide, name = 'sulfonamides')

#For Tetracyclines
tet <- createplot(dataframe = d, antibiotic = d$tetracycline, name = 'tetracyclines')

#For Trimethoprims
tri <- createplot(dataframe = d, antibiotic = d$trimethoprim, name = 'trimethoprims')

#Combine plots
png('combined_plot_Pasteurella.png')
combined_plot <- ggarrange(a, b, c, dis, fos, fus, g, m, n, o, phe, pse, q, r, s, tet, tri)
annotate_figure(combined_plot, 
                top=text_grob("Evolution of AMR genes families across time in Pasteurella multicoda"))
dev.off()

