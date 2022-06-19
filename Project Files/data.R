columns <- data.frame(
    
    #names of tabs, number of columns/dropdown for the tab. Order is not important
    Category = c(
        rep('Cases', 3),
        rep('Response', 3),
        rep('Population', 5),
        rep('Education', 4),
        rep('Health', 18),
        rep('Economy', 5),
        rep('Happiness', 5)
    ),
    
    ### short name only used in map, order of category must be the same as above
    VariableLabel = c(
        'Confirmed', 'Recovered', 'Deaths',
        'Stringency Index','Containment Health Index','Economic Support Index',
        'Population', 'TestPeople', 'TestSamples', 'TestPerformed', 'TestUnclear',        
        'Tertiary', 'Enrollment', 'Literacy', 'EducationExp',
        'ComDiseases', 'NonComDiseases', 'Injury', 'DeathRate', 'Diabetes', 'ImmDPT', 'ImmHep', 'ImmMeasles', 'LifeExpectancy', 'Anemia', 'Undernourishment', 'Surgical', 'Nurse', 'Physician', 'Bed', 'HealthExp', 'HealthCareIndex', 'HealthCareRank',        
        'Labor', 'NetODA', 'Inflation', 'GDP', 'Unemployment',
        'Happiness', 'Social', 'Freedom', 'Generosity', 'Corruption'
    ),
    
    ### exact names of the variables must be same order as VariableLabel
    VariableName = c(
        'Cumulative confirmed cases',
        'Cumulative recovered cases',
        'Cumulative death cases',
        
        'Stringency Index',
        'Containment Health Index',
        'Economic Support Index',
        
        'Population ages 15-64 (% of total population)',
        'Covid19_test_conducted_people tested',
        'Covid19_test_conducted_samples tested',
        'Covid19_test_conducted_tests performed',
        'Covid19_test_conducted_units unclear',
        
        'School enrollment, tertiary (% gross)',
        'School enrollment, secondary (% net)',
        'Literacy rate, adult total (% of people ages 15 and above)',
        'Government expenditure on education, total (% of GDP)',
        
        'Cause of death, by communicable diseases and maternal, prenatal and nutrition conditions (% of total)',
        'Cause of death, by non-communicable diseases (% of total)',
        'Cause of death, by injury (% of total)',
        'Death rate, crude (per 1,000 people)',
        'Diabetes prevalence (% of population ages 20 to 79)',
        'Immunization, DPT (% of children ages 12-23 months)',
        'Immunization, HepB3 (% of one-year-old children)',
        'Immunization, measles (% of children ages 12-23 months)',
        'Life expectancy at birth, total (years)',
        'Prevalence of anemia among children (% of children under 5)',
        'Prevalence of undernourishment (% of population)',
        'Risk of impoverishing expenditure for surgical care (% of people at risk)',
        'Nurse and Midwives(per1,000People)',
        'Physician(per1,000People)',
        'HOSPITAL_BED_TOTAL(1000HAB)',
        'Health Expenditure (% of GDP)',
        'Health Care Index _2000',
        'HealthCare_Rank_2000',
        
        'Labor force, total',
        'Net ODA Received (2018)',
        'Inflation (2019)',
        'GDP (2019)',
        'Unemployment (2019)',
        
        'Happiness Score (2019)',
        'Social support (2019)',
        'Freedom to make life choices (2019)',
        'Generosity (2019)',
        'Perceptions of corruption (2019)'
    ),
    
    ### Mood for map color scheme, order important
    Mood = c(
        'Neutral', 'Good', 'Bad',
        'Neutral', 'Neutral', 'Neutral',
        'Neutral', 'Neutral', 'Neutral', 'Neutral', 'Neutral',        
        'Good', 'Good', 'Good', 'Good',
        'Bad', 'Bad', 'Bad', 'Bad', 'Bad', 'Good', 'Good', 'Good', 'Good', 'Bad', 'Bad', 'Neutral', 'Neutral', 'Neutral', 'Neutral', 'Good', 'Good', 'Good',
        'Neutral', 'Neutral', 'Neutral', 'Good', 'Bad',
        'Good', 'Good', 'Good', 'Good', 'Bad'
    )
)

#-- Return list of all Category --#
GetCategories <- function() {
    columns %>%
        pull(Category) %>%
        unique() %>%
        sort()
}

#-- Return list of VariableLabels for Category --#
GetColumnVariableLabels <- function(category = 'All') {
    columns %>%
        { if (category == 'All') filter(., TRUE) else filter(., Category == category) } %>%
        pull(VariableLabel)
    }

#-- Return list of VariableNames for Category --#
GetColumnVariableNames <- function(category = 'All') {
    columns %>%
        { if (category == 'All') filter(., TRUE) else filter(., Category == category) } %>%
        pull(VariableName)
}

#-- Return list of VariableNames with VariableLabel as column name, for Category --#
GetColumns <- function(category = 'All') {
    columns <- GetColumnVariableNames(category)
    names(columns) <- GetColumnVariableLabels(category)
    columns
}

#-- Return category of specific VariableLabel --#
GetCategory <- function(label) {
    columns %>%
        filter(VariableLabel == label) %>%
        pull(Category)
}

#-- Return variable name of specific VariableLabel --#
GetVariableName <- function(label) {
    columns %>%
        filter(VariableLabel == label) %>%
        pull(VariableName)
}

#-- Return mood of specific VariableLabel --#
GetMood <- function(label) {
    columns %>%
        filter(VariableLabel == label) %>%
        pull(Mood)
}

ggplot_defaults <- theme(
    plot.title = element_text(face = 'bold', size = rel(1), hjust = 0),
    axis.title = element_text(face = 'bold'),
    plot.background = element_blank(),
    panel.background = element_blank(),
    panel.grid.major.y = element_line(color = 'grey')
)
