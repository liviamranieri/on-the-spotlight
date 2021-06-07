# on-the-spotlight
The world in 2020 has faced different hurdles and difficult conversations about inclusion. The Oscars have been a topic of these conversations for years and under scrutiny for lack of diversity in multiple fronts.  

The goal of this analysis is to understand if minorities are underrepresented in the Oscars, and if this representation reflects the United States’ population. These minority groups include gender, race, LGBT+ and people with disabilities.  

We chose to analyze diversity within a few of the main Oscars categories: Director, Lead Actor and Actress, Supporting Actor and Actress, Original and Adapted Screenplay and Original Score. We looked at data from 2015 to 2020 and found that the pattern of nominations and winners usually favors heterossexual white males with no disabilities.


Set-up
# Load packages
library(tidyverse)
library(RColorBrewer) #We installed this package in order to use color palettes in the plots. 
library(ggpubr) #We installed this package in order to arrange multiple graphs with different axis in the same plot. 
1 Introduction
The world in 2020 has faced different hurdles and difficult conversations about inclusion. The Oscars have been a topic of these conversations for years and under scrutiny for lack of diversity in multiple fronts.

The goal of this analysis is to understand if minorities are underrepresented in the Oscars, and if this representation reflects the United States’ population. These minority groups include gender, race, LGBT+ and people with disabilities.

We chose to analyze diversity within a few of the main Oscars categories: Director, Lead Actor and Actress, Supporting Actor and Actress, Original and Adapted Screenplay and Original Score. We looked at data from 2015 to 2020 and found that the pattern of nominations and winners usually favors heterossexual white males with no disabilities.

# Load data from the csv file
oscar_diversity = read_csv("oscar_diversity.csv")
2 Data
2.1 Background
Our data includes information about categories delineated above regarding Oscars in the last 6 years (2015, 2016, 2017, 2018, 2019, 2020), starting when the #OscarsSoWhite movement began.

We created our own data set based on different sources:

We used the Academy Awards website to find winners and nominations for each category.

http://awardsdatabase.oscars.org/

We used Statista and NBC to find information about Academy Awards voters demographics.

https://www.statista.com/statistics/321286/voters-academy-awards-gender/

https://www.statista.com/statistics/321291/voters-academy-awards-ethnicity/

https://www.nbcnews.com/pop-culture/awards/who-makes-academy-breakdown-exclusive-oscars-club-n1126866

We used online biographies to collect data about nominees demographics.

3 Analysis
We will split our analysis into four diversity categories: Gender, Race, LGBT+ and People with Disabilities. That way, the plots and information are tailored to each category.

We mainly focused on the nominations, because if these are not representative, minorities don’t have the space to win. Beyond that, the data on wins by minorities is very restrictive.

3.1 Gender
The gender gap in the Oscars is not a recent issue. Men have on average won 86% of the Awards while being nominated on average for 84% of the awards since the Oscars were created (Corona, 2020).

One of the consequences of higher visibility and praise for men are pay gaps. Research has found women in Hollywood earn on average around $1.1m less than male actors with similar experience – a 25% pay gap (Corona, 2020). The World Economic Forum’s Global Gender Gap Report 2020 predicts it will take 99.5 years to reach gender parity (Wood, 2020).

We analyzed the nominations for non-gendered categories in the past 6 years. In some categories, it is possible to have more than one person nominated (e.g. a movie might have multiple screenplay writers). We labeled as “male” the nominations that were received by men and labeled as “female/mixed” the nominations that were received by women or a team that included men and women.

The first graph in the plot below shows that 2018 was the year with the highest percentage of female nominations for categories where men and women competed (25%), while 2015 had no female nominations. There has not been a pattern of improvement across the years.

One of our hypotheses is that the lack of female voters could be one of the root causes for this discrepancy. The second graph in the plot bellow aims to verify the female representation among voters and we can see that has been only a slight improvement in female representation from 2015 to 2020 (25% to 32%).

# Create a data set to plot the percentage of nominations per gender across the years
oscar_diversity_gender = oscar_diversity %>%
  # Filter out gender specific categories to avoid bias
  filter(nomination_type %in% c("Lead Actor", "Lead Actress","Supporting Actor","Supporting Actress") == FALSE) %>%
  # Group the data by the column "year" so that future operations will be performed "by year"
  group_by(year) %>%
  # Create a dummy column for gender
  mutate(gender_n = ifelse(gender == "Male", 1, 0)) %>% 
  # Count the nominations and sum the ones received by men
  summarise(non_gendered_nominations = n(), male_only_nominations = sum(gender_n)) %>%
  # Calculate the percentage of male nominations
  mutate(male = (male_only_nominations/non_gendered_nominations)*100) %>%
  # Calculate the percentage of non-male nominations (female + mixed)
  mutate("female/mixed" = 100-male) %>%
  # Join the columns with nominations percentages under one column 
  pivot_longer(c(`male`, `female/mixed`), names_to = "gender", values_to = "nomination_percentage") 
# Create a plot showing the percentage of nominations per gender across the years
nomination_gender_graph = oscar_diversity_gender %>% 
# Plot the graph
 ggplot(data = ., aes(x = year, y = nomination_percentage, fill = gender)) + 
  geom_col() + # add a bar chart
  ggtitle("Oscar Nominations per Gender") + # add title
  xlab("") + # add x axis label
  ylab("Percentage of nominees") + # add y axis label
  scale_fill_brewer(palette = "Paired") # define the color palette to be used by the fill function in the aesthetics 
# Load data from the csv file about Oscar's voters gender
oscar_voters_gender = read_csv("oscar_voter_gender.csv")

# Create a plot showing the percentage of voters per gender across the years
voter_gender_graph = oscar_voters_gender %>% 
# Plot the graph
 ggplot(data = ., aes(x = year, y = voter_percentage, fill = gender)) + 
  geom_col() + # add a bar chart
  ggtitle("Oscar Voters per Gender") + # add title
  xlab("") + # add x axis label
  ylab("Percentage of voters") +  # add y axis label
  scale_fill_brewer(palette = "Paired") # define the color palette to be used by the fill function in the aesthetics
# Arrange the two previous ggplots in one panel with 1 column and 2 rows
ggarrange(nomination_gender_graph, voter_gender_graph, nrow = 2, align = "v" )


3.2 Race
Hattie McDaniel was the first black person to win an Oscar in 1940. During these times, Jim Crow laws forbid her from attending the premiere of own her movie and sitting with her white fellow cast-members in the Oscars ceremony (Woo, 2017).

After Hattie McDaniel, only two more black people won the award for best actor or actress (Woo, 2017). Far from being an issue of the past, in 2015 and 2016, no people of color were awarded any of the acting nominations (Ugwu, 2020). This initiated the movement defined by #OscarsSoWhite, which questioned racial diversity and inclusion in the ceremony.

In the American society, movements such as #BlackLivesMatter have put a spotlight on conversations about racial inequalities in recent years.

The first graph in the plot below shows the racial diversity in the United States’ population (average between 2015-2019). The second graph shows the racial diversity in the Oscar nominations (average between 2015-2020).

There is considerable discrepancy between the racial diversity in the United States’ population and the Oscar nominations. The most significant being Latinos, which are the second biggest population (18%), but account for only 3.8% of the nominations.

# Load data from the csv file about the US census
census_race = read_csv("census_race.csv")

# Create a data set to plot the race diversity in the US population between 2015 and 2019
census_race = census_race %>%
  # Join the columns with race percentages under one column 
  pivot_longer(c(`white`, `black`, `latino`, `asian`, `indigenous`), names_to = "race", values_to = "census_percentage") %>%
  # Group the data by the column "race" so that future operations will be performed "by race"
  group_by(race) %>%
  # Calculate the average percentage each race represents in the US population
  summarise(average_census_percentage = mean(census_percentage))
# Create a plot showing the racial diversity in the US
census_race_graph = census_race %>% 
# Plot the graph
 ggplot(data = ., aes(x = reorder(race, average_census_percentage),y = average_census_percentage, fill = race)) + # organize x axis in ascendant order
 geom_col(fill = c("#A6CEE3", "#A6CEE3", "#A6CEE3","#A6CEE3", "#1F78B4")) + # add a bar chart and define the colors to be used by the fill function in the aesthetics
 ggtitle("Racial Diversity in US population (2015 - 2019)") + # add title
 xlab("Race") + # add x axis label
 ylab("Average % in the population") +  # add y axis label
 geom_text(aes(label = paste0(average_census_percentage, "%")), # label each column with its value
           vjust = -0.3, # position the label outside of the bars
           color = "black", 
           size = 3) + 
 theme(text = element_text(size = 10), # define text elements size
       legend.position="none") + # remove the plot legend
 ylim (0,90) # define y axis range
# Create a plot showing the racial diversity in the Oscars
oscar_race_graph = oscar_diversity %>%
    count(race) %>%
    mutate(prop = round(n/sum(n), digits=3)*100) %>% # create a column calculating the percentage of nominations per race
# Plot the graph
    ggplot(aes(x = reorder(race, prop), y = prop, fill = race)) + # organize x axis in ascendant order
    geom_col(position = "dodge", fill = c("#A6CEE3", "#A6CEE3", "#A6CEE3","#A6CEE3", "#A6CEE3", "#A6CEE3","#1F78B4")) +  # add a bar chart and define the colors to be used by the fill function in the aesthetics
    ggtitle("Racial Diversity in Oscar Nominations (2015-2020)") + # add title
    xlab("Race") + # add x axis label
    ylab("Average % in the nominations") +  # add y axis label
    geom_text(aes(label = paste0(prop, "%")), # label each column with its value
              vjust = -0.3, # position the label outside of the bars
              color = "black", 
              size = 3) +
    theme(text = element_text(size = 10), # define text elements size
          legend.position="none") + # remove the plot legend
    ylim (0,90) # define y axis range
# Arrange the two previous ggplots in one panel with 1 column and 2 rows
ggarrange(census_race_graph, oscar_race_graph,heights = c(4, 4), nrow = 2, align = "v")


We created the plot below to check if the Oscar nominations were getting more racially diverse over the years. 2015 only had 2 races represented in the nominations; 2016 and 2017 had 3; 2018 had 4; and the last two years had 5. Although we can see an increase in races represented, there is no clear pattern of improvement in the percentage of the minorities being nominated.

# Create a data set to plot the percentage of nominations per race across the years
oscar_diversity_race = oscar_diversity %>%
  # Group the data by the column "year" so that future operations will be performed "by year"
  group_by(year) %>%
  # Create a dummy columns for each gender
  mutate(white_n = ifelse(race == "White", 1, 0)) %>%
  mutate(black_n = ifelse(race == "Black", 1, 0)) %>%
  mutate(latino_n = ifelse(race == "Latino", 1, 0)) %>%
  mutate(asian_n = ifelse(race == "Asian", 1, 0)) %>%
  mutate(indigenous_n = ifelse(race == "Indigenous", 1, 0)) %>%
  mutate(middle_eastern_n = ifelse(race == "Middle Eastern", 1, 0)) %>%
  mutate(mixed_n = ifelse(race == "Mixed", 1, 0)) %>%
  # Count the nominations and sum the ones received by each race
  summarise(total_nominations = n(),total_white_nominations = sum(white_n),total_black_nominations = sum(black_n),total_latino_nominations = sum(latino_n),total_asian_nominations = sum(asian_n),total_indigenous_nominations = sum(indigenous_n),total_middle_eastern = sum(middle_eastern_n),total_mixed_nominations = sum(mixed_n)) %>%
  # Calculate the percentages of nominations received by each race
  mutate(white = (total_white_nominations/total_nominations)*100) %>%
  mutate(black = (total_black_nominations/total_nominations)*100) %>%
  mutate(latino = (total_latino_nominations/total_nominations)*100) %>%
  mutate(asian = (total_asian_nominations/total_nominations)*100) %>%
  mutate(indigenous = (total_indigenous_nominations/total_nominations)*100) %>%
  mutate(middle_eastern = (total_middle_eastern/total_nominations)*100) %>%
  mutate(mixed = (total_mixed_nominations/total_nominations)*100) %>%
  # Join the columns with nominations percentages under one column 
  pivot_longer(c(`white`, `black`, `latino`, `asian`, `indigenous`, `middle_eastern`, `mixed`), names_to = "race", values_to = "nomination_percentage")
# Create a plot showing the racial diversity in the Oscars over the years
oscar_diversity_race %>% 
# Plot the graph
 ggplot(data = ., aes(x = reorder(race, nomination_percentage),y = nomination_percentage, fill = race)) + # organize x axis in ascendant order
 facet_wrap(~year) + # create a multi-panel plot divided by year
 geom_col() + # add a bar chart
 ggtitle("Oscar Nominations per Race") + # add title
 xlab("Race") + # add x axis label
 ylab("% of Nominations") + # add y axis label
 geom_text(aes(label = ifelse(nomination_percentage == 0,"",paste0(nomination_percentage, "%"))), # label each column with its value (unless the value is 0)
           vjust = -0.3,  # position the label outside of the bars
           color = "black", 
           size = 2.1) +
 theme(axis.text.x=element_blank(), # remove labels for each column in the x axis
       axis.ticks.x=element_blank(), # remove x axis tick mark labels
       legend.title = element_blank()) + # remove legend title
  scale_fill_brewer(palette = "YlGnBu", # define the color palette to be used by the fill function in the aesthetics
                    breaks = levels(with(oscar_diversity_race,
                    reorder(race, nomination_percentage)))) + # organize legend in ascendant order
   ylim (0,100) # define y axis range


After analyzing the data, we realized that, among racial minorities, black people are more often cast in movies that address racial issues and given stereotypical roles (i.e. black women as slaves, maids or sex symbols). The plot below shows the total amount of black actors and actresses nominated between 2015 and 2020. Out of 13 nominations, 10 were given to people in racially themed movies and 8 played stereotypical characters.

# Create a data set to plot black actors and actresses nominations
black_actors_actresses = oscar_diversity %>%
  # Choose the columns to be shown in the data set
  select(year, movie, nomination_type, person_name, winner, race, racial_theme,racial_stereotypes) %>%
  # Filter out non black races and non acting awards
  filter(race == "Black", nomination_type %in% c("Lead Actor", "Lead Actress","Supporting Actor","Supporting Actress")) %>%
  # Count the nominations for black actors and actresses, sum the nominations received by actors and actresses in racial themed movies and the ones received by people playing stereotypical "black" characters
  summarise("Black Actors Actresses" = n(), "Racial Themed Movies" = sum(racial_theme), "Stereotypical Roles" = sum(racial_stereotypes))%>%
  # Join the columns above under one column 
  pivot_longer(c(`Black Actors Actresses`, `Racial Themed Movies`, `Stereotypical Roles`), names_to = "black_people_participation", values_to = "number_of_nominations")
# Create a plot showing black actors and actresses nominations
black_actors_actresses %>%
# Plot the graph
  ggplot(data = .,aes(x = black_people_participation, y = number_of_nominations))+
  geom_col(fill = c("#A6CEE3", "#A6CEE3", "#A6CEE3")) + # add a bar chart and define the colors to be used by the fill function in the aesthetics
  ggtitle("Black Actors and Actresses Participation (2015-2020)") + # add title
  xlab("") + # add a blank x axis label
  ylab("Number of Nominations") + # add y axis label
  geom_text(aes(label = ifelse(number_of_nominations == 0,"",paste0(number_of_nominations))), # label each column with its value (unless the value is 0)
            vjust = -0.3, # position the label outside of the bars               
            color = "black", 
            size = 4)+
  theme(axis.ticks.x=element_blank(), # remove x axis tick mark labels
        legend.position="none") + # remove color legend
  ylim (0,15) # define y axis range


3.3 LGBT+
Similarly to race, the LGBT+ community englobes different groups of people. These groups can be related to sexual or gender orientation.

Within the LGBT+ community, one of the most marginalized groups are trans people. To date, only two trans people have been nominated for Oscars – composer Angela Morley, nominated for “The Little Prince” (1974) and “The Slipper and the Rose” (1976) (Davidson, 2017). No nominations happened in the 2000s, much less in the 6 year window we are analyzing here.

The LGBT+ representation in movies is not as easily spotted as race or gender, which makes it even harder to quantify and analyze.

The plot below shows the number of nominations given to a person or a team that includes at least one person that identifies as LGBT+. The categories we analyzed include 40 nominations per year, but in the past 6 years between 0 and 4 nominations were given to LGBT+ people and only one resulted in a win.

# Create a data set to plot the number of LGBT+ nominations per year
lgbt_people = oscar_diversity %>%
   # Group the data by the column "year" so that future operations will be performed "by year"
  group_by(year) %>%
  # Sum LGBT+ nominations per year
  summarise(lgbt_nominations = sum(lgbt_person)) 
# Create a plot showing the number of LGBT+ nominations per year
lgbt_people %>%
# Plot the graph
 ggplot(data = ., aes(x = year, y = lgbt_nominations)) +
 geom_col(fill = c("#A6CEE3", "#A6CEE3", "#A6CEE3", "#A6CEE3", "#A6CEE3", "#A6CEE3")) + # add a bar chart and define the colors to be used by the fill function in the aesthetics
 ggtitle("LGBT+ Nominees") + # add title
  xlab("") + # add a blank x axis label
  ylab("Number of Nominations") + # add y axis label
  geom_text(aes(label = paste0(lgbt_nominations)), # label each column with its value
            vjust = -0.3,  # position the label outside of the bars  
            color = "black", 
            size = 4) 


Even among movies with LGBT+ narratives, LGBT+ people are not prioritized to tell these stories.

The history of non-LGBT+ actors portraying LGBT+ characters is also extensive. In 2015, Eddie Redmayne won an Oscar portraying a trans woman in “The Danish Girl”. In 2020, out of the 20 nominees for acting awards, none identify as LGBT+, although two (Margot Robbie, Best Supporting Actress for “Bombshell,” and Antonio Banderas, Best Actor for “Pain and Glory”) portrayed LGBT+ characters.

The plot below shows that from 9 nominations given to movies that had LGBT+ narratives in the last 6 years, 2 were given to actors who played LGBT+ characters and 2 were given to LGBT+ people participating in the production of these movies.

# Create a data set to plot LGBT+ nominations in LGBT+ themed movies
lgbt_theme_movies = oscar_diversity %>%
  # Choose the columns to be shown in the data set
  select(year, movie, nomination_type, person_name, winner, race, lgbt_theme, lgbt_person, lgbt_role) %>%
  # Filter out movies that are not LGBT+ themed
  filter(lgbt_theme == "1") %>%
  # Sum LGBT+ themed movies, actors/actresses who were nominated for playing LGBT+ characters in those movies, LGBT+ people nominated in those movies 
  summarise("LGBT+ Themed Movies" = sum(lgbt_theme), "LGBT+ Character" = sum(lgbt_role), "LGBT+ Nominees" = sum(lgbt_person)) %>%
   # Join the columns above under one column
  pivot_longer(c(`LGBT+ Themed Movies`, `LGBT+ Character`, `LGBT+ Nominees`), names_to = "lgbt_representation", values_to = "number_of_nominations")
# Create a plot showing LGBT+ nominations in LGBT+ themed movies
lgbt_theme_movies %>%
# Plot the graph
  ggplot(data = ., aes(x = reorder(lgbt_representation, - number_of_nominations), y = number_of_nominations)) + 
    geom_col(fill = c("#A6CEE3", "#A6CEE3", "#A6CEE3")) + # add a bar chart and define the colors to be used by the fill function in the aesthetics
    ggtitle("LGBT+ Representation in LGBT+ Themed Movies") + # add title
    xlab("") + # add a blank x axis label
    ylab("Number of Nominations") +  # add y axis label
    geom_text(aes(label = ifelse(number_of_nominations == 0,"",paste0(number_of_nominations))), # label each column with its value (unless the value is 0)
              vjust = -0.3, # position the label outside of the bars                
              color = "black",
              size = 4) +
    ylim (0,10) # define y axis range


3.4 People with Disabilities
1 in each 5 Americans have a known disability (CDC, 2015). However, they still remain one of the most underrepresented groups in the movie industry.

In the history of the Oscars, there have been only two winning actors with known disabilities: Harold Russell in 1946 and Merlee Matlin in 1986. However, since the late 1980s, almost one-third of winners for the lead actor award were playing a character with a disability (Gray, 2019). These include Dustin Hoffman in “Rain Man” (1989) and Eddie Redmayne in “The Theory of Everything” (2015), for example.

Much like the LGBT+ people, people with disabilities are not even chosen to portray characters, or be included in the production of movies with disability narratives.

The plot below shows that from 8 nominations given to disability themed movies in the last 6 years, 3 were given to actors who played characters with disabilities and none of those actors had a known disability themselves.

# Create a data set to plot nominations for people with disabilities in disability themed movies
disability_theme_movies = oscar_diversity %>%
  # Choose the columns to be shown in the data set
  select(year, movie, nomination_type, person_name, winner, race, disability_theme, person_with_disability, disability_role) %>%
  # Filter out movies that are not disability themed
  filter(disability_theme == "1") %>%
  # Sum disability themed movies, actors/actresses who were nominated for playing characters with disabilities in those movies, people with known disabilities nominated in those movies 
  summarise("Disability Themed Movies" = sum(disability_theme), "Character with Disability" = sum(disability_role), "Nominees with Disabilities" = sum(person_with_disability)) %>%
  # Join the columns above under one column
  pivot_longer(c(`Disability Themed Movies`, `Character with Disability`, `Nominees with Disabilities`), names_to = "disability_representation", values_to = "number_of_nominations")
# Create a plot showing nominations for people with disabilities in disability themed movies
disability_theme_movies %>%
# Plot the graph
  ggplot(data = ., aes(x = reorder(disability_representation, - number_of_nominations), y = number_of_nominations)) + # organize x axis in ascendant order
    geom_col(fill = c("#A6CEE3", "#A6CEE3", "#A6CEE3")) + # add a bar chart and define the colors to be used by the fill function in the aesthetics
    ggtitle("Representation of People with Disabilities in Disability Themed Movies") + # add title
    xlab("") + # add a blank x axis label
    ylab("Number of Nominations") +  # add y axis label
    geom_text(aes(label = paste0(number_of_nominations)), # label each column with its value 
            vjust = -0.3, # position the label outside of the bars 
            color = "black", 
            size = 4) +
  ylim (0,9) # define y axis range


4 Conclusion
We wanted to map the lack of diversity and inclusion in a prestigious ceremony such as the Oscars. For a number of years, the Academy has struggled to nominate films that are diverse in their cast, directors and crew. From 2015 to 2020, we noticed some small signs of improvement, but there is no clear growth pattern in terms of diversity and inclusion.

5 Ideas for Future Research
The Academy, after the lack of diversity in their 2020 ceremony, has pledged to have additional diversity requirements for future nominations (Davis, 2020). This is an effort to further include ethnic minorities, the LGBT+ community, people with disabilities and women. In future research, we’d like to go further in the past with our analysis (for instance, the 2000s), and evaluate the success of the implementation of these new “requirements” in the years to come. We would also include the remaining Oscar categories.

Finally, we attempted to build regressions to model the likelihood of being a winner, as a function of gender and race. However, we believe that we would need to include more years in our analysis to have enough evidence to create an accurate model.

Here are the findings with the data we currently have.

5.1 Predictive Model for Winning in Terms of Gender
Null Hypothesis: The chances or being a winner do not increase or decrease depending on the gender.

In order to test this, we tried predicting the chances of winning an Oscar based on gender:

oscar_diversity %>%
  # Filter out gender specific categories 
  filter(nomination_type %in% c("Lead Actor", "Lead Actress","Supporting Actor","Supporting Actress") == FALSE) %>%
  # Run a regression with winner as a function of gender
  lm(winner ~ gender_reg, data = .) %>%
  # Show results of the model
  summary()

Call:
lm(formula = winner ~ gender_reg, data = .)

Residuals:
    Min      1Q  Median      3Q     Max 
-0.2019 -0.2019 -0.2019 -0.0625  0.9375 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)
(Intercept)  0.06250    0.09682   0.646    0.520
gender_reg   0.13942    0.10400   1.341    0.183

Residual standard error: 0.3873 on 118 degrees of freedom
Multiple R-squared:  0.015, Adjusted R-squared:  0.006655 
F-statistic: 1.797 on 1 and 118 DF,  p-value: 0.1826
5.1.1 Interpretation:
There is no significant effect of gender. Each gender equally likely to win an award that all genders compete for. This is conflicting with our analysis, because in the past 5 years, there has been significantly more wins from men than women.

There could be other factors influencing these wins.

5.2 Predictive Model for Winning in Terms of Race
Null Hypothesis: Race has no effect on chances of winning an Oscar.

In order to test this, we created two different variables based on the winners (White and Non-White) and ran the regression:

oscar_diversity %>%
  # Run a regression with winner as a function of race
  lm(winner ~ race_reg, data = .) %>%
  # Show results of the model
  summary()

Call:
lm(formula = winner ~ race_reg, data = .)

Residuals:
    Min      1Q  Median      3Q     Max 
-0.3846 -0.1492 -0.1492 -0.1492  0.8508 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  0.38462    0.06119   6.286 1.54e-09 ***
race_reg    -0.23536    0.06686  -3.520 0.000517 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.3821 on 238 degrees of freedom
Multiple R-squared:  0.04949,   Adjusted R-squared:  0.04549 
F-statistic: 12.39 on 1 and 238 DF,  p-value: 0.0005168
5.3 Interpretation:
Turns out that race has a determining effect in winning chances. We have enough evidence to reject our null hypothesis, given that non-white people significantly less likely to win an award.

6 References
Davis, C. (2020). Oscars Announce New Inclusion Requirements for Best Picture Eligibility. Retrieved online from: https://variety.com/2020/film/news/oscars-inclusion-standards-best-picture-diversity-1234762727/

Wood, J. (2020). Only 5 women have ever been nominated for an Oscar for Best Director. Retrieved online from: https:weforum.org/agenda/2020/02/oscars-academy-awards-gender-women-director-film-equality

Corona, A. (2020) Why Oscar is a Male. A Data-Driven Analysis of Gender Representation in 87 Years of Academy Awards. Retrieved online from: https://medium.com/silk-stories/why-oscar-is-a-male-a-data-driven-analysis-of-gender-representation-in-87-years-of-academy-awards-837ddd812d27

Ugwu, Reggie. (2020).The Hashtag That Changed the Oscars: An Oral History. Retrieved online from: https://www.nytimes.com/2020/02/06/movies/oscarssowhite-history.html

Woo, Eugene. (2017). 6 Facts That Prove That The Oscars is More Racist Than You Think [Infographic]. Retrieved online from: https://venngage.com/blog/oscar-racism-interactive-infographic/

Davidson, A. (2017). The strange history of LGBT films at the Oscars. Retrieved online from: https://www.washingtonblade.com/2020/01/15/oscars-2020-and-the-loser-is-diversity/

King, J. (2020). Oscars 2020: And the loser is … diversity. Retrieved online from: https://www2.bfi.org.uk/news-opinion/news-bfi/features/history-lgbt-gay-lesbian-trans-films-oscars

Levy, S. (2015). Oscar-Winners Often Play Disabled Characters. So Why Don’t We See Disabled Actors? Retrieved online from: https://variety.com/2019/film/news/oscar-and-hollywoods-nearly-invisible-people-with-disabilities-1203422966/ Gray, T. (2019). Oscar and Hollywood’s Nearly ‘Invisible’ People With Disabilities. Retrieved online from: https://groundswell.org/oscar-winners-often-play-disabled-characters-so-why-dont-we-see-disabled-actors/

