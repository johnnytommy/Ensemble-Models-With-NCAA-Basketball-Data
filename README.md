# Ensemble-Models-With-NCAA-Basketball-Data
An adaptation of the annual Kaggle March Madness, our team took it upon ourselves to see if we could determine which variables impacted the classification of a game being a win the most.

The competition we are working with is Google Cloud & NCAA® ML Competition 2019-Men’s Challenge. For this competition, kagglers forecast the outcomes of the 2019 NCAA Division I Men’s Basketball Championship tournament. Since the 2019 tournament has come and gone, the tournament year we choose to predict does not really matter. To better align ourselves with the course objectives and thereby better utlize the techniques learned in class, we’ve opted to adapt the research question at hand. Instead of prediction, we made this a classification  problem and challenged ourselves to analyze the various variables and see which ones contribute most to a winning game. 

Because we have the results of all the tournament games through 2018 from Kaggle, we will use historical regular season and tournament data to predict the outcomes of the 2017 and 2018 tournaments. For the training set, we will be combining regular season game-level and tournament game-level basektball data together with matchup data. As we will see below, our training dataset contains each matchup twice, where the first half will be what we call the winners dataset and the second half will be the losers dataset. We are duplicating these matchups to eliminate any bias in the data that skews results towards winners or losers. While the matchups will be duplicated, the regular season game-level and tournament game-level features in each obervation will be from the perspective of Team1 which will be the winner in the winners dataset and the loser in the losers dataset. Similarly, Team2 will be the loser in the winners dataset and the winner in the losers dataset.

This analysis was later developed into a presentation for the 'Governmental Applications of Statistical Practices" (GASP) Conference at the Bureau of Labor Statistics (9/23/2019)

John Thomas, Sam Luxenberg, Ning Xie with guidance from Dr. Emre Barut.
Statistical Data Mining Spring 2019. The George Washington University.
