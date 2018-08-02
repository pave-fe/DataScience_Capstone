# DataScience_Capstone
Capstone project on word prediction

This is the repository for the Coursera Data Science Capstone course.   This project builds a
prediction model based on 3 data sources (blogs, twitter, and news).  

This initial concept was to take samples of the three sources and create ngram models.  
After exploring the frequency results, I determined that their were too many ( as much as 85%)
of instances with only 1 occurence.   I adjusted my approach to evaluating the entire samples, combining
the frequency tables and trimming the files based on frequency.  It was also beneficial to trim words longer 
than 18 characters to reduce nonsensical words and hashtag mashups.  As an example, the resulting 4-gram frequency table contains
over 700K phrases and is 58Mb in size.   This was an important consideration for improving the accuracy and performance of the
Shiny Application.

Additional performance gains were made  by using data tables vs data frames.   The frequency tables consisted of two columns
and using the indexing feature of data tables helped increase the speed of the prediction model.


Shiny App:  https://pavefe.shinyapps.io/Word_predictor/

Presentation: http://rpubs.com/mlhulin/word_predictor
