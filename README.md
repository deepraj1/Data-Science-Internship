# Data-Science-Internship

## Overview: 
* I did this internship in summer 2021 at the Political Consultancy, Janta ka Mood. 

## Objective 
* The main objective for this internship is to predict the election using the data.
## Approach
* To achieve the primary goal, we analyze the area-wise (Mix of 2-3 polling booths) possible core voters, swing voters.
* For swing voters and core voters, we analyze the caste and religion of the voters.
* We use Electoral roll data for voters, and for election results, we use Form-20, affidavit of the candidates.
# challenges
* Mapping the polling booths
* while predicting the voter's caste, it isn't easy because in Rajasthan, especially on the Jhunjhunu and Churu side, many people don't have a surname. They have only a first name.
* many people are using some general surnames like Kumar, Lal, Ram, Singh, Kumari, Devi, etc., so it is challenging to predict the caste.
* Also the spelling mistakes, for example, Agraval, Agrawal

# Solution
* For assigning the surnames, instead of assigning one by one, we first find the family's surname then give that surname to whole members.
* We first assign the major surnames that we are sure about, like "Kanwar," "Agrawal," "Sharma," "Ali," "khan," etc.
* After assigning the surname that we are sure about, we set the surname to be around 42%. 
* We also used some ground knowledge to find the caste. After this, we got 52%.
* After assigning the caste that has different from the general caste, we got around 67%.
* After that, we assign the surname by the address of the sections in a booth. by this point, we got 82%
* Remaining we try to assign the surname by their neighbors by this we got 89%.
* Rest of then, we just assigned their surname.
* So the overall accuracy for the caste analysis is around 90-92%
# Final Work
* Built an interactive Dashboard using R Shiny which shows the caste distribution, past election results, age-wise distribution, and comparison between two area
