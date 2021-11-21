# Data-Science-Internship

## Overview: 
* I did this internship in summer 2021 at the Political Consultancy, Janta ka Mood. 

## Objective 
* The main objective for this internship is to predict the election using the data.
## Approach
* To achieve the main goal, we analyze the area wise(Mix of 2-3 poolling booths) possible core voters, swing voters.
* For swing voter and core voters we analyze the the caste and religon of the voters.
* We use Electoral roll data for voters and for election results we use Form-20.
# challenges
* Mapping the poolling booths
* while predicting the voters caste it is the diffuclt job because in rajasthan specially in Jhunjhunu churu side many people dont have surname they have only first name.
* there are lots of people using some general surnames like kumar ,lal ,ram ,singh,kumari,devi etc. so it is difficult to pridicts the caste.
* Also the spelling mistakes for example Agrawal,Agraval

# Solution
* For assigning the surnames rather assignning one by one we first find the surname of the family then assign that name to whole memebers.
* We assignn the firt that surnames that we are sure about like "Kanwar", "Agrawal","Sharma","ali", "khan" etc.
* After assigning the surnames that we are sure about we assign the surname total around 42%. 
* We also use some ground knowledge to find the caste after this we got 52% .
* After that we assign the caste we the surnae is different fromt he general surname after this we got 67%
* After that we assign the surname by the address of the sections in a booth. by this point we got 82%
* Remainng we try to assigne the surename by thire neighbors by this we got 89%.
* Rest of then we just assigne their surname.
* SO the overall accuracy for the caste analysis is around 90-92%
# Final Work
* Built an interactive Dashboard using R Shiny which shows the caste distribution, past election results, age-wise distribution, and comparison between two area
