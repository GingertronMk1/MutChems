# MutChems

This is a program written in Haskell to generate the best option for team chemistries given a Madden Ultimate Team lineup

It takes a lineup, removes all team options with fewer than 5 representatives, and iterates over all possible combinations to give the best option based on:

1. The total amount of chemistry in the top 3 teams
2. The number of chemistries that are an even multiple of 5
3. The average distance each chemistry amount in the top 3 is from being a multiple of 5
