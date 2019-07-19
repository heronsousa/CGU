#Analysing NBA data

#Salary
myplot(Salary)
myplot(Salary/Games)
myplot(Salary/FieldGoalAttempts)

#In-Game Metrics
myplot(MinutesPlayed)
myplot(Points)

#In-Game Metrics Noemalized     --    Due to injury
myplot(FieldGoals/Games)
myplot(FieldGoals/FieldGoalAttempts)
myplot(FieldGoalAttempts/Games)
myplot(Points/Games)

#Interesting Observation
myplot(MinutesPlayed/Games)
myplot(Games)

#Time is valuable
myplot(FieldGoals/MinutesPlayed)

#Player Style
myplot(Points/FieldGoals)
