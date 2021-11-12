## Exercise 8 Biocomputing
## November 12, 2021
## Dianna Perez

### Problem 1
#Load scoring data into a dataframe
scoring.data <- read.table("Biocomp-Exercise08/UWvMSU_1-22-13.txt", 
                           sep="\t", header = TRUE)

#Separate the observations into two data sets based on which team scored
UWscore<-scoring.data[which(scoring.data$team=="UW"),]
MSUscore<-scoring.data[which(scoring.data$team=="MSU"),]

#Create a new column which takes on the cumulative score for each scoring event
UWscore$UWCumScore<-cumsum(UWscore$score)
MSUscore$MSUCumScore<-cumsum(MSUscore$score)

#plot this information! x axis will be time, y will be cumulative score, and 
#    each line/curve represents a team (red=UW and green=MSU)
plot(UWscore$time,UWscore$UWCumScore, type='l', col="red", 
        main = "UW vs. MSU Game Scoring", xlab="Time (min)",
        ylab="Cumulative Score") +
        lines(MSUscore$time,MSUscore$MSUCumScore, col = "green") +
        legend(25,20,legend=c("UW","MSU"),col=c("red","green"), lty=c(1,1), ncol=1)


###Problem 2
#Create a function that when called becomes the "Guess My Number Game"
GuessMyNumber<-function(){
  number<-sample(1L:100L,1) #randomly sample an integer [1,100]
  print("I'm thinking of a number 1-100...")
  n1<-readline(prompt="Guess: ") 
  
  i<-1 #keeps track of number of guesses
  
  while (n1 != number && i < 10) {
    if(n1 > number){
      print("Lower")
      i<-i+1
      n1<-readline(prompt="Guess: ")
    }else if(n1 < number){
      print("Higher")
      i<-i+1
      n1<-readline(prompt="Guess: ")  
      }
    } #while loop end bracket
 
  if(n1 == number){
    return("Congratulations, you guessed correctly!")
  }else if(i>=10){
    return("Sorry, you ran out of guesses!")
  } #if statement end bracket
}#function end bracket

GuessMyNumber() 




