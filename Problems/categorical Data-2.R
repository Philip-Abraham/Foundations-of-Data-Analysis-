
#Create Matrix of counts
Count_Matrix<-cbind(c(6,17,15,12),c(28,20,26,6))
colnames(Count_Matrix)  <- c("Women","Men")
rownames(Count_Matrix)  <- c("Action", "Country","Horror","Romance")
Count_Matrix

#Create Probabilies(P(A & B)) of each cell in table - example: P(Action & Women) =0.0462. All the the probabilities
#in each CELL add to 1.00 or 100%
prop.table(Count_Matrix)
barplot(prop.table(Count_Matrix), legend=T, beside=T)

#Create Probabilies(P(A l B))) of each cell in table - example: P(Women l Action) =0.177. All the the probabilities
#in each ROWS add to 1.00 or 100%
prop.table(Count_Matrix,1)
barplot(prop.table(Count_Matrix,1), legend=T, beside=T)

#Create Probabilies(P(B l A))) of each cell in table - example: P(Action l Women) =0.120. All the the probabilities
#in each COLUMNS add to 1.00 or 100%
prop.table(Count_Matrix,2)
barplot(prop.table(Count_Matrix,2), legend=T, beside=T)


