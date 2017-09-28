# The ABC credit card company charges an Annual Percentage Rate (APR) of 21.99%, compounded
# monthly. If you have a balance of $2000 on the card, and you pay $200 a month to the card, how many months 
# would it take you to pay it off? How much P&I will you pay


remainder<-1800
extra<-0
t<-1

while(remainder >=-15) {
        
        balance<-(remainder)*(1+(.2199/12))
        
        print(t)
        print(remainder)
        print(balance)
        
        difference<-balance-remainder
        extra<-extra+difference
        
         t<-t+1
         remainder <- balance-200
         
}
print(t-1)
Total_Prin_Interest<-2000+extra
Total_Prin_Interest
