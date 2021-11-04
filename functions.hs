doubleMe x = x + x

squareMe num = num * num

stm val = squareMe val + doubleMe val 

doubleUs num1 num2 = num1*2 + num2*2


complexDouble val1 val2 = doubleUs val1 val2 + doubleMe val2

doubleSmallNumber num = if num < 10 
then num * 2
else num


