multMin a b c = (max a b) * c
square x = x * x

posOrNeg a =
    if a >= 0
        then "Positive"
        else "Negative"