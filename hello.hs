repeatString str a =
    if a == 0
    then ""
    else str ++ (repeatString str (a-1))

lista = [1, 5, 10]

y = 10 : lista

yhdistetty = lista ++ y

double nums =
    if null nums
        then []
        else (2 * (head nums)) : (double (tail nums))