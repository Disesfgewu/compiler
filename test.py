def make( n ):
    if n == 0 :
        return "l"
    else:
        return "a" + make( n-1 )
    
print( make(2))