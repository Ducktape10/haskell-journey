add (a, b) (c, d) = (a + c, b + d)

sub (a, b) (c, d) = (a - c, b - d)

mul c (a, b) = (a*c, b*c)

scalar (a, b) (c, d) =  (a*c+b*d)

vecLength (a, b) = sqrt(a^2 + b^2)

pairToList (a, b) = a : b : []