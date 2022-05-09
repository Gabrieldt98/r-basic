#sortrev = function(x){
#  rev(sort(x))
#}

vec = c(1,3,5,7,9,2,6,8,4)

#sortrev(vec)

x = rev(sort(vec))
y = sort(rev(vec))

sort(vec, decreasing = TRUE)