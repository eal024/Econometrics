



qnorm(p = 0.975)
t.value = (mean(data) - 10) / (sd(data) / sqrt(length(data))) 
p.value = 2*pt(-abs(t.value), df=length(data)-1)
pt(1.96, 1000000, lower.tail = F) * 2
norm()
