
set.seed(123456L)

# 60 time periods, 30 individuals, and 5 waves of treatment
tmax = 60; imax = 30; nlvls = 5

dat = 
  expand.grid(time = 1:tmax, id = 1:imax) |>
  within({
    
    cohort      = NA
    effect      = NA
    first_treat = NA
    
    for (chrt in 1:imax) {
      cohort = ifelse(id==chrt, sample.int(nlvls, 1), cohort)
    }
    
    for (lvls in 1:nlvls) {
      effect      = ifelse(cohort==lvls, sample(2:10, 1), effect)
      first_treat = ifelse(cohort==lvls, sample(1:(tmax+20), 1), first_treat)
    }
    
    first_treat = ifelse(first_treat>tmax, Inf, first_treat)
    treat       = time>=first_treat
    rel_time    = time - first_treat
    y           = id + time + ifelse(treat, effect*rel_time, 0) + rnorm(imax*tmax)
    
    rm(chrt, lvls, cohort, effect)
  })



# Convert to a tibble
dat <-  tibble::as_tibble(dat)

# Summary statistics 
dat |> 
    summarise( 
        nrtime       = n_distinct( time),
        nindivids  = n_distinct( id),
        treated    = n_distinct( id[ time == max(time) & treat == 1]),
        nottreated = n_distinct( id[ time == max(time) & treat == FALSE])
        )

# Which period id gets treated
dat |> 
    filter( rel_time == 0) |> 
    group_by(time ) |> 
    count()


# Graphical illustration of the data

library(lattice)

# Some (optional!) plot theme settings
trellis.par.set(list(
  axis.line      = list(col = NA),
  reference.line = list(col = "gray85", lty = 3),
  superpose.line = list(col = hcl.colors(imax, "SunsetDark")),
  par.xlab.text  = list(fontfamily = "ArialNarrow"),
  par.ylab.text  = list(fontfamily = "ArialNarrow"),
  axis.text      = list(fontfamily = "ArialNarrow")
  ))

xyplot(
  y ~ time,  
  groups = id,
  type = c("l", "g"),
  ylab = "Y", xlab = "Time variable",
  data = dat
  )
