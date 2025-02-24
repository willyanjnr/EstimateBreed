pkgname <- "EstimateBreed"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('EstimateBreed')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("atsum")
### * atsum

flush(stderr()); flush(stdout())

### Name: atsum
### Title: Accumulated Thermal Sum
### Aliases: atsum

### ** Examples




cleanEx()
nameEx("deltat")
### * deltat

flush(stderr()); flush(stdout())

### Name: deltat
### Title: Optimum conditions for pesticide application
### Aliases: deltat

### ** Examples




cleanEx()
nameEx("desv_clim")
### * desv_clim

flush(stderr()); flush(stdout())

### Name: desv_clim
### Title: Auxiliary function for calculating ISGR
### Aliases: desv_clim

### ** Examples




cleanEx()
nameEx("estresse")
### * estresse

flush(stderr()); flush(stdout())

### Name: estresse
### Title: Stress indices for genotype selection
### Aliases: estresse

### ** Examples




cleanEx()
nameEx("gvri")
### * gvri

flush(stderr()); flush(stdout())

### Name: gvri
### Title: Selection for Grain Volume
### Aliases: gvri

### ** Examples




cleanEx()
nameEx("hello")
### * hello

flush(stderr()); flush(stdout())

### Name: hello
### Title: Hello, World!
### Aliases: hello

### ** Examples

hello()



cleanEx()
nameEx("heterose")
### * heterose

flush(stderr()); flush(stdout())

### Name: heterose
### Title: Heterosis and Heterobeltiosis
### Aliases: heterose

### ** Examples




cleanEx()
nameEx("indger")
### * indger

flush(stderr()); flush(stdout())

### Name: indger
### Title: Índice de germinação pela contagem subsequente
### Aliases: indger

### ** Examples

with(data,indviab(genot,var1,var2))



cleanEx()
nameEx("indviab")
### * indviab

flush(stderr()); flush(stdout())

### Name: indviab
### Title: Ear Indexes
### Aliases: indviab

### ** Examples




cleanEx()
nameEx("is_ptnerg")
### * is_ptnerg

flush(stderr()); flush(stdout())

### Name: is_ptnerg
### Title: Selection index for protein and grain yield
### Aliases: is_ptnerg

### ** Examples




cleanEx()
nameEx("is_qindustrial")
### * is_qindustrial

flush(stderr()); flush(stdout())

### Name: is_qindustrial
### Title: Industrial quality of wheat
### Aliases: is_qindustrial

### ** Examples




cleanEx()
nameEx("isgr")
### * isgr

flush(stderr()); flush(stdout())

### Name: isgr
### Title: ISGR - Genetic Selection Index for Resilience
### Aliases: isgr

### ** Examples




cleanEx()
nameEx("lai")
### * lai

flush(stderr()); flush(stdout())

### Name: lai
### Title: Leaf Area Index (LAI)
### Aliases: lai

### ** Examples




cleanEx()
nameEx("optemp")
### * optemp

flush(stderr()); flush(stdout())

### Name: optemp
### Title: Plotting the optimum and cardinal temperatures for crops
### Aliases: optemp

### ** Examples




cleanEx()
nameEx("ph")
### * ph

flush(stderr()); flush(stdout())

### Name: ph
### Title: Hectolitre weight of cereals
### Aliases: ph

### ** Examples




cleanEx()
nameEx("plast")
### * plast

flush(stderr()); flush(stdout())

### Name: plast
### Title: Soybean plastochron estimation
### Aliases: plast

### ** Examples




cleanEx()
nameEx("rend_ind")
### * rend_ind

flush(stderr()); flush(stdout())

### Name: rend_ind
### Title: Peeling Index and Industrial Yield
### Aliases: rend_ind

### ** Examples




cleanEx()
nameEx("restr")
### * restr

flush(stderr()); flush(stdout())

### Name: restr
### Title: Restriction of witness variability
### Aliases: restr

### ** Examples




cleanEx()
nameEx("risco")
### * risco

flush(stderr()); flush(stdout())

### Name: risco
### Title: Risk of Disease Occurrence in Soybeans
### Aliases: risco

### ** Examples




### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
