pkgname <- "phreeqc"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "phreeqc-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('phreeqc')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("ex1")
### * ex1

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ex1
### Title: Example 1-Speciation Calculation
### Aliases: ex1
### Keywords: dataset

### ** Examples

phrLoadDatabaseString(phreeqc.dat)
phrSetOutputStringsOn(TRUE)
phrRunString(ex1)
phrGetOutputStrings()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ex1", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ex10")
### * ex10

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ex10
### Title: Example 10-Aragonite-Strontianite Solid Solution
### Aliases: ex10
### Keywords: dataset

### ** Examples

phrLoadDatabaseString(phreeqc.dat)
phrSetOutputStringsOn(TRUE)
phrRunString(ex10)
phrGetOutputStrings()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ex10", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ex11")
### * ex11

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ex11
### Title: Example 11-Transport and Cation Exchange
### Aliases: ex11
### Keywords: dataset

### ** Examples

phrLoadDatabaseString(phreeqc.dat)
phrSetOutputStringsOn(TRUE)
phrRunString(ex11)
phrGetOutputStrings()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ex11", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ex12")
### * ex12

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ex12
### Title: Example 12-Advective and Diffusive Flux of Heat and Solutes
### Aliases: ex12
### Keywords: dataset

### ** Examples

phrLoadDatabaseString(phreeqc.dat)
phrSetOutputStringsOn(TRUE)
phrRunString(ex12)
phrGetOutputStrings()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ex12", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ex13")
### * ex13

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ex13a
### Title: Example 13-Aragonite-Strontianite Solid Solution
### Aliases: ex13a ex13b ex13c
### Keywords: dataset

### ** Examples

phrLoadDatabaseString(phreeqc.dat)
phrSetOutputStringsOn(TRUE)
phrRunString(ex13a)
phrGetOutputStrings()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ex13", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ex14")
### * ex14

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ex14
### Title: Example 14-Advective Transport, Cation Exchange, Surface
###   Complexation, and Mineral Equilibria
### Aliases: ex14
### Keywords: dataset

### ** Examples

phrLoadDatabaseString(phreeqc.dat)
phrSetOutputStringsOn(TRUE)
phrRunString(ex14)
phrGetOutputStrings()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ex14", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ex15")
### * ex15

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ex15
### Title: Example 15-1D Transport: Kinetic Biodegradation, Cell Growth,
###   and Sorption
### Aliases: ex15
### Keywords: dataset

### ** Examples

# this example takes longer than 5 seconds
phrLoadDatabaseString(ex15.dat)
phrSetOutputStringsOn(TRUE)
## Not run: phrRunString(ex15)
phrGetOutputStrings()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ex15", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ex16")
### * ex16

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ex16
### Title: Example 16-Inverse Modeling of Sierra Spring Waters
### Aliases: ex16
### Keywords: dataset

### ** Examples

phrLoadDatabaseString(phreeqc.dat)
phrSetOutputStringsOn(TRUE)
phrRunString(ex16)
phrGetOutputStrings()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ex16", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ex17")
### * ex17

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ex17
### Title: Example 17-Inverse Modeling With Evaporation
### Aliases: ex17
### Keywords: dataset

### ** Examples

phrLoadDatabaseString(pitzer.dat)
phrSetOutputStringsOn(TRUE)
phrRunString(ex17)
phrGetOutputStrings()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ex17", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ex18")
### * ex18

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ex18
### Title: Example 18-Inverse Modeling of the Madison Aquifer
### Aliases: ex18
### Keywords: dataset

### ** Examples

phrLoadDatabaseString(phreeqc.dat)
phrSetOutputStringsOn(TRUE)
phrRunString(ex18)
phrGetOutputStrings()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ex18", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ex19")
### * ex19

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ex19
### Title: Example 19-Modeling Cd+2 Sorption With Linear, Freundlich, and
###   Langmuir Isotherms, and With a Deterministic Distribution of Sorption
###   Sites for Organic Matter, Clay Minerals, and Iron Oxyhydroxides
### Aliases: ex19
### Keywords: dataset

### ** Examples

phrLoadDatabaseString(phreeqc.dat)
phrSetOutputStringsOn(TRUE)
phrRunString(ex19)
phrGetOutputStrings()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ex19", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ex2")
### * ex2

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ex2
### Title: Example 2-Equilibration With Pure Phases
### Aliases: ex2
### Keywords: dataset

### ** Examples

phrLoadDatabaseString(phreeqc.dat)
phrSetOutputStringsOn(TRUE)
phrRunString(ex2)
phrGetOutputStrings()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ex2", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ex20")
### * ex20

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ex20a
### Title: Example 20-Distribution of Isotopes Between Water and Calcite
### Aliases: ex20a ex20b
### Keywords: dataset

### ** Examples

phrLoadDatabaseString(iso.dat)
phrSetOutputStringsOn(TRUE)
phrRunString(ex20a)
phrGetOutputStrings()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ex20", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ex21")
### * ex21

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ex21
### Title: Example 21-Modeling Diffusion of HTO, 36Cl-, 22Na+, and Cs+ in a
###   Radial Diffusion Cell
### Aliases: ex21
### Keywords: dataset

### ** Examples

phrLoadDatabaseString(phreeqc.dat)
# example 21 requires the selected_output file to be turned on
phrSetSelectedOutputFileOn(1, TRUE)
phrSetOutputStringsOn(TRUE)
# this takes longer than 5 seconds
## Not run: phrRunString(ex21)
phrGetOutputStrings()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ex21", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ex22")
### * ex22

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ex22
### Title: Example 22-Modeling Gas Solubilities: CO2 at High Pressures
### Aliases: ex22
### Keywords: dataset

### ** Examples

phrLoadDatabaseString(phreeqc.dat)
phrSetOutputStringsOn(TRUE)
phrRunString(ex22)
phrGetOutputStrings()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ex22", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ex3")
### * ex3

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ex3
### Title: Example 3-Mixing
### Aliases: ex3
### Keywords: dataset

### ** Examples

phrLoadDatabaseString(phreeqc.dat)
phrSetOutputStringsOn(TRUE)
phrRunString(ex3)
phrGetOutputStrings()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ex3", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ex4")
### * ex4

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ex4
### Title: Example 4-Evaporation and Homogeneous Redox Reactions
### Aliases: ex4
### Keywords: dataset

### ** Examples

phrLoadDatabaseString(phreeqc.dat)
phrSetOutputStringsOn(TRUE)
phrRunString(ex4)
phrGetOutputStrings()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ex4", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ex5")
### * ex5

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ex5
### Title: Example 5-Irreversible Reactions
### Aliases: ex5
### Keywords: dataset

### ** Examples

phrLoadDatabaseString(phreeqc.dat)
phrSetOutputStringsOn(TRUE)
phrRunString(ex5)
phrGetOutputStrings()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ex5", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ex6")
### * ex6

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ex6
### Title: Example 6-Reaction-Path Calculations
### Aliases: ex6
### Keywords: dataset

### ** Examples

phrLoadDatabaseString(phreeqc.dat)
phrSetOutputStringsOn(TRUE)
phrRunString(ex6)
phrGetOutputStrings()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ex6", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ex7")
### * ex7

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ex7
### Title: Example 7-Gas-Phase Calculations
### Aliases: ex7
### Keywords: dataset

### ** Examples

phrLoadDatabaseString(phreeqc.dat)
phrSetOutputStringsOn(TRUE)
phrRunString(ex7)
phrGetOutputStrings()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ex7", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ex8")
### * ex8

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ex8
### Title: Example 8-Surface Complexation
### Aliases: ex8
### Keywords: dataset

### ** Examples

phrLoadDatabaseString(phreeqc.dat)
phrSetOutputStringsOn(TRUE)
# example 8 requires the selected_output file to be turned on
phrSetSelectedOutputFileOn(1, TRUE)
phrRunString(ex8)
phrGetOutputStrings()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ex8", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ex9")
### * ex9

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ex9
### Title: Example 9-Kinetic Oxidation of Dissolved Ferrous Iron With
###   Oxygen
### Aliases: ex9
### Keywords: dataset

### ** Examples

phrLoadDatabaseString(phreeqc.dat)
phrSetOutputStringsOn(TRUE)
phrRunString(ex9)
phrGetOutputStrings()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ex9", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("phrAccumulateLine")
### * phrAccumulateLine

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: phrAccumulateLine
### Title: Accumulate line(s) for input to phreeqc.
### Aliases: phrAccumulateLine

### ** Examples

# this example loads the phreeqc.dat database, accumulates input, and
# runs it
phrLoadDatabaseString(phreeqc.dat)
phrAccumulateLine("TITLE Example 2.--Temperature dependence of solubility")
phrAccumulateLine("                  of gypsum and anhydrite")
phrAccumulateLine("SOLUTION 1 Pure water")
phrAccumulateLine("        pH      7.0")
phrAccumulateLine("        temp    25.0")
phrAccumulateLine("EQUILIBRIUM_PHASES 1")
phrAccumulateLine("        Gypsum          0.0     1.0")
phrAccumulateLine("        Anhydrite       0.0     1.0")
phrAccumulateLine("REACTION_TEMPERATURE 1")
phrAccumulateLine("        25.0 75.0 in 51 steps")
phrAccumulateLine("SELECTED_OUTPUT")
phrAccumulateLine("        -file   ex2.sel")
phrAccumulateLine("        -temperature")
phrAccumulateLine("        -si     anhydrite  gypsum")
phrAccumulateLine("END")
phrSetOutputFileOn(TRUE)
if (is.null(phrRunAccumulated())) {
  cat(paste("see ", phrGetOutputFileName(), ".\n", sep = ""))
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("phrAccumulateLine", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("phrClearAccumulatedLines")
### * phrClearAccumulatedLines

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: phrClearAccumulatedLines
### Title: Clear the accumulated input buffer.
### Aliases: phrClearAccumulatedLines

### ** Examples

# This example loads some keyword input, clears the input, and displays
# the results.
phrAccumulateLine("SOLUTION 1")
phrAccumulateLine("END")
cat("The accumulated input is:", phrGetAccumulatedLines(), sep = "\n")
phrClearAccumulatedLines()
cat("The accumulated input now is:\n", phrGetAccumulatedLines(), sep = "\n")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("phrClearAccumulatedLines", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("phrGetAccumulatedLines")
### * phrGetAccumulatedLines

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: phrGetAccumulatedLines
### Title: Retrieve the accumulated input.
### Aliases: phrGetAccumulatedLines

### ** Examples

# This example loads some keyword input and displays the contents.
phrAccumulateLine("SOLUTION 1")
phrAccumulateLine("END")
cat("The accumulated input is:", phrGetAccumulatedLines(), sep = "\n")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("phrGetAccumulatedLines", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("phrGetComponentList")
### * phrGetComponentList

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: phrGetComponentList
### Title: Retrieve a list containing the current list of components.
### Aliases: phrGetComponentList

### ** Examples

# This example runs the ex2 input file and echos the list of components.
phrLoadDatabaseString(phreeqc.dat)
phrRunString(ex2)
cat("components:\n")
for (comp in phrGetComponentList()) {
  cat(comp, "\n")
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("phrGetComponentList", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("phrGetDumpFileName")
### * phrGetDumpFileName

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: phrGetDumpFileName
### Title: Retrieve the name of the dump file.
### Aliases: phrGetDumpFileName

### ** Examples

phrLoadDatabaseString(phreeqc.dat)
phrSetDumpFileOn(TRUE)

input <-              "SOLUTION 1 Pure water     \n"
input <- paste(input, "EQUILIBRIUM_PHASES 1      \n")
input <- paste(input, "    Calcite 0 10          \n")
input <- paste(input, "SAVE solution 1           \n")
input <- paste(input, "SAVE equilibrium_phases 1 \n")
input <- paste(input, "DUMP                      \n")
input <- paste(input, "    -solution 1           \n")
input <- paste(input, "    -equilibrium_phases  1\n")

if (!is.null(phrRunString(input))) {
  cat(phrGetErrorStrings())
}
cat(paste("see ", phrGetDumpFileName(), "."))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("phrGetDumpFileName", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("phrGetDumpStrings")
### * phrGetDumpStrings

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: phrGetDumpStrings
### Title: Retrieve DUMP strings.
### Aliases: phrGetDumpStrings

### ** Examples

phrLoadDatabaseString(phreeqc.dat)
phrSetDumpStringsOn(TRUE)

input <-              "SOLUTION 1 Pure water     \n"
input <- paste(input, "EQUILIBRIUM_PHASES 1      \n")
input <- paste(input, "    Calcite 0 10          \n")
input <- paste(input, "SAVE solution 1           \n")
input <- paste(input, "SAVE equilibrium_phases 1 \n")
input <- paste(input, "DUMP                      \n")
input <- paste(input, "    -solution 1           \n")
input <- paste(input, "    -equilibrium_phases 1 \n")

if (!is.null(phrRunString(input))) {
  cat(phrGetErrorStrings(), sep = "\n")
}
cat(phrGetDumpStrings(), sep = "\n")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("phrGetDumpStrings", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("phrGetErrorStrings")
### * phrGetErrorStrings

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: phrGetErrorStrings
### Title: Retrieve error string messages.
### Aliases: phrGetErrorStrings

### ** Examples

# loaddatabase should fail
n <- try(phrLoadDatabase("missing.dat"), silent = TRUE)
# if n is non-NULL display error string
if (!is.null(n)) phrGetErrorStrings()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("phrGetErrorStrings", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("phrGetLogFileName")
### * phrGetLogFileName

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: phrGetLogFileName
### Title: Retrieve the name of the log file.
### Aliases: phrGetLogFileName

### ** Examples

# This example checks to see if the log file is turned on
# and prints the appropriate message
if (phrGetLogFileOn()) {
  cat("The name of the log file (is/will be):", phrGetLogFileName(), "\n")
} else {
  cat("The log file is not turned on\n")
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("phrGetLogFileName", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("phrGetLogStrings")
### * phrGetLogStrings

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: phrGetLogStrings
### Title: Retrieve log output.
### Aliases: phrGetLogStrings

### ** Examples

# This example equilibrates pure water with gypsum with the output file on.
phrLoadDatabaseString(phreeqc.dat)
phrSetOutputFileOn(TRUE)

input <- vector(mode="character")
input <- c(input, "SOLUTION 1 Pure water ")
input <- c(input, "EQUILIBRIUM_PHASES 1  ")
input <- c(input, "  Gypsum 0 10         ")
input <- c(input, "KNOBS                 ")
input <- c(input, "  -logfile TRUE       ")

if (is.null(phrRunString(input))) {
  log <- phrGetLogStrings()
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("phrGetLogStrings", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("phrGetOutputFileName")
### * phrGetOutputFileName

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: phrGetOutputFileName
### Title: Retrieve the name of the output file.
### Aliases: phrGetOutputFileName

### ** Examples

# This example equilibrates pure water with gypsum with the output file on.
phrLoadDatabaseString(phreeqc.dat)
phrSetOutputFileOn(TRUE)

input <- vector(mode="character")
input <- c(input, "SOLUTION 1 Pure water ")
input <- c(input, "EQUILIBRIUM_PHASES 1  ")
input <- c(input, "  Gypsum 0 10         ")

if (is.null(phrRunString(input))) {
  output <- readLines(phrGetOutputFileName())
  unlink(phrGetOutputFileName())  # tidy up
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("phrGetOutputFileName", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("phrGetOutputStrings")
### * phrGetOutputStrings

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: phrGetOutputStrings
### Title: Retrieve standard phreeqc output.
### Aliases: phrGetOutputStrings

### ** Examples

# This example equilibrates pure water with calcite and displays
# the results
phrLoadDatabaseString(phreeqc.dat)
phrSetOutputStringsOn(TRUE)

input <- vector(mode="character")
input <- c(input, "SOLUTION 1 Pure water ")
input <- c(input, "EQUILIBRIUM_PHASES 1  ")
input <- c(input, "  Gypsum 0 10         ")

if (is.null(phrRunString(input))) {
  cat(phrGetOutputStrings(), sep = "\n")
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("phrGetOutputStrings", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("phrGetSelectedOutput")
### * phrGetSelectedOutput

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: phrGetSelectedOutput
### Title: Returns the contents of the selected output as a list of data
###   frames.
### Aliases: phrGetSelectedOutput

### ** Examples

# Load database and run ex2
phrLoadDatabaseString(phreeqc.dat)
phrRunString(ex2)

# display a summary of the results
df <- phrGetSelectedOutput()
summary(df$n1)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("phrGetSelectedOutput", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("phrGetSelectedOutputFileName")
### * phrGetSelectedOutputFileName

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: phrGetSelectedOutputFileName
### Title: Retrieve the name of the selected_output file.
### Aliases: phrGetSelectedOutputFileName

### ** Examples

# This example equilibrates pure water with calcite at various temperatures
# and displays the name of the selected_output file.
phrLoadDatabaseString(phreeqc.dat)
phrSetSelectedOutputFileOn(1, TRUE)
phrSetSelectedOutputFileName(1, "ex2.sel")

input <- c(
  'SOLUTION 1 Pure water     ',
  'EQUILIBRIUM_PHASES 1      ',
  '    Calcite    0.0   1.0  ',
  'REACTION_TEMPERATURE 1    ',
  '    25.0 75.0 in 51 steps ',
  'SELECTED_OUTPUT 1         ',
  '    -temperature          ',
  '    -si     calcite       '
  )


if (is.null(phrRunString(input))) {
  cat("see", phrGetSelectedOutputFileName(1))
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("phrGetSelectedOutputFileName", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("phrGetWarningStrings")
### * phrGetWarningStrings

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: phrGetWarningStrings
### Title: Retrieve warning messages.
### Aliases: phrGetWarningStrings

### ** Examples

# This example loads the phreeqc.dat database and attempts to use the
# DATABASE keyword to set the database to wateq4f.dat.  A warning is
# displayed stating that the DATABASE keyword is ignored in the 'R'
# implementation.
phrLoadDatabaseString(phreeqc.dat)
phrAccumulateLine("DATABASE wateq4f.dat")
phrAccumulateLine("SOLUTION 1")
phrRunAccumulated()
if (!is.null(phrGetWarningStrings())) {
  cat(phrGetWarningStrings(), sep = "\n")
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("phrGetWarningStrings", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("phrLoadDatabase")
### * phrLoadDatabase

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: phrLoadDatabase
### Title: Load a phreeqc database file
### Aliases: phrLoadDatabase

### ** Examples

# create temporary database file
tf <- tempfile()
writeLines(phreeqc.dat, tf)

if (is.null(phrLoadDatabase(tf))) {
  cat("database ok\n")
} else {
  cat("database contains errors\n")
}

# delete temporary database file
unlink(tf)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("phrLoadDatabase", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("phrLoadDatabaseString")
### * phrLoadDatabaseString

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: phrLoadDatabaseString
### Title: Load a phreeqc database as a string or a list of strings.
### Aliases: phrLoadDatabaseString
### Keywords: interface

### ** Examples

# this example loads the phreeqc.dat database, turns on the
# output file and runs ex2 as a string
phrLoadDatabaseString(phreeqc.dat)
phrSetOutputFileOn(TRUE)
if (is.null(phrRunString(ex2))) {
  cat(paste("see ", phrGetOutputFileName(), ".\n", sep = ""))
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("phrLoadDatabaseString", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("phrRunAccumulated")
### * phrRunAccumulated

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: phrRunAccumulated
### Title: Runs the accumulated input.
### Aliases: phrRunAccumulated

### ** Examples

# turn on the output file
phrSetOutputFileOn(TRUE)

# load the phreeqc.dat database
phrLoadDatabaseString(phreeqc.dat)

# accumulate the input
phrAccumulateLine("TITLE Example 2.--Temperature dependence of solubility")
phrAccumulateLine("                  of gypsum and anhydrite")
phrAccumulateLine("SOLUTION 1 Pure water")
phrAccumulateLine("        pH      7.0")
phrAccumulateLine("        temp    25.0")
phrAccumulateLine("EQUILIBRIUM_PHASES 1")
phrAccumulateLine("        Gypsum          0.0     1.0")
phrAccumulateLine("        Anhydrite       0.0     1.0")
phrAccumulateLine("REACTION_TEMPERATURE 1")
phrAccumulateLine("        25.0 75.0 in 51 steps")
phrAccumulateLine("SELECTED_OUTPUT")
phrAccumulateLine("        -file   ex2.sel")
phrAccumulateLine("        -temperature")
phrAccumulateLine("        -si     anhydrite  gypsum")
phrAccumulateLine("END")

# run it and echo the name of the output file
if (is.null(phrRunAccumulated())) {
  cat(paste("see ", phrGetOutputFileName(), ".\n", sep = ""))
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("phrRunAccumulated", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("phrRunFile")
### * phrRunFile

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: phrRunFile
### Title: Run phreeqc input file
### Aliases: phrRunFile

### ** Examples

# load the phreeqc.dat database
phrLoadDatabaseString(phreeqc.dat)

# create ex2 if it doesn't exist
if (!file.exists("ex2")) writeLines(ex2, "ex2")

# run ex2
if (is.null(phrRunFile("ex2"))) {
  cat("use phrGetSelectedOutput() to see results.")
}

unlink("ex2")  # tidy up



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("phrRunFile", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("phrRunString")
### * phrRunString

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: phrRunString
### Title: Runs phreeqc using the given string as input.
### Aliases: phrRunString
### Keywords: interface

### ** Examples

#
# This example accumulates phreeqc input into a character vector
# and runs it.
#

# load phreeqc.dat file
phrLoadDatabaseString(phreeqc.dat)

# create input
input <- vector()
input <- c(input, "SOLUTION 1")
input <- c(input, "  temp 25.0")
input <- c(input, "  pH    7.0")

# turn on output
phrSetOutputFileOn(TRUE)

# run input
phrRunString(input)
cat(paste("see", phrGetOutputFileName(), "."))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("phrRunString", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("phrSetDumpFileName")
### * phrSetDumpFileName

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: phrSetDumpFileName
### Title: Set the name of the dump file.
### Aliases: phrSetDumpFileName

### ** Examples

# This example equilibrates pure water with calcite and writes the
# dump results to file.
phrLoadDatabaseString(phreeqc.dat)
phrSetDumpFileOn(TRUE)
phrSetDumpFileName("phreeqc.dump")
input <- c(
  'SOLUTION 1 Pure water     ',
  'EQUILIBRIUM_PHASES 1      ',
  '    Calcite 0 10          ',
  'SAVE solution 1           ',
  'SAVE equilibrium_phases 1 ',
  'DUMP                      ',
  '    -solution 1           ',
  '    -equilibrium_phases 1 '
  )

if (is.null(phrRunString(input))) {
  cat("see", phrGetDumpFileName(), "\n")
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("phrSetDumpFileName", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("phrSetDumpFileOn")
### * phrSetDumpFileOn

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: phrSetDumpFileOn
### Title: Set the dump file on/off.
### Aliases: phrSetDumpFileOn

### ** Examples

# This example equilibrates pure water with calcite and writes the
# dump results to file.
phrLoadDatabaseString(phreeqc.dat)
phrSetDumpFileOn(TRUE)
input <- c(
  'SOLUTION 1 Pure water     ',
  'EQUILIBRIUM_PHASES 1      ',
  '    Calcite 0 10          ',
  'SAVE solution 1           ',
  'SAVE equilibrium_phases 1 ',
  'DUMP                      ',
  '    -solution 1           ',
  '    -equilibrium_phases 1 '
  )

if (is.null(phrRunString(input))) {
  cat("see", phrGetDumpFileName(), "\n")
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("phrSetDumpFileOn", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("phrSetDumpStringsOn")
### * phrSetDumpStringsOn

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: phrSetDumpStringsOn
### Title: Set dump strings on/off.
### Aliases: phrSetDumpStringsOn

### ** Examples

# This example equilibrates pure water with calcite and echos the
# dump strings.
phrLoadDatabaseString(phreeqc.dat)
phrSetDumpStringsOn(TRUE)
input <- c(
  'SOLUTION 1 Pure water     ',
  'EQUILIBRIUM_PHASES 1      ',
  '    Calcite 0 10          ',
  'SAVE solution 1           ',
  'SAVE equilibrium_phases 1 ',
  'DUMP                      ',
  '    -solution 1           ',
  '    -equilibrium_phases 1 '
  )

if (is.null(phrRunString(input))) {
  cat("Dump:\n")
  cat(phrGetDumpStrings(), sep = "\n")
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("phrSetDumpStringsOn", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("phrSetErrorFileName")
### * phrSetErrorFileName

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: phrSetErrorFileName
### Title: Set the name of the error file.
### Aliases: phrSetErrorFileName

### ** Examples

# This example equilibrates pure water with calcite and displays
# the log file name.
phrLoadDatabaseString(phreeqc.dat)
phrSetLogFileOn(TRUE)
phrSetLogFileName("phreeqc.log")
input <- c(
  'SOLUTION 1 Pure water ',
  'EQUILIBRIUM_PHASES 1  ',
  '    Calcite 0 10      '
  )

if (is.null(phrRunString(input))) {
  cat("see", phrGetErrorFileName(), "\n")
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("phrSetErrorFileName", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("phrSetErrorFileOn")
### * phrSetErrorFileOn

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: phrSetErrorFileOn
### Title: Set error file on/off.
### Aliases: phrSetErrorFileOn

### ** Examples

# This example attempts to run ex1, fails, and writes the error
# message to the error file (no database is loaded).
phrSetErrorFileOn(TRUE)
phrSetErrorFileName("phreeqc.errors")
if (!is.null(try(phrRunString(ex1), silent=TRUE))) {
  cat("see", phrGetErrorFileName(), "\n")
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("phrSetErrorFileOn", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("phrSetErrorStringsOn")
### * phrSetErrorStringsOn

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: phrSetErrorStringsOn
### Title: Set error strings on/off.
### Aliases: phrSetErrorStringsOn

### ** Examples

# This example attempts to run ex1, fails, and displays the error message
# (no database is loaded).
phrSetErrorStringsOn(TRUE)
if (!is.null(try(phrRunString(ex1), silent=TRUE))) {
  cat(phrGetErrorStrings(), "\n")
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("phrSetErrorStringsOn", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("phrSetLogFileName")
### * phrSetLogFileName

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: phrSetLogFileName
### Title: Set the name of the log file.
### Aliases: phrSetLogFileName

### ** Examples

# This example equilibrates pure water with calcite and displays
# the log file name.
phrLoadDatabaseString(phreeqc.dat)
phrSetLogFileOn(TRUE)
phrSetLogFileName("phreeqc.log")
input <- c(
  'SOLUTION 1 Pure water ',
  'EQUILIBRIUM_PHASES 1  ',
  '    Calcite 0 10      ',
  'KNOBS                 ',
  '    -logfile true     '
  )

if (is.null(phrRunString(input))) {
  cat("see", phrGetLogFileName(), "\n")
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("phrSetLogFileName", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("phrSetLogFileOn")
### * phrSetLogFileOn

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: phrSetLogFileOn
### Title: Set log file on/off.
### Aliases: phrSetLogFileOn

### ** Examples

# This example runs ex2 with the log file turned on.
phrLoadDatabaseString(phreeqc.dat)
phrSetLogStringsOn(TRUE)

# turn logging on
phrAccumulateLine("KNOBS; -logfile true")
phrRunAccumulated()

if (is.null(phrRunString(ex2))) {
  cat("see", phrGetLogFileName(), "\n")
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("phrSetLogFileOn", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("phrSetLogStringsOn")
### * phrSetLogStringsOn

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: phrSetLogStringsOn
### Title: Set log strings on/off.
### Aliases: phrSetLogStringsOn

### ** Examples

# This example runs ex2 with log strings turned on.
phrLoadDatabaseString(phreeqc.dat)
phrSetLogStringsOn(TRUE)

# turn logging on
phrAccumulateLine("KNOBS; -logfile true")
phrRunAccumulated()

if (is.null(phrRunString(ex2))) {
  cat(phrGetLogStrings(), sep = "\n")
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("phrSetLogStringsOn", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("phrSetOutputFileName")
### * phrSetOutputFileName

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: phrSetOutputFileName
### Title: Set the name of the output file.
### Aliases: phrSetOutputFileName

### ** Examples

# This example equilibrates pure water with calcite and displays
# the resulting file name.
phrLoadDatabaseString(phreeqc.dat)
phrSetOutputFileOn(TRUE)
phrSetOutputFileName("phreeqc.output")
input <- c(
  'SOLUTION 1 Pure water ',
  'EQUILIBRIUM_PHASES 1  ',
  '    Calcite 0 10      '
  )

if (is.null(phrRunString(input))) {
  cat("see", phrGetOutputFileName(), "\n")
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("phrSetOutputFileName", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("phrSetOutputFileOn")
### * phrSetOutputFileOn

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: phrSetOutputFileOn
### Title: Set output file on/off.
### Aliases: phrSetOutputFileOn

### ** Examples

# This example runs ex2 with the output file turned on.

# write temporary input file
tf <- tempfile()
writeLines(ex2, tf)

# load database and run input file
phrLoadDatabaseString(phreeqc.dat)
phrSetOutputFileOn(TRUE)
if (is.null(phrRunFile(tf))) {
  cat("see", phrGetOutputFileName(), "\n")
}

# delete temporary input file
unlink(tf)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("phrSetOutputFileOn", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("phrSetOutputStringsOn")
### * phrSetOutputStringsOn

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: phrSetOutputStringsOn
### Title: Set output strings on/off.
### Aliases: phrSetOutputStringsOn

### ** Examples

# This example equilibrates pure water with calcite and displays
# the results.
phrLoadDatabaseString(phreeqc.dat)
phrSetOutputStringsOn(TRUE)
input <- c(
  'SOLUTION 1 Pure water ',
  'EQUILIBRIUM_PHASES 1  ',
  '    Calcite 0 10      '
  )

if (is.null(phrRunString(input))) {
  cat(phrGetOutputStrings(), sep = "\n")
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("phrSetOutputStringsOn", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("phrSetSelectedOutputFileName")
### * phrSetSelectedOutputFileName

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: phrSetSelectedOutputFileName
### Title: Set the name of the selected_output file.
### Aliases: phrSetSelectedOutputFileName

### ** Examples

# This example equilibrates pure water with calcite at various temperatures
# and displays the name of the selected_output file.
phrLoadDatabaseString(phreeqc.dat)
phrSetSelectedOutputFileOn(1, TRUE)
phrSetSelectedOutputFileName(1, "ex2.sel")

input <- c(
  'SOLUTION 1 Pure water     ',
  'EQUILIBRIUM_PHASES 1      ',
  '    Calcite    0.0   1.0  ',
  'REACTION_TEMPERATURE 1    ',
  '    25.0 75.0 in 51 steps ',
  'SELECTED_OUTPUT 1         ',
  '    -temperature          ',
  '    -si     calcite       '
  )


if (is.null(phrRunString(input))) {
  cat("see", phrGetSelectedOutputFileName(1))
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("phrSetSelectedOutputFileName", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("phrSetSelectedOutputFileOn")
### * phrSetSelectedOutputFileOn

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: phrSetSelectedOutputFileOn
### Title: Set selected_output file on/off.
### Aliases: phrSetSelectedOutputFileOn

### ** Examples

# This example equilibrates pure water with calcite at various temperatures
# and displays the name of the selected_output file.
phrLoadDatabaseString(phreeqc.dat)
phrSetSelectedOutputFileOn(1, TRUE)
phrSetSelectedOutputFileName(1, "ex2.sel")

input <- c(
  'SOLUTION 1 Pure water     ',
  'EQUILIBRIUM_PHASES 1      ',
  '    Calcite    0.0   1.0  ',
  'REACTION_TEMPERATURE 1    ',
  '    25.0 75.0 in 51 steps ',
  'SELECTED_OUTPUT 1         ',
  '    -temperature          ',
  '    -si     calcite       '
  )


if (is.null(phrRunString(input))) {
  cat("see", phrGetSelectedOutputFileName(1))
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("phrSetSelectedOutputFileOn", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("phreeqc-package")
### * phreeqc-package

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: phreeqc-package
### Title: R interface to the PHREEQC geochemical modeling program.
### Aliases: phreeqc phreeqc-package
### Keywords: package

### ** Examples

#########################################################################
# Run ex2 and plot results
#########################################################################

# load the phreeqc.dat database
phrLoadDatabaseString(phreeqc.dat)

# run example 2
phrRunString(ex2)

# retrieve selected_output as a list of data.frame
so <- phrGetSelectedOutput()

# plot the results
attach(so$n1)
title  <- "Gypsum-Anhydrite Stability"
xlabel <- "Temperature, in degrees celsius"
ylabel <- "Saturation index"
plot(temp.C., si_gypsum, main = title, xlab = xlabel, ylab = ylabel,
     col = "darkred", xlim = c(25, 75), ylim = c(-0.4, 0.0))
points(temp.C., si_anhydrite, col = "darkgreen")
legend("bottomright", c("Gypsum", "Anhydrite"),
       col = c("darkred", "darkgreen"), pch = c(1, 1))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("phreeqc-package", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
