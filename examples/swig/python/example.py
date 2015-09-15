import iphreeqc
i = iphreeqc.IPhreeqc()

if i.LoadDatabase("phreeqc.dat") != 0:
    print(i.GetOutputString())

i.SetOutputStringOn(True)

input =  "SOLUTION 1 Pure water \n"
input += "EQUILIBRIUM_PHASES 1  \n"
input += "   Calcite 0 10       \n"

if i.RunString(input) == 0:
    print(i.GetOutputString())
else:
    print(i.GetErrorString())
