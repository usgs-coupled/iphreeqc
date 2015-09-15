require 'iphreeqc_ruby'

i = Iphreeqc_ruby::IPhreeqc.new()

unless i.LoadDatabase("phreeqc.dat") == 0
  printf "%s", i.GetErrorString()
  exit
end

i.SetOutputStringOn(true)

input = "SOLUTION 1 Pure water \n"
input += "EQUILIBRIUM_PHASES 1 \n"
input += "  Calcite 0 10 \n"

unless i.RunString(input) == 0
  printf "%s", i.GetErrorString()
  exit
end

printf "%s", i.GetOutputString()
