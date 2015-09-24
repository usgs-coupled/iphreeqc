import sys
import iphreeqc

class MyData(iphreeqc.BasicCallback):
	def __init__(self):
		# must call base class ctor
		iphreeqc.BasicCallback.__init__(self)
		
		# Create module
		self.iphreeqc = iphreeqc.IPhreeqc()
		self.results  = []
		self.year     = 0.0
		
	
	def ExtractWrite(self, cell):
		self.results = []
		for j in range(0, self.iphreeqc.GetSelectedOutputColumnCount()):
			v = iphreeqc.VAR()
			iphreeqc.VarInit(v)
			self.iphreeqc.GetSelectedOutputValue(1, j, v)
			self.results.append(v)
		print 'Cell {0}  {1}'.format(cell, int(self.results[7].dVal))
		print '\tpH: {0:.2f}\tSR(calcite): {1:.2f}'.format(self.results[5].dVal, self.results[6].dVal)

	def EHandler(self):
		sys.exit(self.iphreeqc.GetErrorString())
		
	def Callback(self, x1, x2, s1):
		if (s1 == "Year"):
			print '\nCallback for cell {0}: pH {1:.2f}'.format(int(x1), x2)
			return float(self.year)
		return float(-1)

	def Exec(self):
		# Load database
		if (self.iphreeqc.LoadDatabase('phreeqc.dat') != 0):
			self.EHandler()

		# Set callback
		self.iphreeqc.SetBasicCallback(self)
		
		# Define initial conditions and selected output
		self.year = 2014
		if (self.iphreeqc.RunFile('ic') != 0):
			self.EHandler()
		
		# Run cell 1
		if (self.iphreeqc.RunString('RUN_CELLS; -cells; 1; END') != 0):
			self.EHandler()
		
        # Extract/write results
		self.ExtractWrite(1)
		
		# Advect cell 1 solution to cell 2
		self.year += 1
		
		# Define new solution composition for cell 2 from cell 1 selected-output
		s = 'SOLUTION_MODIFY 2' + '\n'
		s += '   -cb      ' + str(self.results[0].dVal) + '\n'		
		s += '   -total_h ' + str(self.results[1].dVal) + '\n'
		s += '   -total_o ' + str(self.results[2].dVal) + '\n'
		s += '   -totals  ' + '\n'
		s += '      C     ' + str(self.results[3].dVal) + '\n'
		s += '      Ca    ' + str(self.results[4].dVal) + '\n'	
		# run cell 2
		s += 'RUN_CELLS; -cells; 2; END\n'
		
		if (self.iphreeqc.RunString(s) != 0):
			self.EHandler()

		self.ExtractWrite(2)

if __name__ == '__main__':
	mydata = MyData()
	mydata.Exec()