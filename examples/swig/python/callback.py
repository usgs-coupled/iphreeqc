import sys
import iphreeqc

#class MyData(iphreeqc.Callback):
class MyData:
	def __init__(self):
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
		
	def MyCallback(self, x1, x2, s1):
		if (s1 == "Year"):
			print '\nCallback for cell ' + str(x1) + ': pH ' + str(x2) + '\n'
			return self.year
		return -1
		

	def Exec(self):
		if (self.iphreeqc.LoadDatabase('phreeqc.dat') != 0):
			self.EHandler()

		self.year = 2014
		if (self.iphreeqc.RunFile('ic') != 0):
			self.EHandler()
			
		if (self.iphreeqc.RunString('RUN_CELLS; -cells; 1; END') != 0):
			self.EHandler()
		
		self.ExtractWrite(1)
		
		self.year += 1
		
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