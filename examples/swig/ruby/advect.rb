require 'iphreeqc_ruby'

class MyData < Iphreeqc_ruby::BasicCallback

  def initialize()
    # must call superclass ctor
    super
    # initialize instance vars
    @iphreeqc = Iphreeqc_ruby::IPhreeqc.new()
    @results  = []
    @year     = 0
  end

  def ExtractWrite(cell)
    @results = []
    for j in @iphreeqc.GetSelectedOutputColumnCount().times
      @results[j] = Iphreeqc_ruby::VAR.new()
      Iphreeqc_ruby.VarInit(@results[j])
      @iphreeqc.GetSelectedOutputValue(1, j, @results[j])
    end
    puts "Cell #{cell} #{@results[7].dVal.round}\n"
    puts "\tpH: #{@results[5].dVal.round(2)}\tSR(calcite): #{@results[6].dVal.round(2)}\n"
  end

  def EHandler()
    abort(@iphreeqc.GetErrorString())
  end
  
  def Callback(x1, x2, str)
    #
    # Use of a callback is optional.
    #    
    # The callback provides a way to obtain data from a Basic program
    # through the variables x1, x2, and str1, and send data to a 
    # Basic program through the return value of the callback.
    #    
    # The callback function is called whenever CALLBACK(x1, x2, str$)  
    # is used in a Basic program (usually USER_PUNCH). See file "ic".
    #
    if str == 'Year'
      puts "\nCallback for cell #{x1.round}: pH #{x2.round(2)}\n"
      return @year
    end
    return -1.0
  end

  def Exec()
    # Load database
    unless @iphreeqc.LoadDatabase('phreeqc.dat') == 0
      EHandler()
    end
    
    # Set callback
    @iphreeqc.SetBasicCallback(self)
    
    # Define initial conditions and selected output
    @year = 2014
    unless @iphreeqc.RunFile('ic') == 0
      EHandler()
    end

    # Run cell 1
    unless @iphreeqc.RunString('RUN_CELLS; -cells; 1; END') == 0
      EHandler()
    end

    # Extract/write results
    ExtractWrite(1)
    
    # Advect cell 1 solution to cell 2
    @year += 1

    # Define new solutoin composition for cell 2 from cell 1 selected-output
    s = "SOLUTION_MODIFY 2\n"
    s += "   -cb      #{@results[0].dVal}\n"
    s += "   -total_h #{@results[1].dVal}\n"
    s += "   -total_o #{@results[2].dVal}\n"
    s += "   -totals  \n"
    s += "      C     #{@results[3].dVal}\n"
    s += "      Ca    #{@results[4].dVal}\n"
    # run cell 2
    s += "RUN_CELLS; -cells; 2; END\n"
    unless @iphreeqc.RunString(s) == 0
      EHandler()
    end
    # Extract/write results
    ExtractWrite(2)
  end
end

m = MyData.new()
m.Exec()
