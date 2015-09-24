public class advect extends BasicCallback {

	// data members
    public int year;
    public IPhreeqc iphreeqc;
    public java.util.Vector<VAR> results;

	// load the dll/so
    static {
        try {
            System.loadLibrary("iphreeqc_java");
        } catch (UnsatisfiedLinkError e) {
            System.err.println("Native code library failed to load. See the chapter on Dynamic Linking Problems in the SWIG Java documentation for help.\n" + e);
            System.exit(1);
        }
    }

    public advect() {
        year = 0;
        iphreeqc = null;
        results = null;
    }

    public void ExtractWrite(int cell) {
        results.removeAllElements();
        for (int j = 0; j < iphreeqc.GetSelectedOutputColumnCount(); ++j) {
            VAR v = new VAR();
            iphreeqc_java.VarInit(v);
            iphreeqc.GetSelectedOutputValue(1, j, v);
            results.add(v);
        }
        System.out.format("Cell %d %d\n", cell, (int)results.get(7).getDVal());
        System.out.format("\tpH: %.2f\tSR(calcite):  %.2f\n", results.get(5).getDVal(), results.get(6).getDVal());
    }

    public void EHandler() {
        System.out.println(iphreeqc.GetErrorString());
		System.exit(1);
    }

    public double Callback(double x1, double x2, String str) {
		/*
		Use of a callback is optional.

		The callback provides a way to obtain data from a Basic program
		through the variables x1, x2, and str1, and send data to a
		Basic program through the return value of the callback.

		The callback function is called whenever CALLBACK(x1, x2, str$)
		is used in a Basic program (usually USER_PUNCH). See file "ic".
		*/
		if (str.compareTo("Year") == 0) {
			System.err.format("\nCallback for cell %d: pH %8.2f\n", (int)x1, x2);
			return year;
		}
		return -1.0;
	}

    public void Exec() {
		// Create module
        iphreeqc = new IPhreeqc();
        results = new java.util.Vector<VAR>();

		// Load database
        if (iphreeqc.LoadDatabase("phreeqc.dat") != 0) {
            EHandler();
        }

        // Set callback
        iphreeqc.SetBasicCallback(this);

		// Define initial conditions and selected output
        year = 2014;
        if (iphreeqc.RunFile("ic") != 0) {
            EHandler();
        }

        // Run cell 1
        if (iphreeqc.RunString("RUN_CELLS; -cells; 1; END") != 0) {
            EHandler();
        }

        // Extract/write results
        ExtractWrite(1);

		// Advect cell 1 solution to cell 2
        year += 1;

		// Define new solution composition for cell 2 from cell 1 selected-output
        StringBuffer input = new StringBuffer();
        input.append("SOLUTION_MODIFY 2\n");
        input.append("   -cb      " + Double.toString(results.get(0).getDVal()) + "\n");
        input.append("   -total_h " + Double.toString(results.get(1).getDVal()) + "\n");
        input.append("   -total_o " + Double.toString(results.get(2).getDVal()) + "\n");
        input.append("   -totals  \n");
        input.append("      C     " + Double.toString(results.get(3).getDVal()) + "\n");
        input.append("      Ca    " + Double.toString(results.get(4).getDVal()) + "\n");
        // run cell 2
        input.append("RUN_CELLS; -cells; 2; END\n");
        if (iphreeqc.RunString(input.toString()) != 0) {
            EHandler();
        }
        // Extract/write results
        ExtractWrite(2);
    }

    public static void main(String argv[]) {
        advect a = new advect();
        a.Exec();
    }
}
