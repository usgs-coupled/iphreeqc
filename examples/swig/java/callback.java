public class callback {
    
    public int year;
    public IPhreeqc iphreeqc;
    public java.util.Vector<VAR> results;
    
    static {
        try {
            System.loadLibrary("iphreeqc_java");
        } catch (UnsatisfiedLinkError e) {
            System.err.println("Native code library failed to load. See the chapter on Dynamic Linking Problems in the SWIG Java documentation for help.\n" + e);
            System.exit(1);
        }
    }

    public callback() {
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
    }
    
    public void Exec() {
        iphreeqc = new IPhreeqc();
        results = new java.util.Vector<VAR>();

        if (iphreeqc.LoadDatabase("phreeqc.dat") != 0) {
            EHandler();
        }

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

        year += 1;

        java.lang.StringBuffer input = new java.lang.StringBuffer();
        input.append("SOLUTION_MODIFY 2\n");
        input.append("   -cb      " + java.lang.Double.toString(results.get(0).getDVal()) + "\n");
        input.append("   -total_h " + java.lang.Double.toString(results.get(1).getDVal()) + "\n");
        input.append("   -total_o " + java.lang.Double.toString(results.get(2).getDVal()) + "\n");
        input.append("   -totals  \n"); 
        input.append("      C     " + java.lang.Double.toString(results.get(3).getDVal()) + "\n");
        input.append("      Ca    " + java.lang.Double.toString(results.get(4).getDVal()) + "\n");
        // run cell 2
        input.append("RUN_CELLS; -cells; 2; END\n");
        if (iphreeqc.RunString(input.toString()) != 0) {
            EHandler();
        }
        // Extract/write results
        ExtractWrite(2);
    }
    
    public static void main(String argv[]) {
        callback c = new callback();
        c.Exec();
    }
}
