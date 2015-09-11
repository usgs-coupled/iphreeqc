public class main {
	public static void main(String argv[]) {
		System.loadLibrary("iphreeqc_java");
		IPhreeqc i = new IPhreeqc();

		if (i.LoadDatabase("phreeqc.dat") != 0) {
			System.out.println(i.GetErrorString());
			System.exit(1);
		}

		i.SetOutputStringOn(true);

		java.lang.StringBuffer input = new java.lang.StringBuffer();
		input.append("SOLUTION 1 Pure water \n");
		input.append("EQUILIBRIUM_PHASES 1  \n");
		input.append("    Calcite 0 10      \n");

		if (i.RunString(input.toString()) == 0) {
			System.out.println(i.GetOutputString());
		} else {
			System.out.println(i.GetErrorString());
		}
	}
}
