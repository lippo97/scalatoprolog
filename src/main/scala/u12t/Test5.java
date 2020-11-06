package u12t;

import alice.tuprolog.*;

public class Test5 {

    public static void main(String[] args) throws Exception {

        final Prolog engine = new Prolog();
        final Theory t = new Theory(
                "search(E,[E|_]). " + // a space is needed here!
                "search(E,[_|L]):-search(E,L)."
        );
        // engine.setTheory(new FileInputStream("file.pl"));
        engine.setTheory(t);
        final SolveInfo info = engine.solve("search(1,[1,2,3]).");
        System.out.println("" + info.getSolution()); // search(1,[1,2,3])
    }
} 
