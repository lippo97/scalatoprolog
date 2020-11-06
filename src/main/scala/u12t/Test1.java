package u12t;

// libraryDependencies += "it.unibo.alice.tuprolog" % "tuprolog" % "3.3.0"
import alice.tuprolog.*;

public class Test1 {

    public static void main(String[] args) throws Exception {
        final Prolog engine = new Prolog();
        final SolveInfo info = engine.solve("append([1],[2,3],X).");
        System.out.println(info.getSolution());
        // "append([1],[2,3],[1,2,3])"
    }
} 