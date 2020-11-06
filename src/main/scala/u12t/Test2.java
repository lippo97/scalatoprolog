package u12t;

import alice.tuprolog.*;

public class Test2 {

    public static void main(String[] args) throws Exception {

        final Prolog engine = new Prolog();
        SolveInfo info = engine.solve("append(X,Y,[1,2,3]).");
        while (info.isSuccess()){
            System.out.print("solution: "+info.getSolution()+" - ");
            System.out.println("bindings: X/"+info.getTerm("X")
                                       +" Y/"+info.getTerm("Y"));
            if (engine.hasOpenAlternatives()){
                info=engine.solveNext();
            } else {
                break;
            }
        }
        // solution: append([],[1,2,3],[1,2,3]) - bindings: X/[] Y/[1,2,3]
        // ..
        // solution: append([1,2,3],[],[1,2,3]) - bindings: X/[1,2,3] Y/[]
    }
} 