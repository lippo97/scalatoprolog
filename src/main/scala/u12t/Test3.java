package u12t;

import alice.tuprolog.*;

public class Test3 {

    public static void main(String[] args) throws Exception {
        final Prolog engine = new Prolog();
        // [1]
        final Term list1=engine.toTerm("[1]");
        // [2,3]
        final Term list2=new Struct(new Term[]{new Int(2),new Int(3)});
        // append([1],[2,3],X).
        final Term app=new Struct("append",list1,list2,new Var("X"));
        final SolveInfo info = engine.solve(app);
        System.out.println(info.getSolution());
        // "append([1],[2,3],[1,2,3])"
    }
} 
