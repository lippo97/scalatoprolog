package u12t;

import alice.tuprolog.*;

public class Test4 {

    public static void main(String[] args) throws Exception {
        final Prolog engine = new Prolog();
        Term t=new Struct();
        // t will be [0,1,2,...,9,10]
        for (int i=10;i>=0;i--){
            t=new Struct(new Int(i),t);
        }
        final Term app=new Struct("append",t,t,new Var("X"));
        final SolveInfo info = engine.solve(app);
        final Struct t2=(Struct)info.getTerm("X");
        for(java.util.Iterator i=t2.listIterator();i.hasNext();){
            System.out.print(" "+i.next()); // 0 1 2 .. 10 0 1 2 .. 10
        }
    }
} 
