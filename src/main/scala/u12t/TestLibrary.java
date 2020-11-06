package u12t;

import alice.tuprolog.*;
import alice.tuprolog.Number;

// run with: java -cp <path-to-libraries>:2p.jar alice.tuprologx.ide.GUILauncher
public class TestLibrary extends Library {

    private int termAsInt(Term t){
        return ((Number)t).intValue();
    }

    // try with: X is sum(10,20).
    public Term sum_2(Term arg0, Term arg1){
        return new Int(termAsInt(arg0)+termAsInt(arg1));
    }

    // try with: X is sum(10,20).
    public Term minus_2(Term arg0, Term arg1){
        return new Int(termAsInt(arg0)-termAsInt(arg1));
    }

    // try with: sum(Y,8,21).
    public boolean sum_3(Term arg0, Term arg1, Term arg){
        try {
            if (arg instanceof Var) {
                return arg.unify(this.getEngine(), sum_2(arg0, arg1));
            }
            if (arg0 instanceof Var) {
                return arg0.unify(this.getEngine(), minus_2(arg, arg1));
            }
            return arg1.unify(this.getEngine(), minus_2(arg, arg0));
        } catch (Exception e){
            return false;
        }
    }
}
