package u12t;/*
%createArray(@Size,-OutArray)
%getSize(@Array,-Size)
%elem(@Array,+Pos,?Elem)
%tolist(+Array,-List)
*/
import alice.tuprolog.*;
import java.util.*;

// exec with java -cp 2p.jar:. alice.tuprologx.ide.GUILauncher
// try with: newarray(5,X),size(X,N),elem(X,3,10),tolist(X,L).

public class ArrayLibrary extends Library{
	private Vector<int[]> arrays=new Vector<int[]>();
	public boolean newarray_2(Term size, Var v){
		int s=((Int)size.getTerm()).intValue();
		arrays.add(new int[s]);
		return v.unify(this.getEngine(),new Struct("array",new Int(arrays.size()-1)));
	}
	public boolean size_2(Term array,Term v){
		Struct s_array=(Struct)array.getTerm();
		if (arrayWellFormed(s_array)){
			int id=((Int)s_array.getArg(0)).intValue();
			return v.unify(this.getEngine(),new Int(arrays.get(id).length));
		}
		return false;
	}
	public boolean elem_3(Term array, Term pos, Term t){
		Struct s_array=(Struct)array.getTerm();
		if (arrayWellFormed(s_array)){
			int id=((Int)s_array.getArg(0)).intValue();
			int p=((Int)pos.getTerm()).intValue();
			if (!t.isGround()){
				return ((Var)t).unify(this.getEngine(),new Int(arrays.get(id)[p]));
			} else if (t.getTerm() instanceof Int){
				arrays.get(id)[p]=((Int)t.getTerm()).intValue();
				return true;
			}
		}
		return false;
	}
	public boolean tolist_2(Term array, Term v){
		Struct s_array=(Struct)array.getTerm();
		if (arrayWellFormed(s_array)){
			int id=((Int)s_array.getArg(0)).intValue();
			Int[] ii=new Int[arrays.get(id).length];
			for (int i=0;i<ii.length;i++){
				ii[i]=new Int(arrays.get(id)[i]);
			}
			return v.unify(this.getEngine(),new Struct(ii));
		}
		return false;
	}

	private boolean arrayWellFormed(Struct array){
		return array.getName().equals("array") && array.getArity()==1 && array.getArg(0) instanceof Int;
	}
}
