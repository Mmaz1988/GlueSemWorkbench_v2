package main;

import java.util.Iterator;
import java.util.List;

import glueSemantics.linearLogic.Premise;


public class NaturalDeductionProof {
	public static class PremiseInfo{
		public String semanticSide;
		public String linearLogicSide;
		public String premiseIds;
		public PremiseInfo(){
		}
		public PremiseInfo(String s, String l, String p){
			semanticSide=new String(s);
			linearLogicSide=new String(l);
			premiseIds=new String(p);
		}
	}
	public static NaturalDeductionProof createTree(Premise p) {
		if (p==null)return null;
		NaturalDeductionProof myProof = new NaturalDeductionProof();
		myProof.semanticSide = p.getSemTerm().toString();
		myProof.linearLogicSide = p.getGlueTerm().getReverseCompiledString();
		
		List<Premise> list = p.getGlueTerm().assumptions2;
		Iterator<Premise> it = list.iterator();
		int counter=0;
		while (it.hasNext()) {
			Premise pr = it.next();
			if(pr!=null)
				{
				counter++;
				}
		}
		list = p.getGlueTerm().assumptions2;
		it = list.iterator();
		myProof.assumptions=new NaturalDeductionProof.PremiseInfo[counter];
		counter=0;
		myProof.subproof1=createTree(p.comb_a);
		myProof.subproof2=createTree(p.comb_b);
		while (it.hasNext()) {
			Premise pr = it.next();
			if(pr!=null)
				{
				NaturalDeductionProof myNewProof = new NaturalDeductionProof();
				myNewProof.semanticSide = "\\" + pr.getSemTerm().toString() + ".(" + myProof.semanticSide + ")";
				myNewProof.subproof1 = myProof;
				myNewProof.type="I"+pr.getPremiseIDs().toString();
				myNewProof.linearLogicSide = pr.getGlueTerm().getReverseCompiledString() + " -o " + myProof.linearLogicSide;
				
				/*myProof.assumptions[counter] = new NaturalDeductionProof.PremiseInfo(pr.getSemTerm().toString() ,pr.getGlueTerm().toString(), pr.getPremiseIDs().toString() );*/

				myProof = myNewProof;
				counter++;
				}
		}
		if (counter>0) {
			//myProof.type="Z";
		}
		else myProof.type="E";
		return myProof;
	}
	public String semanticSide;
	public String linearLogicSide;
	public String type;
	public PremiseInfo[] assumptions;
	public NaturalDeductionProof subproof1;
	public NaturalDeductionProof subproof2;
}
