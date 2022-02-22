package main;

import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
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
	private static boolean changeControl=false;
	private static void removeAssumption(NaturalDeductionProof node, int idd) {
		if (node ==null) return;
		//if (node.myId==idd) return;
		Iterator<Integer> it =node.assumptions.iterator();
		int num = 0;
		while(it.hasNext()) {
			int n = it.next();
			if (n == idd) {
				node.assumptions.remove(num);
				num --;
				}
			num++;
		}
		removeAssumption(node.subproof1, idd);
		removeAssumption(node.subproof2, idd);
	}
	private static NaturalDeductionProof find(NaturalDeductionProof node, int idd) {
		if (node ==null) return null;
		if (node.myId==idd) return node;
		NaturalDeductionProof node1=find(node.subproof1,idd);
		if (node1!=null) return node1;
		return find(node.subproof2,idd);
		}
	public static NaturalDeductionProof getNaturalDeductionProof(Premise p) {

		NaturalDeductionProof algorithmStyle = createTree( p );
		
		NaturalDeductionProof natural = new NaturalDeductionProof();
		
		natural.subproof1 = algorithmStyle;
		
		while(NaturalDeductionProof.convert(algorithmStyle,natural,0)) {};
		String s = natural.subproof1.prettyPrint(); 
		return natural.subproof1;
	}
	
	private String mergeLeftRight(String left, String right) {
		String merged ="";
		String linesLeft[] = left.split("\\r?\\n");
		String linesRight[] = right.split("\\r?\\n");
		int maxLength = linesRight.length;
		int addedChars = linesLeft[0].length();
		String fill="";
		if (linesLeft.length>maxLength){
			maxLength = linesLeft.length;
			addedChars = linesRight[0].length();
			}
		fill=String.join("", Collections.nCopies(addedChars, String.valueOf(" ")));
		
		
		for (int a = maxLength-1;a>=0 ;a--) {
			if (a<linesLeft.length&&a<linesRight.length) {
				merged = linesLeft[a] + linesRight[a] + System.lineSeparator();
			}
			else if (a>=linesLeft.length)
				merged += fill + linesRight[a] + System.lineSeparator();
			else if (a>=linesRight.length)
				merged += linesLeft[a] + fill +  System.lineSeparator();
		}
		return merged;
	}
	/* Taken from https://www.demo2s.com/java/java-string-finds-the-first-non-whitespace-character-starting-at-index.html */
	public static int findNonwhitespaceCharacter(String s, int index) {
        int sLength = s.length();
        while (index < sLength && Character.isWhitespace(s.charAt(index))) {
            index++;
        }

        return index;
    }
	
	public static int findNonwhitespaceCharacterRev(String s) {
        int ind= s.length()-1;
        while (ind>= 0 && Character.isWhitespace(s.charAt(ind))) {
            ind--;
        }

        return ind;
    }
	private static String coverageLine(String s) {
		int st = findNonwhitespaceCharacter(s,0);
		int en = findNonwhitespaceCharacterRev(s);
		String fill=String.join("", Collections.nCopies(st, String.valueOf(" ")));
		fill = fill + String.join("", Collections.nCopies(en-st, String.valueOf("-")));
		return fill;
	}
	public String prettyPrint() {
		String right="";
		String left = "";
		if (this.subproof2!=null)
			right = this.subproof2.prettyPrint();
		if (this.subproof1!=null)
			left = this.subproof1.prettyPrint();
		
		if (right=="" && left == "")
			if (this.myId==-1)
				return "  " + this.semanticSide + " : " + this.linearLogicSide + "   ";
			else
				return "  [" + this.semanticSide + " : " + this.linearLogicSide + "]_"+Integer.toString(this.myId) + "   ";
		
		String top = mergeLeftRight(left,right);
		String bottom = this.semanticSide + " : " + this.linearLogicSide;
		String middle = coverageLine(top);
		
		return top;
	}
	private static boolean convert(NaturalDeductionProof node, NaturalDeductionProof parent, int which) {
		if (node==null)
			return false;
		Iterator<Integer> it = node.assumptions.iterator();
		if (it.hasNext()) {
			int thisId = it.next();
			
			NaturalDeductionProof item = find(node, thisId);
			if (item != null) {
				NaturalDeductionProof myNewProof = new NaturalDeductionProof();
				myNewProof.assumptions = (LinkedList<Integer>) node.assumptions.clone();
				myNewProof.subproof1 = node;
				myNewProof.subproof2 = null;
				myNewProof.myId = -1;
				myNewProof.semanticSide = "\\" + item.semanticSide + ".(" + node.semanticSide + ")";
				myNewProof.linearLogicSide = item.linearLogicSide + " -o " + node.linearLogicSide;
				myNewProof.type = "I_" + Integer.toString(thisId);
				removeAssumption(myNewProof, thisId);
				if(which==0) 
					parent.subproof1=myNewProof;
				else
					parent.subproof2=myNewProof;
				node = myNewProof;
				return true;
				}
			}
		if(convert(node.subproof1,node,0))
			return true;
		if(convert(node.subproof2,node,1))
			return true;
		return false;
	}
	public static NaturalDeductionProof createTree(Premise p) {
		if (p==null)return null;
		NaturalDeductionProof myProof = new NaturalDeductionProof();
		myProof.semanticSide = p.getSemTerm().toString();
		myProof.linearLogicSide = p.getGlueTerm().getReverseCompiledString();
		myProof.type="E";


		myProof.subproof1=createTree(p.comb_a);
		myProof.subproof2=createTree(p.comb_b);
		List<Premise> list = p.getGlueTerm().assumptions2;
		Iterator<Premise> it = list.iterator();
		myProof.assumptions = new java.util.LinkedList<Integer>();
		myProof.myId = -1;
		while (it.hasNext()) {
			Premise pr = it.next();
			if(pr!=null)
				{
				HashSet<Integer> usedUpPremises = pr.getPremiseIDs();
				
		        Iterator<Integer> it1 = usedUpPremises.iterator();
		        while (it1.hasNext()) {
		        	myProof.assumptions.add(it1.next());
		        	}
				}
			}
		
		if (myProof.assumptions.size() ==1 && myProof.subproof1 == null && myProof.subproof2==null){
			HashSet<Integer> thisPremiseIds=  p.getPremiseIDs();
			if (thisPremiseIds.size()==1) {
				int proofInt = myProof.assumptions.get(0);
				Iterator<Integer> it1 = thisPremiseIds.iterator();
				int myInt = it1.next();
				
				if (proofInt == myInt)
					myProof.myId = myInt;
			}			
		}
		return myProof;
	}
	public String semanticSide;
	public String linearLogicSide;
	public String type;
	public NaturalDeductionProof subproof1;
	public NaturalDeductionProof subproof2;
	public java.util.LinkedList<Integer> assumptions;
	public int myId;
}
