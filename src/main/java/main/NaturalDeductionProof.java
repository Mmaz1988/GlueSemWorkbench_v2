package main;

import glueSemantics.linearLogic.Premise;
import glueSemantics.semantics.lambda.SemFunction;
import glueSemantics.semantics.lambda.SemanticExpression;

import java.util.*;


public class NaturalDeductionProof {

	public static class PrintStructure {
		public String[] lines;
		public PrintStructure() {}
		public PrintStructure(String s1) {
			lines = new String[1];
			lines[0] = s1;
		}
		public int getHeight() {return lines.length;};
		public int getWidth() {if (lines.length > 0) return lines[0].length(); else return 0;};
		public String getLastLine() {if (lines.length>0) return lines[lines.length-1]; else return ""; };
		public String getFirstLine() {if (lines.length>0) return lines[0]; else return "";};

		/* Taken from https://www.demo2s.com/java/java-string-finds-the-first-non-whitespace-character-starting-at-index.html */
		private int findNonwhitespaceCharacter(String s, int index) {
	        int sLength = s.length();
	        while (index < sLength && Character.isWhitespace(s.charAt(index))) {
	            index++;
	        }

	        return index;
	    }
		private int findNonwhitespaceCharacterRev(String s) {
	        int ind= s.length()-1;
	        while (ind>= 0 && Character.isWhitespace(s.charAt(ind))) {
	            ind--;
	        }

	        return ind;
	    }
		
		private String createMiddleLine(String []top, String[] bottom, String type) {
			String up=top[top.length-1];
			String down=bottom[0];
			int upSt = findNonwhitespaceCharacter(up,0);
			int upEn = findNonwhitespaceCharacterRev(up);
			
			int downSt = findNonwhitespaceCharacter(down,0);
			int downEn = findNonwhitespaceCharacterRev(down);
			int st=(upSt<downSt)?upSt:downSt;
			int en=(upEn>downEn)?upEn:downEn;
			
			String fill=String.join("", Collections.nCopies(st, String.valueOf(" ")));
			fill = fill + String.join("", Collections.nCopies(en-st+2, String.valueOf("─")));
			fill += type;
			int diff = fill.length() - top[0].length();
			if (diff>0) {
				String rightPad=String.join("", Collections.nCopies(diff, String.valueOf(" ")));
				for (int a=0;a<top.length;a++) {
					top[a] =  top[a] + rightPad;
				}
				for (int a=0;a<bottom.length;a++) {
					bottom[a] =  bottom[a] + rightPad;
				}
			}
			else
				fill += String.join("", Collections.nCopies(-diff, String.valueOf(" ")));
			return fill;
		}

		private void makeWidthsEqual(String[]s1, String[]s2) {
			if (s1[0].length()==s2[0].length()) return;
			String []s;
			int diff;
			int s1length = s1[0].length();
			int s2length = s2[0].length();
			if(s1length>s2length)
				{
				diff=s1length-s2length;
				s=s2;
				}
			else {
				diff = s2length-s1length;
				s=s1;
			}
			
			String leftPad=String.join("", Collections.nCopies(diff/2, String.valueOf(" ")));
			String rightPad=String.join("", Collections.nCopies(diff/2, String.valueOf(" ")));
			if (diff%2==1) rightPad+=" ";
			for (int a=0;a<s.length;a++) {
				s[a] = leftPad + s[a] + rightPad;
			}
		};
		public PrintStructure(PrintStructure left, PrintStructure right,PrintStructure  bottom, String type) {
			int leftHeight=0;
			int leftWidth=0;
			int rightHeight=0;
			int rightWidth=0;
			String[] leftLines;
			String[] rightLines;
			if (left!=null) {
				leftHeight=left.getHeight();
				leftWidth=left.getWidth();
				leftLines=left.lines;
			}
			else
				{
				leftLines=new String[1];
				leftLines[0]=new String("");
				leftHeight=1;
				leftWidth=0;
				}
			if (right!=null) {

				rightHeight=right.getHeight();
				rightWidth=right.getWidth();
				rightLines=right.lines;
				}
			else
				{
				rightLines=new String[1];
				rightLines[0]=new String("");
				rightHeight=1;
				rightWidth=0;
				}
			int maxHeight = rightHeight;
			int addedChars = leftWidth;
			String fill="";
			if (leftHeight>maxHeight){
				maxHeight = leftHeight;
				addedChars = rightWidth;
				}
			fill=String.join("", Collections.nCopies(addedChars, String.valueOf(" ")));
			
			lines = new String[maxHeight];
			
			int leftPointer=leftLines.length-1;
			int rightPointer=rightLines.length-1;;
			for (int a = maxHeight-1;a>=0 ;a--) {
				lines[a] = new String("");
				if (leftPointer>=0&&rightPointer>=0) {
					lines[a] = leftLines[leftPointer] + rightLines[rightPointer];
					leftPointer--;
					rightPointer--;
				}
				else if (rightPointer>=0)
				{
					lines[a] = fill + rightLines[rightPointer];
					rightPointer--;
				}
				else if (leftPointer>=0)
				{
					lines[a] =  leftLines[leftPointer] + fill;
					leftPointer--;
				}
			}
			
			makeWidthsEqual(lines,bottom.lines);
			String middleLine = createMiddleLine(lines, bottom.lines, type);
			
			String [] lastLines = new String[lines.length + bottom.lines.length + 1];
			int a=0;
			for (int b = 0;b<lines.length;b++) {
				lastLines[a]=lines[b];
				a++;
			}
			lastLines[a]=middleLine;
			a++;
			for (int b = 0;b<bottom.lines.length;b++) {
				lastLines[a]=bottom.lines[b];
				a++;
			}

			lines=lastLines;
		}
		public String toString() {
			return String.join(System.lineSeparator(), this.lines);
		}		
	};
	private static void removeAssumption(NaturalDeductionProof node, int idd) {
		if (node ==null) return;


		ListIterator<Integer> it =node.assumptions.listIterator();
		int num = 0;
		while(it.hasNext()) {
			int n = it.next();
			if (n == idd) {
				node.assumptions.remove(num);
				it = node.assumptions.listIterator();
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
	public static String getNaturalDeductionProof(Premise p, Integer setting) {

		NaturalDeductionProof algorithmStyle = createTree( p, setting );
		
		NaturalDeductionProof natural = new NaturalDeductionProof();
		
		natural.subproof1 = algorithmStyle;
		
 		while(NaturalDeductionProof.convert(algorithmStyle,natural,0, setting)) {};
		return  natural.subproof1.prettyPrint(setting);
		
	}
	public PrintStructure getPrintStructure(Integer setting) {
		String semanticRep = "";
		if (this.subproof1==null&&this.subproof2==null) {
			if(this.myId!=-1){
				semanticRep = "  [" + this.semanticSide + " : " + this.linearLogicSide + "]"+makeSubscript(Integer.toString(this.myId)) + "   ";
				if (setting == 2)
				{
					semanticRep = "  [" + this.linearLogicSide + "]"+makeSubscript(Integer.toString(this.myId)) + "   ";
				}
			}
			else {
				semanticRep = "  " + this.semanticSide + " : " + this.linearLogicSide + "   ";
				if (setting == 2) {
					semanticRep = "  " + this.linearLogicSide + "   ";
				}
			}
		return new PrintStructure(semanticRep);
		}

		String semanticRep1 = "";
		semanticRep1 = "  " + this.semanticSide + " : " + this.linearLogicSide + "   ";

		if (setting == 2)
		{
			semanticRep1 = "  " + this.linearLogicSide + "   ";
		}

		PrintStructure bottom = new PrintStructure(semanticRep1);
		
		
		
		PrintStructure right = null;
		PrintStructure left = null;
		if (this.subproof2!=null)
			right = this.subproof2.getPrintStructure(setting);
		if (this.subproof1!=null)
			left = this.subproof1.getPrintStructure(setting);
		return new PrintStructure(left,right,bottom, this.type);
	}
	public String prettyPrint(Integer setting) {
		
		return this.getPrintStructure(setting).toString();
	}
	private static boolean convert(NaturalDeductionProof node, NaturalDeductionProof parent, int which, Integer setting) {
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

				if (setting == 2)
				{
					myNewProof.semanticSide = "";
				} else
				{
					myNewProof.semanticSide = "\\" + item.semanticSide + ".(" + node.semanticSide + ")";
				}
				//myNewProof.semanticSide = "";
				myNewProof.linearLogicSide = item.linearLogicSide + " -o " + node.linearLogicSide;
				myNewProof.linearLogicSide = myNewProof.linearLogicSide;
				myNewProof.type = "I" + makeSubscript(Integer.toString(thisId));
				removeAssumption(myNewProof, thisId);
				if(which==0) 
					parent.subproof1=myNewProof;
				else
					parent.subproof2=myNewProof;
				node = myNewProof;
				return true;
				}
			}
		if(convert(node.subproof1,node,0, setting))
			return true;
		if(convert(node.subproof2,node,1, setting))
			return true;
		return false;
	}
	public static NaturalDeductionProof createTree(Premise p, Integer setting) {
		if (p==null)return null;
		NaturalDeductionProof myProof = new NaturalDeductionProof();
		//myProof.semanticSide = p.getSemTerm().toString();

		if (setting == 0)
		{
			myProof.semanticSide = p.getSemTerm().toString();
		} else if (setting == 1) {
			myProof.semanticSide = NaturalDeductionProof.reduceSemanticSide(p);
		} else if (setting == 2) {
			myProof.semanticSide = "";
		}

		myProof.linearLogicSide = InputOutputProcessor.translateBack(p.getGlueTerm().getReverseCompiledString()).replace("⊸", "-o");;
		myProof.type="E";


		myProof.subproof1=createTree(p.comb_a, setting);
		myProof.subproof2=createTree(p.comb_b, setting);
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
	public static String makeSubscript(String s) {
		String[] nums= {"₀","₁","₂","₃","₄","₅","₆","₇","₈","₉"};
		String toReturn="";
		for (int a=0;a<s.length();a++) {
			String ch = s.substring(a,a+1);
			try {
				toReturn+=nums[Integer.parseInt(ch)];
			}
			catch (Exception E) {
				toReturn+=ch;
			}
		}
		return toReturn;
		
	}
	public String semanticSide;

	public static String reduceSemanticSide(Premise p)
	{
		String reducedString = p.getSemTerm().toString();

		if (p.getSemTerm() instanceof SemanticExpression) {
			SemanticExpression short_p = (SemanticExpression) p.getSemTerm();

			while (short_p instanceof SemFunction) {
				short_p = (SemanticExpression) ((SemFunction) short_p).getFuncBody();
			}
			reducedString = short_p.toString();
		}

	//if reduced String is longer than 10 characters, then replace all characters beyond the 10th with ...
		if (reducedString.length() > 10) {
			reducedString = reducedString.substring(0, 10) + "...";
		}
		return reducedString;
	}
	public String linearLogicSide;
	public String type;
	public NaturalDeductionProof subproof1;
	public NaturalDeductionProof subproof2;
	public java.util.LinkedList<Integer> assumptions;
	public int myId;

}
