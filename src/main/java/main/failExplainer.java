package main;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import glueSemantics.linearLogic.Premise;

public class failExplainer {

	private static class structToKeepIntValue {
		public int index;
		public int value;
		structToKeepIntValue(int i, int j) {this.index = i; this.value = j;};
	}
	private static class sortDescendingByValue implements Comparator<structToKeepIntValue>{
	    public int compare(structToKeepIntValue a, structToKeepIntValue b)
	    {
	        return b.value - a.value;
	    }
	}
	private static String lastExplanation="";
	public static String getLastExplanation() {return lastExplanation;};
	private static boolean intersects(boolean []indices, int[] nums) {
		/*
		 * This function checks if coverage nums intersects with the indices 
		 * covered by previous premises.
		 * 
		 * Returns true if intersects, false otherwise.
		 * 
		 * Implements brute-force check.
		 * 
		 */
            
        for (int i = 0; i<indices.length; i++) {
        	if (!indices[i])
        		continue;
           	for (int j = 0; j<nums.length; j++) {
           		if (i==nums[j])
           			return true;
           	}
        }
		return false;
	}
	private static List<Integer> tryToAddNew(List nums, List explanation, boolean []used, boolean[]indices, structToKeepIntValue[] sizes, int left ){
		/*
		 * This function does a recursive depth-first search by
		 *  adding premises to the explanation starting from premises that span the
		 * largest number of indices.
		 * */
		
		/* If there is nothing to add then it means that an explanation has been found */
		if(left == 0)
			return explanation;
		
		for (int used_indice = 0; used_indice < used.length; used_indice ++) {
			
			/* if this indice is already used then continue with the next one */
			if(used[used_indice])
				continue;
			
			/* if the size is greater than the left amount of indices then continue with the next one.
			 * This is especially useful to cut the space complexity of premises with large indices
			 * that won't have any effect. */
			if(sizes[used_indice].value > left)
				continue;
			
			/* Check if this premise and the premises used up to now have conflicting elements.
			 * If there is conflict, then this premise could not be added, so continue
			 * with the next one. */ 
			if(intersects(indices, (int[])nums.get(sizes[used_indice].index)))
				continue;
			
			/* All the controls have been passed. This means, this premise could be added 
			 * to the explanation. So, the following will copy the memory to local and call
			 * the function recursively. */
			
			/* copy state to local memory */
			boolean[] my_used = new boolean[used.length];
			for(int i =0 ; i < used.length; i++) {
				my_used[i] = used[i];
			}
			boolean[] my_indices = new boolean[indices.length];
			for(int i =0 ; i < indices.length; i++) {
				my_indices[i] = indices[i];
			}
			List<Integer> my_explanation = new ArrayList<Integer>();
			Iterator it = explanation.iterator();
			while(it.hasNext()) {
				my_explanation.add((Integer)it.next());
			}
			int my_left = left;
			
			
			/* Make state modifications to add a new premise to the explanation*/
			my_left -= sizes[used_indice].value;
			my_explanation.add(sizes[used_indice].index);
			my_used[used_indice] = true;
			int[]premise_indexes = (int[])nums.get(sizes[used_indice].index);
			for (int pre = 0 ; pre < premise_indexes.length; pre ++) {
				my_indices[premise_indexes[pre]]=true;
			}
			
			/* Recursive call with the new state*/
			List<Integer> res = tryToAddNew(nums, my_explanation, my_used, my_indices, sizes, my_left );
			if(res != null)
				return res;
		}
		return null;
	}
	
	private static List<Integer> findAnExplanation(List nums){
		
		List<Integer> explanation = new ArrayList<Integer>();

		/* First locate the maximum number */
    	/* Compute the maximum number of coverage with this integer variable */ 
		int max = -1;

		/* Additionaly, put sizes into an array with the corresponding indexes */ 
		structToKeepIntValue[] sizes = new structToKeepIntValue[nums.size()];

		Iterator it = nums.iterator();
		int i = 0;
		while(it.hasNext()) {
			int []numsInThis = (int[])it.next();
			sizes[i] = new structToKeepIntValue(i,numsInThis.length);
			i++;
			for (int j = 0; j< numsInThis.length;j++)
				if(numsInThis[j] > max)
					max = numsInThis[j];
			}
		max ++;
		Arrays.sort(sizes, new sortDescendingByValue());
		
		/* create a boolean array to keep track of consumed premises */
		boolean used[] = new boolean[nums.size()];
		/* initialize all to false as nothing has used up yet */
		for (i=0; i< used.length; i++) {
			used[i] = false;
		}
		
		/* create a boolean array to keep track of if indices are inluded*/
		boolean indices[] = new boolean[max];
		/* initialize all to false as no indice has been included in explanation yet*/
		for (i=0; i< used.length; i++) {
			used[i] = false;
		}
		List<Integer> res = tryToAddNew(nums, explanation, used, indices, sizes, max);
		if(res == null)
			return explanation;
		return res;
	}

    public static String explain( HashMap nonAtomicChart, HashMap atomicChart) {
    	List<String> str = new LinkedList<String>();
    	List<int []> nums = new ArrayList<int[]>();
    	
		
		/* For all non-atomic chart elements */ 
    	for (Object key : nonAtomicChart.keySet()) {
    		
    		String keyName = key.toString();
    		List list = (List)nonAtomicChart.get(key);
    		for (Object temp : list) {
    			
    			/*
    			 * numbersOfThisObjbect holds the numbers of this nonAtomicChart
    			 * toAdd is used to construct a string to add to numbersOfThisObject 
    			 * */
    			ArrayList<Integer> numbersOfThisObject = new ArrayList<Integer>();
        		String toAdd = "";
        		
        		int[] nmbrs = ((Premise)temp).getNumbersOfThisObject(); 
    			String objStr = ((Premise)temp).getReverseCompiledString() + " : " + ((Premise)temp).getSemTerm() + "  " + ((Premise)temp).getReverseCompiledPremiseIds(nmbrs) ;
    	    			
        		str.add(objStr);

                /* Add numbers of this object to int[] list. */
                nums.add(nmbrs);
    		}
    	}

		/* For all atomic chart elements */ 
    	for (Object key : atomicChart.keySet()) {
    		
    		String keyName = key.toString();
    		List list = (List)atomicChart.get(key);
    		for (Object temp : list) {
    			
    			/*
    			 * numbersOfThisObjbect holds the numbers of this nonAtomicChart
    			 * toAdd is used to construct a string to add to numbersOfThisObject 
    			 * */
    			ArrayList<Integer> numbersOfThisObject = new ArrayList<Integer>();
        		String toAdd = "";

        		int[] nmbrs = ((Premise)temp).getNumbersOfThisObject();
    			String objStr =  ((Premise)temp).getReverseCompiledString() + " : " + ((Premise)temp).getSemTerm() + "  " + ((Premise)temp).getReverseCompiledPremiseIds(nmbrs) ;
    	    			

        		str.add(objStr);
    		
                /* Add numbers of this object to int[] list. */
                nums.add(nmbrs);
    		}

    	}
    	StringBuilder explanationString = new StringBuilder();
    	
    	/* Get a list of premises that are scored the topmost. 
    	 * build a string by concatenating their representative strings.
    	 * 
    	 * Return the string that is built.
    	 * */
		List<Integer> failExplainingList = failExplainer.findAnExplanation(nums);
		lastExplanation = "";
		Iterator<Integer> it = failExplainingList.iterator();
		while (it.hasNext()) {
			Integer arr = it.next();
			String exp = InputOutputProcessor.restoreBackLinearLogicSide(str.get(arr)) + System.lineSeparator();
			explanationString.append(exp);
			lastExplanation += "% " + exp;
		}
		return explanationString.toString();
    }
}
