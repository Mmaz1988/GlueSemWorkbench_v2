package main;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Stack;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.Collections;

public class InputOutputProcessor {
	private static String names[];
	private static String values[];
	private static int size;
	private static int reserved;

	private static final String regex1 = "[a-zA-Z]+\\([0-9]+\\)";
	private static final String regex2 = "[a-zA-Z]+\\(\'[0-9]+\'\\)";
	private static final String regex3 = "\\([K][K]*-o[K][K]*\\)";
	private static final String regex4 = "([K]+-o[K]+)";
	private static final String regex5 = "[1-9][0-9]*";
	
	
	private static void add(String s1) {
		String original = new String(s1);
		s1 = s1.replace("'", "");
		String myNumber=Integer.toString(InputOutputProcessor.size + 1);
		String val = myNumber + "_" +s1.substring(0,s1.indexOf('('));
				
		for (int i = 0; i < size; i++) {
			if (names[i].equals(s1)) {
				return;
			}
		}
		if (size == reserved - 1) {
			int newSize = reserved *= 2;
			String namesNew[] = new String[newSize];
			String valuesNew[] = new String[newSize];
			for (int i = 0; i < size; i++) {
				namesNew[i] = names[i];
				valuesNew[i] = values[i];
			}
			InputOutputProcessor.names=namesNew;
			InputOutputProcessor.values=valuesNew;
			reserved = newSize;
		}
		InputOutputProcessor.names[size] = original;
		InputOutputProcessor.values[size] = val;
		InputOutputProcessor.size++;
	}

	public static void resetInputProcessor() {
		InputOutputProcessor.reset();
	}

	public static void process(String s) {
		InputOutputProcessor.reset();
		Pattern pattern = Pattern.compile(regex1, Pattern.CASE_INSENSITIVE);
		Matcher matcher = pattern.matcher(s);
		while (matcher.find()) {
			add(matcher.group());
		}
		pattern = Pattern.compile(regex2, Pattern.CASE_INSENSITIVE);
		matcher = pattern.matcher(s);
		while (matcher.find()) {
			add(matcher.group());
		}
	}

	public static void process(List<String> lines) {
		String s = "";
		Iterator<String> it = lines.iterator();
		while (it.hasNext()) {
			s += it.next() + "\n";
		}
		process(s);
	}

	public static void reset() {
		names = new String[20];
		values = new String[20];
		reserved = 20;
		size = 0;
	}

	public static String translate(String s) {
		boolean update = false;
		
		String lines[] = s.split("\\r?\\n|\\r");
		for (int j = 0; j<lines.length; j++) {
			for (int i = 0; i < size; i++) {
				String S = lines[j].replace(InputOutputProcessor.names[i], InputOutputProcessor.values[i]);
				if(!S.equals(lines[j]))
					{
					update = true;
					lines[j] = S;
					}
			}
		}
		if (update) {
			String toReturn = "";
			for (int j = 0; j<lines.length; j++) {
				String semSide =lines[j].substring(0,lines[j].indexOf(':'));
				String linSide = lines[j].substring(lines[j].indexOf(':')+ 1);
				linSide = processPharanthesing(linSide);
				toReturn +=  semSide + " : " + linSide + "\n";
			}
			return toReturn;
		}
		return s;
	}

	public static List<String> translate(List<String> l) {
		List<String> newList = new ArrayList<String>();
		Iterator<String> it = l.iterator();
		while (it.hasNext()) {
			newList.add(translate(it.next()));
		}
		return newList;
	}

	private static boolean checkIfInBrackets(String s, int start, int end) {
		Stack<Character> stack1 = new Stack<Character>();
		Stack<Character> stack2 = new Stack<Character>();

		for (int i = 0; i < s.length(); i++) {
			if (i >= start && i < end) {
				if (!stack1.empty())
					return true;
				if (!stack2.empty())
					return true;
			}
			String ch = s.substring(i, i + 1);

			if (ch.equals("["))
				stack1.push('[');
			else if (ch.equals("]"))
				stack1.pop();
			else if (ch.equals("{"))
				stack2.push('{');
			else if (ch.equals("}"))
				stack2.pop();
		}

		return false;
	}

	public static String translateBack(String s) {
		if (size == 0)
			return s;
		String toReturn = "";
		s = "  " + s + "  ";
		Pattern pattern = Pattern.compile(regex5, Pattern.CASE_INSENSITIVE);
		Matcher matcher = pattern.matcher(s);
		int prev = 0;
		while (matcher.find()) {
			String matched = matcher.group();
			try {
				if (checkIfInBrackets(s, matcher.start(), matcher.end())) {
					toReturn += s.substring(prev, matcher.start()) + matched;
					prev = matcher.end();
				} else {
					toReturn += s.substring(prev, matcher.start())
							+ InputOutputProcessor.names[Integer.parseInt(matched) - 1];
					prev = matcher.end();
				}
			} catch (Exception e) {
				toReturn += s.substring(prev, matcher.start()) + matched;
				prev = matcher.end();
			}
		}

		toReturn += s.substring(prev, s.length());
		toReturn = toReturn.substring(2);
		toReturn = toReturn.substring(0, toReturn.length() - 2);
		return toReturn;
	}

	public static String restoreBackLinearLogicSide(String s) {
		int splitIndex = s.indexOf(':');
		String firstSide = s.substring(0, splitIndex);
		String secondSide = s.substring(splitIndex);
		firstSide = InputOutputProcessor.translateBack(firstSide);
		return firstSide + secondSide;
	}

	public static String processPharanthesing(String s) {
    	/*
    	 * This function processes the input string, and modifies it in a way that
    	 * the linear logic parser does not fail due to absence of the right
    	 * associative parenthesis. It does this by adding missing parentheses.
    	 * */
    	
    	String r = " " + s.replaceAll("\\s+","") + " ";
		
		String repString = " ";
		for (int i = 1 ; i< r.length() -1; i++) {
			char c = r.charAt(i); 
			if ( c == '(' || c == '-' || c == 'o' || c == ')') 
				repString = repString + c;
			else
				repString = repString + "K";
		}
		repString += " ";
		
		Pattern pattern1 = Pattern.compile(regex3, Pattern.CASE_INSENSITIVE);
		Pattern pattern2 = Pattern.compile(regex4, Pattern.CASE_INSENSITIVE);
		boolean found = true;
		while(found) {
			found = false;
			Matcher matcher;
			String matched = "";
			int start = 0;
			int end = 0;
			boolean found1=true;
			while (found1) {
				found1=false;
				matcher = pattern1.matcher(repString);
				while (matcher.find()) {
					matched = matcher.group();
					start = matcher.start();
					end = matcher.end();
					if (repString.charAt(start - 1) != '(' || repString.charAt(end) != ')') {
						String joinText = String.join("", Collections.nCopies(matched.length(), "K"));
						repString = repString.substring(0, start) + joinText + repString.substring(end, repString.length());
						found1=true;
					}
				}

			
			matcher = pattern2.matcher(repString);
			matched = "";
			start = 0;
			end = 0;
			int lastStart = -1;
			int lastEnd = -1;
			int findIndex = 0;
			while (matcher.find(findIndex)) {
				matched = matcher.group();
				start = matcher.start();
				end = matcher.end();
					if (repString.charAt(start - 1) != '(' || repString.charAt(end) != ')') {
					    lastStart = start;
					    lastEnd = end;	    
					}
				findIndex = start + matched.indexOf('o');
				}
			
			if(lastStart > -1 && lastEnd > -1) {
				
				String joinText = String.join("", Collections.nCopies(matched.length() + 2, "K"));
				repString = repString.substring(0,lastStart) + joinText + repString.substring(lastEnd, repString.length());					
				r = r.substring(0, lastStart) + "(" + r.substring(lastStart,lastEnd)+ ")" + r.substring(lastEnd, r.length());					
				
				found = true;
				}
			
			
			}
		}
	return r.trim().replace("-o", " -o ");
	}
}
