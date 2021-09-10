package main;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Stack;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import main.InputOutputProcessor;

public class InputOutputProcessor {
	private static String names[];
	private static String values[];
	private static int size;
	private static int reserved;
	private static int lastInteger;
	
	private static final String regex1 = "[a-zA-Z]+\\([0-9]+\\)";
	private static final String regex2 = "[a-zA-Z]+\\(\'[0-9]+\'\\)";
	
	private static void add(String s1) {
		for(int i = 0; i < size; i ++) {
			if (names[i].equals(s1)){
				return;
			}
		}
		if (size == reserved -1) {
			int newSize = reserved *= 2;
			String namesNew[] = new String[newSize];
			String valuesNew[] = new String [newSize];
			for(int i = 0; i<size;i++) {
				namesNew[i]=names[i];
				valuesNew[i]=values[i];
			}
			reserved = newSize;
		}
		InputOutputProcessor.names[size] = s1;
		InputOutputProcessor.values[size] = Integer.toString(lastInteger);
		InputOutputProcessor.size ++;
		InputOutputProcessor.lastInteger ++;
	}
	
	public static void resetInputProcessor() {
		InputOutputProcessor.reset();
	}
	public static void process(String s) {
		InputOutputProcessor.reset ();
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
	public static void process( List<String> lines) {
		String s = "";
		Iterator<String> it = lines.iterator();
		while(it.hasNext()) {
			s +=  it.next() + "\n";
		}
		process(s);
	}
	public static void reset() {
		names = new String[20];
		values = new String[20];
		reserved = 20;
		size = 0;
		lastInteger = 1;
	}
	public static String translate(String s) {
		for(int i = 0; i < size; i ++) {
				s = s.replace(InputOutputProcessor.names[i], InputOutputProcessor.values[i] );
			}
		return s;
	}
	public static List<String> translate(List<String> l) {
		List <String> newList = new ArrayList<String>();
		Iterator<String> it = l.iterator();
		while(it.hasNext()) {
			newList.add(translate(it.next()));
		}
		return newList;
	}
	
	private static boolean checkIfInBrackets(String s, int start, int end) {
		Stack<Character> stack1 = new Stack<Character>();
		Stack<Character> stack2 = new Stack<Character>();
		
		for(int i = 0; i < s.length(); i ++) {
			if (i>=start && i<end) {
				if(!stack1.empty())
					return true;
				if(!stack2.empty())
					return true;
			}
			String ch = s.substring(i, i +1);
			
			if(ch.equals("["))
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
		if (size==0)
			return s;
		String toReturn="";
		s = "  " + s + "  ";
		Pattern pattern = Pattern.compile("[1-9][0-9]*", Pattern.CASE_INSENSITIVE);
		Matcher matcher = pattern.matcher(s);
		int prev = 0;
        while (matcher.find()) {
        	String matched = matcher.group();
        	try {
        		if (checkIfInBrackets(s,matcher.start(),matcher.end())) {
        			toReturn += s.substring(prev, matcher.start() ) + matched;
        			prev = matcher.end();        		
        			}
        		else{
        			toReturn += s.substring(prev, matcher.start() ) + InputOutputProcessor.names[Integer.parseInt(matched) -1 ];
        			prev = matcher.end();
        		}
        	}
        	catch(Exception e){
    		toReturn += s.substring(prev, matcher.start() ) + matched;
    		prev = matcher.end();
        	}
        }
        
		toReturn += s.substring(prev,s.length());
		toReturn = toReturn.substring(2);
		toReturn = toReturn.substring(0, toReturn.length() - 2);
		return toReturn;
	}
	
    public static String restoreBackLinearLogicSide(String s) {
    	int splitIndex = s.indexOf(':');
    	String firstSide = s.substring(0,splitIndex);
    	String secondSide = s.substring(splitIndex);
    	firstSide =  InputOutputProcessor.translateBack(firstSide);
    	return firstSide + secondSide;
    }

}
