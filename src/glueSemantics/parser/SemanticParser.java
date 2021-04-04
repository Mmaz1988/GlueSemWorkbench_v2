package glueSemantics.parser;

import glueSemantics.semantics.SemanticRepresentation;
import glueSemantics.semantics.lambda.*;
import glueSemantics.semantics.lambda.SemType.AtomicType;
import main.Settings;
import prover.LLProver1;
import utilities.LexVariableHandler;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

public class SemanticParser extends StringParser {

    private int bracketCounter = 0;
    private int parenthesisCounter = 0;
    private int pos = 0;
    private HashMap<Integer, List<SemAtom>> variableBindings = new HashMap<>();
    private List<Integer> specialChars = Arrays.asList(46, 47, 91, 93, 40, 41, 123, 125, 44, 95, 60, 62, 32, 38);

    public SemanticParser() {
    }

    @Test
    void testParseExpression() {
        //  SemanticRepresentation s = parseExpression( "[/P_<e,t>.[/x_e.[/y_e.P(x)(y)]]]");
        //   SemanticRepresentation p = parseExpression( "[/x_e.[/y_e.sleep(a(x,y),b(y,x))]]");
        // SemanticRepresentation p = parseExpression( "[/x_e.[/y_e.sleep(a(x),b(y))]]");
        //SemanticRepresentation p = parseExpression( "[/P_e.[/x_e.[/y_e.sleep(c(a(x),b(y),P(x),a(x,y)))]]]");
        // SemanticRepresentation p = parseExpression( "[/P_e.[/Q_e.[/y_e.[P(y) & Q(y)]]]]");
       // SemanticRepresentation p = parseExpression( "[/P_<e,t>.[/Q_<e,t>.Ex_v[P(x) -> Q(x))]]]");
        // SemanticRepresentation p = parseExpression("[/R_<v,t>.[/x_e.[/y_e.Ee_v[R(e) & (agent(e,x) v theme(e,y))]]]]");
        //   SemanticRepresentation p = parseExpression("[/M_<s,<s,t>>.[/P_<s,t>.[/s_s.Az_s[M(s)(z) -> P(z)]]]]");
        //SemanticRepresentation p = parseExpression("[/P_t.[/Q_t.[P&Q]]]");
        //     SemanticRepresentation p = parseExpression("{[/x_e.sleep(x)],[/y_e.snore(y)]}");
        // SemanticRepresentation p = parseExpression("[/x_e.sleep(x,y,z)]");
        //   SemanticRepresentation p = parseExpression("[/T_<s,<s,t>>.[/P_<s,t>.[/t_s.Et1_s[T(t)(t1) & P(t1)]]]]");
        // SemanticRepresentation p = parseExpression("[/P_<v,t>.[/s_s.exemplify(s,Ee_v[P(e)])]]");
        //  SemanticRepresentation p = parseExpression("[/x_e.[/y_e.lubić(x,y)]]");
        // SemanticRepresentation p = parseExpression( "[/P_<e,t>.[/Q_<e,t>.Ex_v[P(x) -> Q(x))]]]");


        List<String> testFormulas = new ArrayList<>();
        testFormulas.add("[/P_<e,t>.[/x_e.[/y_e.P(x)(y)]]]");
        testFormulas.add("[/x_e.[/y_e.sleep(a(x,y),b(y,x))]]");
        testFormulas.add("[/x_e.[/y_e.sleep(a(x),b(y))]]");
        testFormulas.add("[/P_e.[/x_e.[/y_e.sleep(c(a(x),b(y),P(x),a(x,y)))]]]");
        testFormulas.add("[/P_e.[/Q_e.[/y_e.[P(y) & Q(y)]]]]");
        testFormulas.add("[/P_<e,t>.[/Q_<e,t>.Ex_v[P(x) -> Q(x))]]]");
        testFormulas.add("[/R_<v,t>.[/x_e.[/y_e.Ee_v[R(e) & (agent(e,x) v theme(e,y))]]]]");
        testFormulas.add("[/R_<v,t>.[/x_e.[/y_e.Ee_v[R(e) & agent(e,x) & theme(e,y)]]]]");
        testFormulas.add("[/M_<s,<s,t>>.[/P_<s,t>.[/s_s.Az_s[M(s)(z) -> P(z)]]]]");
        testFormulas.add("[/P_t.[/Q_t.[P&Q]]]");
        testFormulas.add("{[/x_e.sleep(x)],[/y_e.snore(y)]}");
        testFormulas.add("[/x_e.sleep(x,y,z)]");
        testFormulas.add("[/T_<s,<s,t>>.[/P_<s,t>.[/t_s.Et1_s[T(t)(t1) & P(t1)]]]]");
        testFormulas.add("[/P_<v,t>.[/s_s.exemplify(s,Ee_v[P(e)])]]");
        testFormulas.add("[/x_e.[/y_e.lubić(x,y)]]");
        testFormulas.add("[/P_t.[/Q_t.(P & Q)]]");
        testFormulas.add("[/R_<v,t>.[Ez_v.R(z)]] ");
        testFormulas.add("[/R_<v,t>.[Ez_v.(R(z) & sleep(x))]] ");
        testFormulas.add("[/P_t.[/Q_t.(P&Q)]]");
        testFormulas.add("(/P_<e,t>.(/Q_<e,t>.Ex_v(P(x) -> Q(x)))))");
        testFormulas.add("(/P_e.(/x_e.(/y_e.sleep(c(a(x),b(y),P(x),a(x,y))))))");

        Settings s = new Settings();
        s.setSemanticOutputStyle(0);
        LLProver1 prover = new LLProver1(new Settings());

        for (String p : testFormulas)
        {
            SemanticRepresentation pp = parseExpression(p);
            System.out.println("Input: " + p);
            System.out.println("Output: " + pp.toString());
            resetParser();
        }
        System.out.println("Done");
    }//Only call when settings initialized

    public void testParseExpression2(String test) {
        LLProver1 prover = new LLProver1(new Settings());
        SemanticRepresentation p = parseExpression(test);
        System.out.println("Input: " + test);
        System.out.println("Output: " + p.toString());
    }

    public SemanticRepresentation parse(String input) {
        SemanticRepresentation sr = parseExpression(input);
        variableBindings = new HashMap<>();
        pos = 0;
        bracketCounter = 0;
        return sr;
    }

    public SemanticRepresentation parseExpression(String input) {
        while (pos < input.length()) {
            while (input.charAt(pos) == ' ') {
                pos++;
            }
            char c = input.charAt(pos);
            pos++;
            if (c == 'E' || c == 'A' || c == 'I') {
                //  bracketCounter++;
                int current = pos;
                SemanticRepresentation semBinder = null;
                try {
                    semBinder = parseExpression(input);
                } catch (Exception e) {
                    pos = current;
                }
                if (semBinder instanceof SemAtom && ((SemAtom) semBinder).getSort().equals(SemAtom.SemSort.VAR)) {
                    //         bracketCounter = bracketCounter - 1;
                    SemanticRepresentation scope = parseExpression(input);
                    if (c == 'E') {
                        return new SemQuantEx(SemQuantEx.SemQuant.EX, (SemAtom) semBinder, scope, new SemType(AtomicType.T));
                    } else if (c == 'A') {
                        return new SemQuantEx(SemQuantEx.SemQuant.UNI, (SemAtom) semBinder, scope, new SemType(AtomicType.T));
                    } else if (c == 'I') {
                        return new SemQuantEx(SemQuantEx.SemQuant.DEF, (SemAtom) semBinder, scope, semBinder.getType());
                    }
                } else {
                    pos = current;
                }
            }
            if (c == '/') {
                //c = input.charAt(pos);
                SemanticRepresentation semBinder = parseExpression(input);
                return semBinder;
                //         if((c >= 97 && c <= 122) || (c >= 48 && c <= 57) || (c >= 66 && c <= 90)) {
            }
            if (c == '[' || c == '(') {
                bracketCounter++;
                SemanticRepresentation left = parseExpression(input);
                while (input.charAt(pos) == ' ') {
                    pos++;
                }
                c = input.charAt(pos);
                if (c == ']') {
                    return left;
                }
                if (c =='.') {
                    pos++;
                    SemanticRepresentation right = parseExpression(input);
                    pos++;
                    bracketCounter = bracketCounter - 1;
                    return new SemFunction((SemAtom) left, right);
                } else if (left instanceof FuncApp || left instanceof SemPred || left instanceof SemAtom) {
                    while (input.charAt(pos) == ' ') {
                        pos++;
                    }
                    c = input.charAt(pos);
                    if (c == '&') {
                        pos++;
                        SemanticRepresentation right = parseExpression(input);
                        pos++;
                        c = input.charAt(pos);
                        bracketCounter = bracketCounter - 1;
                        return new BinaryTerm(left, BinaryTerm.SemOperator.AND, right);
                    } else if (c == 'v') {
                        pos++;
                        SemanticRepresentation right = parseExpression(input);
                        pos++;
                        pos++;
                        bracketCounter = bracketCounter - 1;
                        return new BinaryTerm(left, BinaryTerm.SemOperator.OR, right);
                    } else if (c == '-' & input.charAt(pos + 1) == '>') {
                        pos = pos + 2;
                        SemanticRepresentation right = parseExpression(input);
                        pos++;
                        pos++;
                        bracketCounter = bracketCounter - 1;
                        return new BinaryTerm(left, BinaryTerm.SemOperator.IMP, right);
                    }
                } else {
                  //  bracketCounter = bracketCounter - 1;
                    return left;
                }
            }

            /*
            if (c == '(') {
                parenthesisCounter++;
                SemanticRepresentation left = parseExpression(input);
                while (input.charAt(pos) == ' ') {
                    pos++;
                }
                c = input.charAt(pos);
                if (c == ')') {
                    return left;
                }
                if (c =='.') {
                    pos++;
                    SemanticRepresentation right = parseExpression(input);
                    pos++;
                    parenthesisCounter = parenthesisCounter - 1;
                    return new SemFunction((SemAtom) left, right);
                }
                  else if (left instanceof FuncApp || left instanceof SemPred || left instanceof SemAtom) {
                    while (input.charAt(pos) == ' ') {
                        pos++;
                    }
                    c = input.charAt(pos);
                    if (c == '&') {
                        pos++;
                        SemanticRepresentation right = parseExpression(input);
                        pos++;
                        parenthesisCounter = parenthesisCounter - 1;
                        return new BinaryTerm(left, BinaryTerm.SemOperator.AND, right);
                    } else if (c == 'v') {
                        pos++;
                        SemanticRepresentation right = parseExpression(input);
                        pos++;
                        pos++;
                        parenthesisCounter = parenthesisCounter - 1;
                        return new BinaryTerm(left, BinaryTerm.SemOperator.OR, right);
                    } else if (c == '-' & input.charAt(pos + 1) == '>') {
                        pos = pos + 2;
                        SemanticRepresentation right = parseExpression(input);
                        pos++;
                        pos++;
                        parenthesisCounter = parenthesisCounter - 1;
                        return new BinaryTerm(left, BinaryTerm.SemOperator.IMP, right);
                    }
                }
                else
                    {
                    return left;
                }
            }

             */
            if (c == '{') {
                SemanticRepresentation first = parseExpression(input);
                c = input.charAt(pos);
                List<SemanticRepresentation> semSetList = new ArrayList<>();
                semSetList.add(first);
                while (c == ',') {
                    pos++;
                    SemanticRepresentation next = parseExpression(input);
                    semSetList.add(next);
                    c = input.charAt(pos);
                }
                return new SemSet(semSetList, first.getType());
            }
            if (c == ']' || c == ')') {
                pos++;
                bracketCounter = bracketCounter - 1;
            }

           // SemanticRepresentation first = null;

            if (!specialChars.contains((int) c)) {
                StringBuilder sb = new StringBuilder();
                //or sequence of letters
                while (!specialChars.contains((int) c)) {
                    if (pos < input.length()) {
                        sb.append(c);
                        c = input.charAt(pos);
                    } else {
                        sb.append(c);
                        break;
                    }
                    pos++;
                }
                pos = pos - 1;
                String varIdentifier = sb.toString();

                Object semRep = varIdentifier;

                for (Integer i : variableBindings.keySet()) {
                    if (i <= bracketCounter) {
                        for (SemAtom atom : variableBindings.get(i)) {
                            if (varIdentifier.equals(atom.getName())) {
                                semRep = atom;
                            }
                        }
                    }
                }
                c = input.charAt(pos);
                if (c == '(') {
                    //TODO or instance of semfunction
                    if (semRep instanceof SemAtom && ((SemAtom) semRep).getSort().equals(SemAtom.SemSort.VAR)) {
                        pos++;
                        SemanticRepresentation semRep2 = parseExpression(input);
                        pos++;
                        c = input.charAt(pos);
                        FuncApp fa = new FuncApp((SemanticRepresentation) semRep, semRep2);
                        List<SemanticRepresentation> argumentList = new ArrayList<>();
                        while (c == '(') {
                            pos++;
                            SemanticRepresentation semRep3 = parseExpression(input);
                            argumentList.add(semRep3);
                            pos++;
                            c = input.charAt(pos);
                        }
                        for (SemanticRepresentation sr : argumentList) {
                            fa = new FuncApp(fa, sr);
                        }
                        return fa;
                    } else {
                        pos++;
                        SemanticRepresentation semRep2 = parseExpression(input);
                        c = input.charAt(pos);
                        ArrayList<SemanticRepresentation> argumentList = new ArrayList<>();
                        argumentList.add(semRep2);
                        if (input.charAt(pos) == ')')
                        {
                            pos++;
                        }
                        while (c == ',')
                        {
                            pos++;
                            SemanticRepresentation semRep3 = parseExpression(input);
                            argumentList.add(semRep3);
                            c = input.charAt(pos);
                        }
                        if (input.charAt(pos) == ')')
                        {
                            pos++;
                        }
                        return new SemPred(varIdentifier, argumentList);
                    }
                }
                else
                    {
                    try {
                        if (input.charAt(pos) == '_')
                        {
                            pos++;
                            c = input.charAt(pos);
                            SemType t = null;
                            if (c == '<')
                            {
                                StringBuilder sb1 = new StringBuilder();
                                int typeBracketCounter = 1;
                                pos++;
                                sb1.append(c);
                                while (typeBracketCounter > 0)
                                {
                                    c = input.charAt(pos);
                                    sb1.append(c);
                                    if (c == '<')
                                    {
                                        typeBracketCounter++;
                                    }
                                    if (c == '>')
                                    {
                                        typeBracketCounter = typeBracketCounter - 1;
                                    }
                                    pos++;
                                }
                                t = typeParser(sb1.toString(), 0);
                            }
                            else
                                {
                                pos++;
                                t = typeParser("" + c, 0);
                                }
                            SemAtom newVar = new SemAtom(SemAtom.SemSort.VAR, varIdentifier, t);

                            if (!variableBindings.containsKey(bracketCounter))
                            {
                                variableBindings.put(bracketCounter, new ArrayList<SemAtom>());
                            }
                            variableBindings.get(bracketCounter).add(newVar);
                            if (newVar.getType().getLeft() == null && newVar.getType().getSimple() != null)
                            {

                                switch(newVar.getType().getSimple()) {
                                    case E:
                                            LexVariableHandler.getUsedVariables().
                                            get(LexVariableHandler.variableType.SemVarE).add(newVar.getName());
                                            break;
                                    case V:
                                        LexVariableHandler.getUsedVariables().
                                                get(LexVariableHandler.variableType.SemVarV).add(newVar.getName());
                                        break;
                                    case S:
                                        LexVariableHandler.getUsedVariables().
                                                get(LexVariableHandler.variableType.SemVarS).add(newVar.getName());
                                        break;
                                    case I:
                                        LexVariableHandler.getUsedVariables().
                                                get(LexVariableHandler.variableType.SemVarI).add(newVar.getName());
                                        break;
                                    case T:
                                        LexVariableHandler.getUsedVariables().
                                                get(LexVariableHandler.variableType.SemVar).add(newVar.getName());
                                        break;
                                    case TEMP:
                                        LexVariableHandler.getUsedVariables().
                                                get(LexVariableHandler.variableType.SemVar).add(newVar.getName());
                                        break;
                                }

                            }  else
                            {
                                LexVariableHandler.getUsedVariables().
                                        get(LexVariableHandler.variableType.SemVarComp).add(newVar.getName());
                            }
                            return newVar;
                        }
                    }
                    catch (Exception e)
                    {
                        System.out.println("Could not determine type of variable at " + pos);
                        e.printStackTrace();
                    }
                    if (!(semRep instanceof SemAtom))
                    {
                        semRep = new SemAtom(SemAtom.SemSort.CONST, varIdentifier, AtomicType.TEMP);
                    }
                    return (SemanticRepresentation) semRep;
                }
            }
        }
        return null;
    }


    public void resetParser() {
        pos = 0;
        bracketCounter = 0;
        variableBindings = new HashMap<>();
    }
}
