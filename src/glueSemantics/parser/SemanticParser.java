package glueSemantics.parser;

import glueSemantics.semantics.SemanticRepresentation;
import glueSemantics.semantics.lambda.*;
import glueSemantics.synInterface.dependency.LexVariableHandler;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

public class SemanticParser {

    private int bracketCounter = 0;
    private int pos = 0;
    private HashMap<Integer, List<SemAtom>> variableBindings = new HashMap<>();

    public SemanticParser()
    {
    }

    @Test
    void testParseExpression()
    {
      //  SemanticRepresentation s = parseExpression( "[/P_<e,t>.[/x_e.[/y_e.P(x)(y)]]]");
     //   SemanticRepresentation p = parseExpression( "[/x_e.[/y_e.sleep(a(x,y),b(y,x))]]");
       // SemanticRepresentation p = parseExpression( "[/x_e.[/y_e.sleep(a(x),b(y))]]");
       // SemanticRepresentation p = parseExpression( "[/P_e.[/x_e.[/y_e.sleep(c(a(x),b(y),P(x),a(x,y)))]]]");
       // SemanticRepresentation p = parseExpression( "[/P_e.[/Q_e.[/y_e.[P(y) & Q(y)]]]]");
        SemanticRepresentation p = parseExpression( "[/P_<e,t>.[/Q_<e,t>.some(x_e,P(x),Q(x))]]");
      //  SemType t = typeParser("<e,<e,t>>",0);

        System.out.println("Done");
    }


    public SemanticRepresentation parseExpression(String input)
    {


        while(pos < input.length())
        {
            while (input.charAt(pos) == ' ')
            {
                pos++;
            }
            char c = input.charAt(pos);
            pos++;
                    if (c == '/') {

                        c = input.charAt(pos);

                        SemanticRepresentation semBinder = parseExpression(input);
                        pos++;
                        return  semBinder;
               //         if((c >= 97 && c <= 122) || (c >= 48 && c <= 57) || (c >= 66 && c <= 90)) {
                        }


                    if (c == '[')
                    {
                        bracketCounter++;
                        SemanticRepresentation left = parseExpression(input);
                        if (left instanceof SemAtom)
                        {
                            SemanticRepresentation right = parseExpression(input);
                            pos++;
                            return new SemFunction((SemAtom) left,right);
                        }

                        else if(left instanceof FuncApp || left instanceof SemPred)
                        {
                            while (input.charAt(pos) == ' ')
                            {pos++;}

                            c = input.charAt(pos);

                            if (c == '&')
                            {
                                pos++;
                                SemanticRepresentation right = parseExpression(input);
                                pos++;
                                c = input.charAt(pos);
                                return new BinaryTerm(left, BinaryTerm.SemOperator.AND,right);
                            }
                            else if (c == 'v')
                            {
                                pos++;
                                SemanticRepresentation right = parseExpression(input);
                                pos++;
                                pos++;
                                return new BinaryTerm(left, BinaryTerm.SemOperator.OR,right);
                            }
                            else
                            {
                                pos++;
                                SemanticRepresentation right = parseExpression(input);
                                pos++;
                                pos++;
                                return new BinaryTerm(left, BinaryTerm.SemOperator.IMP,right);
                            }
                        }
                        else
                        {
                            return  left;
                        }

                    }

            if (c == '(')
            {
                bracketCounter++;
                SemanticRepresentation left = parseExpression(input);

                if(left instanceof FuncApp || left instanceof SemPred)
                {
                    while (input.charAt(pos) == ' ')
                    {pos++;}

                    c = input.charAt(pos);

                    if (c == '&')
                    {
                        pos++;
                        SemanticRepresentation right = parseExpression(input);
                        pos++;
                        return new BinaryTerm(left, BinaryTerm.SemOperator.AND,right);
                    }
                    else if (c == 'v')
                    {
                        pos++;
                        SemanticRepresentation right = parseExpression(input);
                        pos++;
                        pos++;
                        return new BinaryTerm(left, BinaryTerm.SemOperator.OR,right);
                    }
                    else
                    {
                        pos++;
                        SemanticRepresentation right = parseExpression(input);
                        pos++;
                        pos++;
                        return new BinaryTerm(left, BinaryTerm.SemOperator.IMP,right);
                    }
                }
                else
                {
                    return  left;
                }
            }

            if (c == ']')
            {
                pos++;
                bracketCounter = bracketCounter - 1;
            }
                    if ((c >= 97 && c <= 122) || (c >= 48 && c <= 57) || (c >= 66 && c <= 90))
                    {


                        StringBuilder sb = new StringBuilder();
                        //or sequence of letters
                        while ((c >= 97 && c <= 122) || (c >= 48 && c <= 57) || (c >= 66 && c <= 90)) {
                            sb.append(c);
                            c = input.charAt(pos);
                            pos++;
                        }
                        pos = pos - 1;
                        String varIdentifier = sb.toString();

                        Object semRep = varIdentifier;

                       for (Integer i : variableBindings.keySet())
                       {
                           if (i <= bracketCounter)
                           {
                               for (SemAtom atom : variableBindings.get(i))
                               {
                                   if (varIdentifier.equals(atom.getName()))
                                   {
                                       semRep = atom;
                                   }
                               }
                           }
                       }

                       c = input.charAt(pos);
                       if (c == '(')
                       {
                           if(semRep instanceof SemAtom && ((SemAtom) semRep).getSort().equals(SemAtom.SemSort.VAR))
                           {
                               pos++;
                               SemanticRepresentation semRep2 = parseExpression(input);
                               pos++;
                               c = input.charAt(pos);
                               FuncApp fa = new FuncApp((SemanticRepresentation) semRep,semRep2);
                               List<SemanticRepresentation> argumentList = new ArrayList<>();
                               while(c == '(')
                               {
                                   pos++;
                                   SemanticRepresentation semRep3 = parseExpression(input);
                                   argumentList.add(semRep3);
                                   pos++;
                                   c = input.charAt(pos);
                               }

                               for (SemanticRepresentation sr : argumentList)
                               {
                                   fa = new FuncApp(fa,sr);
                               }
                               return fa;
                           }
                           else
                           {
                               pos++;
                               SemanticRepresentation semRep2 = parseExpression(input);
                               c = input.charAt(pos);
                               ArrayList<SemanticRepresentation> argumentList = new ArrayList<>();
                               argumentList.add(semRep2);

                               if(input.charAt(pos) == ')')
                                    {pos++;}

                               while(c == ',')
                               {
                                   pos++;
                                   SemanticRepresentation semRep3 = parseExpression(input);
                                   argumentList.add(semRep3);
                                   c = input.charAt(pos);

                               }

                               return new SemPred(varIdentifier,argumentList);



                           }


                       }
                       else
                       {

                           try {
                               if (input.charAt(pos) == '_') {
                                   pos++;
                                   c = input.charAt(pos);
                                   SemType t = null;
                                   if (c == '<') {

                                       StringBuilder sb1 = new StringBuilder();
                                       int typeBracketCounter = 1;
                                       pos++;
                                       sb1.append(c);
                                       while (typeBracketCounter > 0) {
                                           c = input.charAt(pos);
                                           sb1.append(c);

                                           if (c == '<') {
                                               typeBracketCounter++;
                                           }
                                           if (c == '>') {
                                               typeBracketCounter = typeBracketCounter - 1;
                                           }

                                           pos++;
                                       }
                                       t = typeParser(sb1.toString(), 0);

                                   } else {
                                       pos++;
                                       t = typeParser("" + c, 0);
                                   }


                                   SemAtom newVar = new SemAtom(SemAtom.SemSort.VAR, varIdentifier, t);
                                   if (!variableBindings.containsKey(bracketCounter)) {
                                       variableBindings.put(bracketCounter, new ArrayList<SemAtom>());
                                   }
                                   variableBindings.get(bracketCounter).add(newVar);


                                   if (newVar.getType().getLeft() == null) {
                                       if (newVar.getType().toString().equals("e")) {
                                           LexVariableHandler.getUsedVariables().
                                                   get(LexVariableHandler.variableType.SemVarE).add(newVar.getName());
                                       }
                                   }

                                   return newVar;
                               }
                           } catch (Exception e) {
                               System.out.println("Could not determine type of variable at " + pos);
                               e.printStackTrace();
                           }


                           if (!(semRep instanceof SemAtom)) {
                               semRep = new SemAtom(SemAtom.SemSort.CONST, varIdentifier, SemType.AtomicType.TEMP);
                           }
                           return (SemanticRepresentation) semRep;
                       }


                    }



        }


        return null;
    }


    private SemType typeParser(String input, int i)
    {




        {
        while(i < input.length())
            {
                char c = input.charAt(i);
                if (c == 'e')
                {
                    return  new SemType(SemType.AtomicType.E);
                }
                if (c == 't')
                {

                    return new SemType(SemType.AtomicType.T);
                }
                if (c == '<')
                {
                    i++;
                    SemType left = typeParser(input,i);
                    i++;
                    SemType right = typeParser(input,i);
                    return new SemType(left,right);
                }
                i++;
            }
        }

        return null;

    }

    public void resetParser()
    {
        pos = 0;
        bracketCounter = 0;
    }
}
