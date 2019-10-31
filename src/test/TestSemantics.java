package test;

import glueSemantics.semantics.SemanticRepresentation;
import glueSemantics.semantics.lambda.*;
import glueSemantics.synInterface.dependency.LexVariableHandler;
import main.Settings;
import org.junit.jupiter.api.Test;
import prover.LLProver2;
import prover.ProverException;

import java.util.ArrayList;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class TestSemantics {


@Test
    void testFuncApp()
{

    LLProver2 lp = new LLProver2(new Settings(true,0));
    SemAtom binder = new SemAtom(SemAtom.SemSort.VAR,LexVariableHandler.returnNewVar(LexVariableHandler.variableType.SemVarE),
            new SemType(SemType.AtomicType.E));

    SemAtom argument = new SemAtom(SemAtom.SemSort.CONST,"success",new SemType(SemType.AtomicType.E));

    SemFunction testFunction = new SemFunction(binder,new SemPred("test",binder));

    FuncApp testFA = new FuncApp(testFunction,argument);

    SemanticRepresentation result = null;
    try {
       result = testFA.betaReduce();
    } catch (ProverException e) {
        e.printStackTrace();
    }

    assertEquals("test(success)",result.toString());


}


    @Test
    void testPointwiseFuncApp()
    {

        LLProver2 lp = new LLProver2(new Settings(true,0));
        SemAtom binder = new SemAtom(SemAtom.SemSort.VAR,LexVariableHandler.returnNewVar(LexVariableHandler.variableType.SemVarE),
                new SemType(SemType.AtomicType.E));


        SemAtom argument = new SemAtom(SemAtom.SemSort.CONST,"success",new SemType(SemType.AtomicType.E));

        SemAtom argument2 = new SemAtom(SemAtom.SemSort.CONST,"success2",new SemType(SemType.AtomicType.E));

        SemAtom argument3 = new SemAtom(SemAtom.SemSort.CONST,"success3",new SemType(SemType.AtomicType.T));

        SemSet newSet = new SemSet(new ArrayList<>(),new SemType(SemType.AtomicType.E));

        newSet.addMember(argument);
        newSet.addMember(argument2);
        newSet.addMember(argument3);

        SemFunction testFunction = new SemFunction(binder,new SemPred("test",binder));

        FuncApp testFA = new FuncApp(testFunction,newSet);

        SemanticRepresentation result = null;
        try {
            result = testFA.betaReduce();
        } catch (ProverException e) {
            e.printStackTrace();
        }

        assertEquals("{test(success), test(success2)}",result.toString());


    }

    @Test
    void testPointwiseFuncApp2()
    {

        LLProver2 lp = new LLProver2(new Settings(true,0));

        SemAtom binder = new SemAtom(SemAtom.SemSort.VAR,LexVariableHandler.returnNewVar(LexVariableHandler.variableType.SemVarE),
                new SemType(SemType.AtomicType.E));

        SemAtom binder2 = new SemAtom(SemAtom.SemSort.VAR,LexVariableHandler.returnNewVar(LexVariableHandler.variableType.SemVarE),
                new SemType(SemType.AtomicType.E));

        SemAtom binder3 = new SemAtom(SemAtom.SemSort.VAR,LexVariableHandler.returnNewVar(LexVariableHandler.variableType.SemVarE),
                new SemType(SemType.AtomicType.E));

        SemAtom argument = new SemAtom(SemAtom.SemSort.CONST,"success",new SemType(SemType.AtomicType.E));

        SemFunction testFunction = new SemFunction(binder,new SemPred("test",binder));

        SemFunction testFunction2 = new SemFunction(binder2,new SemPred("test2",binder2));

        SemFunction testFunction3 = new SemFunction(binder3,new SemPred("test3",binder3));

        SemSet newSet = new SemSet(new ArrayList<>(),testFunction.getType());

        newSet.addMember(testFunction);
        newSet.addMember(testFunction2);
        newSet.addMember(testFunction3);

        FuncApp testFA = new FuncApp(newSet,argument);

        SemanticRepresentation result = null;
        try {
            result = testFA.betaReduce();
        } catch (ProverException e) {
            e.printStackTrace();
        }

        assertEquals("{test(success), test2(success), test3(success)}",result.toString());


    }


    @Test
    void testPointwiseFuncApp3()
    {

        LLProver2 lp = new LLProver2(new Settings(true,0));

        SemAtom binder = new SemAtom(SemAtom.SemSort.VAR,LexVariableHandler.returnNewVar(LexVariableHandler.variableType.SemVarE),
                new SemType(SemType.AtomicType.E));

        SemAtom binder2 = new SemAtom(SemAtom.SemSort.VAR,LexVariableHandler.returnNewVar(LexVariableHandler.variableType.SemVarE),
                new SemType(SemType.AtomicType.E));

        SemAtom binder3 = new SemAtom(SemAtom.SemSort.VAR,LexVariableHandler.returnNewVar(LexVariableHandler.variableType.SemVarE),
                new SemType(SemType.AtomicType.E));



        SemFunction testFunction = new SemFunction(binder,new SemPred("test",binder));

        SemFunction testFunction2 = new SemFunction(binder2,new SemPred("test2",binder2));

        SemFunction testFunction3 = new SemFunction(binder3,new SemPred("test3",binder3));





        SemSet newSet = new SemSet(new ArrayList<>(),testFunction.getType());

        newSet.addMember(testFunction);
        newSet.addMember(testFunction2);
        newSet.addMember(testFunction3);


        SemAtom argument = new SemAtom(SemAtom.SemSort.CONST,"success",new SemType(SemType.AtomicType.E));

        SemAtom argument2 = new SemAtom(SemAtom.SemSort.CONST,"success2",new SemType(SemType.AtomicType.E));

        SemAtom argument3 = new SemAtom(SemAtom.SemSort.CONST,"success3",new SemType(SemType.AtomicType.E));

        SemSet argSet = new SemSet(new ArrayList<>(),new SemType(SemType.AtomicType.E));

        argSet.addMember(argument);
        argSet.addMember(argument2);
        argSet.addMember(argument3);


        FuncApp testFA = new FuncApp(newSet,argSet);

        SemanticRepresentation result = null;
        try {
            result = testFA.betaReduce();
        } catch (ProverException e) {
            e.printStackTrace();
        }

        assertEquals("{{test(success), test(success2), test(success3)}, {test2(success), test2(success2), test2(success3)}, {test3(success), test3(success2), test3(success3)}}"
                ,result.toString());


    }

}
