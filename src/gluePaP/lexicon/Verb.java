package gluePaP.lexicon;

import java.util.LinkedHashMap;
import java.util.StringJoiner;

public class Verb extends LexicalEntry {

    LexType lexType;


    public Verb(LinkedHashMap<String,LexicalEntry> subCatFrame, String lemma) {


    this.lexType = lexTypeFromSubCat(subCatFrame);

/*
        StringBuilder sb = new StringBuilder();

        sb.append("(");
        sb.append(((Noun) subCatFrame.get("agent")).formula);
        sb.append(" -o ");
        sb.append("(");
        sb.append(((Noun) subCatFrame.get("patient")).formula);
        sb.append(" -o ");
        sb.append(" f_t");
        sb.append("))");
        // sb.append(")");
        premises.add(sb.toString());

        */

        //StringJoiner sj = new StringJoiner(" ");

        //f is standard variable for complete f-structure
        //g is standard variable for subject
        //h is standard variable for object

        switch (this.getLexType()) {
            case V_INTR:

                this.llFormula = "(g_e -o f_t)";

                break;

            case V_TRANS:

                this.llFormula = "(g_e -o (h_e -o f_t))";


        }
    }


    //Setter and Getter methods

    public LexType getLexType() {
        return lexType;
    }

    public void setLexType(LexType lexType) {
        this.lexType = lexType;
    }

    public String getLlFormula() {
        return llFormula;
    }

    public void setLlFormula(String llFormula) {
        this.llFormula = llFormula;
    }


    //trivial version of generating LexType from SubCatFrame
    public LexType lexTypeFromSubCat(LinkedHashMap<String,LexicalEntry> subCat)
    {
        if (subCat.size() == 1)
        {
            return LexType.V_INTR;
        }
        if (subCat.size() == 2 && subCat.containsKey("patient"))
        {
            return LexType.V_TRANS;
        }

        return null;
    }
}


