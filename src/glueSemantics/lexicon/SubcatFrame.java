package glueSemantics.lexicon;

import glueSemantics.synInterface.dependency.LexVariableHandler;
import glueSemantics.synInterface.dependency.LexicalParserException;

import java.util.HashMap;
import java.util.LinkedHashMap;

import static glueSemantics.synInterface.dependency.LexVariableHandler.variableType.LLatomE;
import glueSemantics.lexicon.LexicalEntry.LexType;

public class SubcatFrame {
    private HashMap<String,String> scopeVars = new HashMap<>();
    // A Hashmap of the semantic roles associated with this verb: agent, patient, theme etc.
    private LinkedHashMap<String,LexicalEntry> semanticRoles = new LinkedHashMap<>();


    public String getScopeVar(String role) throws LexicalParserException {
        if (scopeVars.get(role) == null)
            throw new LexicalParserException(role + " was not initialized.");
        return scopeVars.get(role);
    }

    public void setScopeVar(String role, String var) {
        this.scopeVars.put(role, var);
    }

    public LinkedHashMap<String, LexicalEntry> getSemanticRoles() {
        return semanticRoles;
    }

    public LexicalEntry getRole(String role) {
        return this.semanticRoles.get(role);
    }

    /**
     * Initializes the specified role by mapping the lexical entry to the name of the role
     * in the semanticRoles HashMap and also creating a new LL atom (type E) that is mapped
     * to that role.
     * @param role The name of the semantic role, e.g. "agent", "patient"
     * @param entry The lexical entry that will be mapped to the role
     */
    public void initializeRole(String role, LexicalEntry entry) {
        this.semanticRoles.put(role,entry);
        this.setScopeVar(role,entry.identifier);
    }

    /**
     * Initializes a quantified role by mapping the lexical entry of the restrictor to the name of the role
     * in the semanticRoles HashMap and maps the identifier of the quantifier to this role.
     * This must be done so the root verb will get the correct variables on its glue side.
     * @param role The name of the semantic role, e.g. "agent", "patient"
     * @param restrictor The lexical entry that will be mapped to the role
     */
    public void initializeQuantifiedRole(String role, LexicalEntry restrictor, String quantIdentifier) throws LexicalParserException {
        if (role.equals("<unknown>"))
            throw new LexicalParserException("The f-structure file contains an unknown grammatical role");
        this.semanticRoles.put(role,restrictor);
        this.setScopeVar(role,quantIdentifier);
    }

    public LexType getLextype() {
        if (semanticRoles.containsKey("agent") && semanticRoles.containsKey("patient")) {
            return LexType.V_TRANS;
        }
        else if (semanticRoles.containsKey("agent")) {
            return LexType.V_INTR;
        }
        else
            return LexType.V_NULL;
    }
/*
    public enum LexType {

        //Verbs
        V_NULL,
        V_INTR,
        V_TRANS,
        V_DTRAN,
        V_COMP,
        V_XCOMP,

        //Nouns
        N_NN,
        N_NNP,
        N_DP,

        //Determiner
        DET,
        //modifiers
        MOD

    }

*/

}
