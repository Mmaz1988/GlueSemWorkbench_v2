package gluePaP.glue;

import Prover.LLProver;
import Prover.ProverException;
import Prover.VariableBindingException;
import com.sun.jdi.event.ModificationWatchpointEvent;
import edu.stanford.nlp.ling.IndexedWord;
import edu.stanford.nlp.trees.GrammaticalStructure;
import edu.stanford.nlp.trees.TypedDependency;
import gluePaP.lexicon.*;
import gluePaP.linearLogic.LLTerm;
import gluePaP.linearLogic.Premise;
import gluePaP.linearLogic.Sequent;
import gluePaP.parser.LinearLogicParser;
import gluePaP.semantics.SemAtom;

import java.lang.reflect.Array;
import java.util.*;
import java.util.regex.Pattern;

// Sentence meaning is a set of glue representations (i.e. a set that represents the available premises)

/*This class determines which lexical entry (if any) is constructed for a given word in the sentence
based on its syntactic structure. (Mainly dependency structure for now)
*/

public class SentenceMeaning {
    private final GrammaticalStructure dependencyStructure;
    private final LinkedHashMap<IndexedWord,List<Tuple>> dependencyMap;
    public List<GlueRepresentation> gluePremises;


    public SentenceMeaning(GrammaticalStructure parsedSentence) throws VariableBindingException
    {
        this.dependencyStructure = parsedSentence;

        /* A depdency map is a hash map whose key is a word in the parsed sentence and whose value is
        a list of all (direct) dependencies of this word. For example:
        Every dog barks.
        every = []
        dog = det(every)
        barks = subj(dog)
        Thus we have a flat structure that still preserves possible transitive relations
        (i.e. A - B - C => A - C) For reference see Unhammer(2010; LFG-based Constituent and Function Alignment for Parallel Treebanking)
        for a similar approach on LFG
         */
        this.dependencyMap = generateDependencyMap();
        System.out.println(dependencyStructure.typedDependencies());

         // Returns the root verb
        IndexedWord root = returnRoot();

        //SubCatFrame produced from syntactic input; is used to derive meaning constructors
        LinkedHashMap<String,LexicalEntry> subCatFrame = new LinkedHashMap<>();

        // Collection of LLFormulas for generating premises
        List<LexicalEntry> lexicalEntries = new ArrayList<>();

        /* Arity of the root verb;
         TODO We need to make this a method that processes all verbal heads,
        so that complements can also be analyzed like this
        */
        Integer rootArity = 0;

        Iterator it = dependencyMap.get(root).iterator();

        while (it.hasNext())
        {
            Tuple t = (Tuple)it.next();
            //Basic categorization based on Universal dependency tags
            //All types of modifiers
            if (t.left.contains("mod"))
            {
                System.out.println( t.right.toString() + " This is a modifier");
            }
            //All types of complements
            else if (t.left.contains("comp"))
            {
                rootArity++;
                System.out.println( t.right.toString() + " This is a complement");
            }

            //Processes subject
           else if (t.left.contains("subj")) {


                //TODO Subjects usually have the identifier g, what to do with embedded subjects?
                HashMap<String,List<LexicalEntry>> subj = extractArgumentEntries(t.right,"g");
                List<LexicalEntry> main = (List<LexicalEntry>) subj.get("main");
                subCatFrame.put("agent",main.get(0));

                lexicalEntries.add(main.get(0));
                subj.remove("main");


                //Adds modifiers of the subject
                if (!subj.keySet().isEmpty()) {
                    for (String key : subj.keySet()) {
                        for (LexicalEntry lex : subj.get(key))
                        {
                            lexicalEntries.add(lex);
                        }
                    }
                }

                it.remove();
                rootArity++;
                System.out.println( t.right.toString() + " This is a subject");
            }

            //Processes object -- Same problem as subject
          else if (t.left.contains("obj"))
            {
                HashMap<String,List<LexicalEntry>> obj = extractArgumentEntries(t.right,"h");

                List<LexicalEntry> main = (List<LexicalEntry>) obj.get("main");

                subCatFrame.put("patient",main.get(0));

                lexicalEntries.add(main.get(0));
                obj.remove("main");

                //Adds modifiers of the object
                if (!obj.keySet().isEmpty()) {
                    for (String key : obj.keySet()) {
                        for (LexicalEntry lex : obj.get(key))
                        {
                            lexicalEntries.add(lex);
                        }

                    }
                }
                it.remove();
                rootArity++;
                System.out.println( t.right.toString() + " This is a object");
            }



        }

        /* Verb is generated last based on the structure of the sentence
        The verb is generated when all its dependencies have been processed
        */

        Verb rootverb;

       if (dependencyMap.get(root).isEmpty())
       {
           rootverb = new Verb(subCatFrame,root.value());
           lexicalEntries.add(rootverb);

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

           LexVariableHandler.resetVars();

      }


        System.out.println(root.toString() + " has arity " + rootArity);

        //System.out.print(premises);

        // Calling engine for deduction for test purposes here

        //LinearLogicParser parser = new LinearLogicParser(premises);




        //LinearLogicParser parser = new LinearLogicParser(testquant);
        Sequent testseq = new Sequent(lexicalEntries);

        System.out.println(testseq.toString());

        System.out.println("Checking simple prover...");
        LLProver prover = new LLProver(testseq);
        List<Premise> result = null;
        try {
            result = prover.deduce();
            System.out.println("Found valid deduction(s): ");
            for (Premise sol : result) {
                System.out.println(sol.toString());
            }
        } catch (ProverException e) {
            e.printStackTrace();
        }

        System.out.println("Done!");
    }


    // generates a HashMap for search purposes; flat representation of dependency structure
    public LinkedHashMap<IndexedWord,List<Tuple>> generateDependencyMap()
    {
        LinkedHashMap<IndexedWord,List<Tuple>> dependencyMap = new LinkedHashMap<>();

        for (TypedDependency structure : dependencyStructure.typedDependencies())
        {
            //new entry if no key for the respective pred is available
            if (dependencyMap.get(structure.gov()) == null)

            {
                List<Tuple> values = new ArrayList<>();
                values.add(new Tuple(structure.reln().toString(),structure.dep()));
                dependencyMap.put(structure.gov(), values);
            }
                else
                    {
                        dependencyMap.get(structure.gov()).add(new Tuple(structure.reln().toString(),structure.dep()));
            }
        }
        return dependencyMap;
    }



    // checks if a word has a specific governing dependency relation
    public boolean hasDependencyType(String dependency,IndexedWord word)
    {
         for (Tuple tuple : dependencyMap.get(word))
         {
             if (dependency.equals(tuple.left))
             {
                 return true;
             }
         }
         return false;
    }


    //Checks dominance relation disregarding dependency
    public boolean governsWord(IndexedWord word1, IndexedWord word2)
    {
        for (Tuple tuple : dependencyMap.get(word1))
        {
            if (word2 == tuple.right)
            {
                return true;
            }
        }
        return false;
    }


    //Returns the main verb of the sentence
    public IndexedWord returnRoot()
    {

        for (TypedDependency td : dependencyStructure.typedDependencies())
              {
                  if (td.reln().toString().equals("root"))
                  {
                      return td.dep();
                  }
              }
              // TODO Add exception?
        return null;
    }


    // Process (nominal) arguments (Subjects, objects)
    private HashMap<String,List<LexicalEntry>> extractArgumentEntries(IndexedWord iw, String identifier)
    {
        HashMap<String,List<LexicalEntry>> lexEn = new HashMap<>();

        if (iw.tag().equals("NNP"))
        {
            Noun main = new Noun(LexicalEntry.LexType.N_NNP,identifier,iw);
    //        premises.add(agent.llFormula);
    //        subCatFrame.put("agent",agent);

            lexEn.put("main",new ArrayList<LexicalEntry>(Arrays.asList(main)));
        }
        else if (iw.tag().equals("NN"))
        {
            Noun main = new Noun(LexicalEntry.LexType.N_NN,identifier,iw);
            //        premises.add(agent.llFormula);
            //        subCatFrame.put("agent",agent);

            lexEn.put("main",new ArrayList<LexicalEntry>(Arrays.asList(main)));
        }

        if (dependencyMap.get(iw) != null)
        {
        for (Tuple t : dependencyMap.get(iw))
        {

            if (t.left.equals("amod"))
            {
                if (!lexEn.containsKey("mod"))
                {
                    List<LexicalEntry> modifiers = new ArrayList<>();
                    modifiers.add(new Modifier(identifier));
                    lexEn.put("mod",modifiers);
                }
                else
                {
                    lexEn.get("mod").add(new Modifier(identifier));
                }
            }
            else if (t.left.equals("det"))
            {
                Determiner det = new Determiner(identifier);

                //TODO make this proper
                lexEn.remove("main");


                lexEn.put("main",new ArrayList<LexicalEntry>(Arrays.asList(det)));
            }
        }
        }
        return lexEn;
    }

    }




