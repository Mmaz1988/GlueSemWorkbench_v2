/*
 * Copyright 2018 Mark-Matthias Zymla & Moritz Messmer
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package glueSemantics.synInterface.dependency;

import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;

import edu.stanford.nlp.ling.CoreLabel;
import edu.stanford.nlp.process.TokenizerFactory;
import edu.stanford.nlp.parser.lexparser.LexicalizedParser;
import edu.stanford.nlp.process.CoreLabelTokenFactory;
import edu.stanford.nlp.process.PTBTokenizer;
import edu.stanford.nlp.process.Tokenizer;
import edu.stanford.nlp.trees.*;

public class DependencyParser {
    private final static String PCG_MODEL = "edu/stanford/nlp/models/lexparser/englishPCFG.ser.gz";

    private final TokenizerFactory<CoreLabel> tokenizerFactory = PTBTokenizer.factory(new CoreLabelTokenFactory(), "invertible=true");

    private final TreebankLanguagePack tlp = new PennTreebankLanguagePack();

    //for dependencies
    private final GrammaticalStructureFactory gsf = tlp.grammaticalStructureFactory();

    private final LexicalizedParser parser = LexicalizedParser.loadModel(PCG_MODEL);

    public List<GrammaticalStructure> parseSet;


    public DependencyParser(){}


    public List<GrammaticalStructure> generateParses(List<String> sentences) {

        List<GrammaticalStructure> parsedSentences = new ArrayList<>();

        for (String sentence : sentences)
        {
            Tree tree = parser.parse(sentence);
            GrammaticalStructure gs = gsf.newGrammaticalStructure(tree);

            parsedSentences.add(gs);
        }
        return parsedSentences;
    }


    public GrammaticalStructure parse(String sentence) {
        Tree tree = parser.parse(sentence);
        GrammaticalStructure gs = gsf.newGrammaticalStructure(tree);
        return gs;
    }

    private List<CoreLabel> tokenize(String str) {
        Tokenizer<CoreLabel> tokenizer =
                tokenizerFactory.getTokenizer(
                        new StringReader(str));
        return tokenizer.tokenize();
    }

    }


