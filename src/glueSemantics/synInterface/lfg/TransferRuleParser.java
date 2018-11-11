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

package glueSemantics.synInterface.lfg;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class TransferRuleParser {
    private boolean isComment;
    private List<TransferRule> rules;


    public static void main(String[] args) {

        //TransferRuleParser trp = new TransferRuleParser("testrule.lfg")
    }

    public TransferRuleParser(Path rulefile) {


    }

    public class TransferRule {
        private List<TransferFact> antecedents;
        private List<TransferFact> consequents;
        private List<String> variables;


        public List<TransferFact> getAntecedents() {
            return antecedents;
        }

        void setAntecedents(List<TransferFact> antecedents) {
            this.antecedents = antecedents;
        }


        public List<TransferFact> getConsequents() {
            return consequents;
        }

        void setConsequents(List<TransferFact> consequents) {
            this.consequents = consequents;
        }


    }


    public List<TransferRule> parseRuleFile(Path rulefile) {
        String filetext = "";
        try {
            filetext = new String(Files.readAllBytes(rulefile));
        } catch (IOException e) {
            e.printStackTrace();
        }
        List<TransferRule> rules = new ArrayList<>();
        Matcher matcher = Pattern.compile("(.?)==>(.?)\\.",Pattern.MULTILINE).matcher(filetext);
        Pattern factpattern = Pattern.compile("(\\+)?(\\S+)\\((.+)\\)");
        while(matcher.find()) {
            // Match rules and split them at commas to extract transfer facts.
            // Rules are instantiated as an object and added to the list of rules.
            TransferRule newRule = new TransferRule();
            List<TransferFact> antecedents = new ArrayList<>();
            List<TransferFact> consequents = new ArrayList<>();
            Set<String> variables = new HashSet<>();

            // Add antecedent facts
            for (String f : matcher.group(1).split(",")) {
                // Split the left hand side of a rule at commas and extract all
                // transfer facts on that side
                f = f.trim();
                Matcher factmatch = factpattern.matcher(f);
                TransferFact  fact = new TransferFact(factmatch.group(2));
                if (factmatch.group(1) != null) fact.setConsumability(true);
                else fact.setConsumability(false);
                ArrayList<String> arguments = new ArrayList<>();
                for (String arg : matcher.group(3).split(",")) {
                    if (arg.trim().charAt(0) == '%')
                        variables.add(arg.trim());
                    else
                        arguments.add(arg.trim());
                }
                fact.setArguments(arguments);
                antecedents.add(fact);
            }

            // Add consequent facts
            for (String f : matcher.group(2).split(",")) {
                // Split the left hand side of a rule at commas and extract all
                // transfer facts on that side
                f = f.trim();
                Matcher factmatch = factpattern.matcher(f);
                TransferFact  fact = new TransferFact(factmatch.group(2));
                if (factmatch.group(1) != null) fact.setConsumability(true);
                else fact.setConsumability(false);
                ArrayList<String> arguments = new ArrayList<>();
                for (String arg : matcher.group(3).split(",")) {
                    if (arg.trim().charAt(0) == '%')
                        variables.add(arg.trim());
                    else
                        arguments.add(arg.trim());
                }
                fact.setArguments(arguments);
                consequents.add(fact);
            }

            newRule.setAntecedents(antecedents);
            newRule.setConsequents(consequents);
            rules.add(newRule);

        }
        return rules;

    }


}
