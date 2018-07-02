package gluePaP.glue.lfg;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.MatchResult;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class TransferRuleParser {
    private boolean isComment;
    private List<TransferRule> rules;

    public TransferRuleParser(Path rulefile) {
        String filetext = "";
        try {
             filetext = new String(Files.readAllBytes(rulefile));
        } catch (IOException e) {
            e.printStackTrace();
        }

        Matcher m = Pattern.compile("(.?)==>(.?)\\.",Pattern.MULTILINE).matcher(filetext);
        while(m.find()) {
            TransferRule newRule = new TransferRule();
            newRule.setAntecedents(Arrays.asList(m.group(1).split(",")));
            newRule.setConsequents(Arrays.asList(m.group(2).split(",")));
        }


    }

    public class TransferRule {
        private List<String> antecedents;
        private List<String> consequents;


        public List<String> getAntecedents() {
            return antecedents;
        }

        void setAntecedents(List<String> antecedents) {
            for (String a : antecedents) {
                a = a.trim();
            }
            this.antecedents = antecedents;
        }


        public List<String> getConsequents() {
            return consequents;
        }

        void setConsequents(List<String> consequents) {
            for (String c : consequents) {
                c = c.trim();
            }
            this.consequents = consequents;
        }


    }


}
