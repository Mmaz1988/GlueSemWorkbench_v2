package gluePaP.glue.lfg;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

public class TransferRuleParser {
    private boolean isComment;

    public TransferRuleParser(Path rulefile) {
        try {
            List<String> lines = Files.readAllLines(rulefile);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public class TransferRule {
        private List<String> antecedents;
        private List<String> consequents;
    }


}
