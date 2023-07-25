package webservice.rest.dtos;

import java.util.HashMap;

public class GswbBatchOutput {

    public HashMap<String,GswbOutput> outputs;
    public String report;

    public GswbBatchOutput(HashMap<String,GswbOutput> outputs, String report) {
        this.outputs = outputs;
        this.report = report;
    }
}
