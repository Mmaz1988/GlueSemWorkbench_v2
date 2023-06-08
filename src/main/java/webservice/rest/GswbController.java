package webservice.rest;

import main.Settings;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;
import prover.LLProver;
import prover.LLProver1;
import prover.LLProver2;
import webservice.rest.dtos.GswbRequest;

@CrossOrigin
@RestController
public class GswbController {

    @Autowired
    private GswbService gswbService;

    public GswbController(){}

    @CrossOrigin
    //(origins = "http://localhost:63342")
    @PostMapping(value = "/deduce", produces = "application/json", consumes = "application/json")
    public String applyRuleRequestXLE2(@RequestBody GswbRequest request) {


        //    public GswbPreferences(int prover, int outputstyle, boolean solutionOnly, boolean debugging, boolean explainFail)
        Settings settings = new Settings();
        settings.setSemanticOutputStyle(request.gswbPreferences.outputstyle);
        settings.setProverType(request.gswbPreferences.prover);
        settings.setDebugging(request.gswbPreferences.debugging);
        settings.setSolutionOnly(request.gswbPreferences.solutionOnly);

        LLProver prover = null;
        StringBuilder sb = new StringBuilder();

        if (settings.getProverType() == 0) {
        prover = new LLProver2(settings,sb);
        } else {
            prover = new LLProver1(settings,sb);
        }




        return null;
    }


}
