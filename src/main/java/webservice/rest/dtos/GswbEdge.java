package webservice.rest.dtos;

import java.util.HashMap;

public class GswbEdge extends GswbGraphComponent{

    public GswbEdge(String source, String target){

        HashMap<String,Object> data = new HashMap<>();
        data.put("source", source);
        data.put("target",target);
        this.data = data;

    };

}
