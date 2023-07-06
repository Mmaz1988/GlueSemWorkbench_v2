package webservice.rest.dtos;

import java.util.List;

public class GswbGraph {

    public List<GswbGraphComponent> graphElements;
    public GswbGraph(List<GswbGraphComponent> graphElements)
    {
        this.graphElements = graphElements;
    }
}
