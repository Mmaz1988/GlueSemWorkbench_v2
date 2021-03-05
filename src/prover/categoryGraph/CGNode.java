package prover.categoryGraph;

import glueSemantics.linearLogic.LLTerm;
import prover.LLProver;

import java.util.*;

public class CGNode {

    public enum type {
        CATEGORY,
        CONNECTOR,
        SCC
    }
    public LLProver prover;
    public String category;
    public type nodeType;
    public Set<History> histories = new HashSet<>();

    public CGNode(String category, type nodeType, LLProver prover)
    {
        this.category = category;
        this.nodeType = nodeType;
        this.prover = prover;
    }

    @Override
    public String toString()
    {
        return this.category;
    }

    public void compressHistories()
    {
        if (histories.size() > 1) {
            List<History> agenda = new ArrayList<>(histories);
            List<History> chart = new ArrayList<>();

            while (!agenda.isEmpty()) {
                ListIterator<History> iter = agenda.listIterator();
                while (iter.hasNext()) {
                    History h1 = iter.next();
                    iter.remove();

                    Boolean added = false;
                    for (History h2 : chart) {

                        if (!(h1.equals(h2)) && h1.indexSet.equals(h2.indexSet) && h1.discharges.equals(h2.discharges) && (h1.requirements.equals(h2.requirements))) {

                            Set<HashMap<Integer, History>> nh = new HashSet<>();
                            nh.addAll(h1.parents);
                            nh.addAll(h2.parents);
                            h2.parents = nh;
                            added = true;
                            break;
                        }
                    }
                    if (!added)
                    {
                        chart.add(h1);
                    }
                }
            }
            if (!chart.isEmpty())
            {
                prover.db.discardedHistories = prover.db.discardedHistories + this.histories.size() - chart.size();
            this.histories = new HashSet<>(chart);
        }
        }
        }
    }


