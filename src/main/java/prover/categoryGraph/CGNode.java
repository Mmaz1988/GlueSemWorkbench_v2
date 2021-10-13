package prover.categoryGraph;

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

    /*
    public void compressHistories()
    {
        Set<History> chart = new HashSet<>();
        if (histories.size() > 1) {
            List<History> agenda = new ArrayList<>(histories);

            while (!agenda.isEmpty()) {
                ListIterator<History> iter = agenda.listIterator();
                while (iter.hasNext()) {
                    History h1 = iter.next();
                    iter.remove();

                    Boolean added = false;
                    for (History h2 : chart) {

                        if (!(h1.equals(h2)) && h1.category.toString().equals(h2.category.toString()) &&
                                h1.indexSet.equals(h2.indexSet) && h1.discharges.equals(h2.discharges) && (h1.requirements.equals(h2.requirements))) {

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
            this.histories = chart;
        }
        }
        }

     */

    public void compressHistories()
    {
        Set<History> chart = new HashSet<>();
        if (histories.size() > 1) {
            List<History> agenda = new ArrayList<>(histories);

            while (!agenda.isEmpty()) {
                ListIterator<History> iter = agenda.listIterator();
                while (iter.hasNext()) {
                    History h1 = iter.next();
                    iter.remove();

                    Boolean added = false;
                    if (chart.isEmpty()) {
                        Iterator<History> chartIter = chart.iterator();
                        while (chartIter.hasNext()) {
                            History h2 = iter.next();

                            if (!(h1.equals(h2)) && h1.category.toString().equals(h2.category.toString()) &&
                                    h1.indexSet.equals(h2.indexSet) && h1.discharges.equals(h2.discharges) && (h1.requirements.equals(h2.requirements))) {

                                Set<HashMap<Integer, History>> nh = new HashSet<>();
                                nh.addAll(h1.parents);
                                nh.addAll(h2.parents);

                                History h3 = new History(h1.category, h1.indexSet, nh, h1.p, h1.prover);
                                h3.discharges = h1.discharges;
                                h3.requirements = h1.requirements;


                                added = true;
                                chart.add(h3);
                                iter.remove();
                            }

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
                this.histories = chart;
            }
        }
    }


    public static Set<History> compressHistories(List<History> histories)
    {
        Set<History> chart = new HashSet<>();

        if (histories.size() > 1) {
            List<History> agenda = new ArrayList<>(histories);

            while (!agenda.isEmpty()) {
                ListIterator<History> iter = agenda.listIterator();
                while (iter.hasNext()) {
                    History h1 = iter.next();
                    iter.remove();

                    Boolean added = false;
                    for (History h2 : chart) {

                        if (!(h1.equals(h2)) && h1.category.toString().equals(h2.category.toString()) &&
                                h1.indexSet.equals(h2.indexSet) && h1.discharges.equals(h2.discharges) && (h1.requirements.equals(h2.requirements))) {

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
              return chart;
            }
        }
        return null;
    }



    }


