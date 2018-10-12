/*
 * Copyright 2018 Moritz Messmer and Mark-Matthias Zymla.
 * This file is part of the Glue Semantics Workbench
 * The Glue Semantics Workbench is free software and distributed under the conditions of the GNU General Public License,
 * without any warranty.
 * You should have received a copy of the GNU General Public License along with the source code.
 * If not, please visit http://www.gnu.org/licenses/ for more information.
 */

public abstract class Settings {
    private static final int PLAIN = 0;
    private static final int PROLOG = 1;
    // TODO implement latex output
    //private final int LATEX = 2;

    private static boolean betaReduce = true;
    private static int semanticOutputStyle = PLAIN;

    public Settings() {
    }

    public boolean isBetaReduce() {
        return betaReduce;
    }

    public void setBetaReduce(boolean betaReduce) {
        betaReduce = betaReduce;
    }

    public int getSemanticOutputStyle() {
        return semanticOutputStyle;
    }

    public void setSemanticOutputStyle(int semanticOutputStyle) {
        semanticOutputStyle = semanticOutputStyle;
    }
}
