package main;/*
 * Copyright 2018 Moritz Messmer and Mark-Matthias Zymla.
 * This file is part of the Glue Semantics Workbench
 * The Glue Semantics Workbench is free software and distributed under the conditions of the GNU General Public License,
 * without any warranty.
 * You should have received a copy of the GNU General Public License along with the source code.
 * If not, please visit http://www.gnu.org/licenses/ for more information.
 */

public class Settings {
    public static final int PLAIN = 0;
    public static final int PROLOG = 1;
    // TODO implement latex output
    //private final int LATEX = 2;

    private boolean betaReduce;
    private int semanticOutputStyle;

    public Settings() {
        this.betaReduce = true;
        this.semanticOutputStyle = PLAIN;
    }

    public Settings (boolean betaReduce, int semanticOutputStyle) {
        this.betaReduce = betaReduce;
        this.semanticOutputStyle = semanticOutputStyle;
    }

    public boolean isBetaReduce() {
        return betaReduce;
    }

    public void setBetaReduce(boolean betaReduce) {
        this.betaReduce = betaReduce;
    }

    public int getSemanticOutputStyle() {
        return semanticOutputStyle;
    }

    public void setSemanticOutputStyle(int semanticOutputStyle) {
        this.semanticOutputStyle = semanticOutputStyle;
    }
}
