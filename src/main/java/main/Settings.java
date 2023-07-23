/*
 * Copyright 2018 Mark-Matthias Zymla & Moritz Messmer
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package main;/*
 * Copyright 2018 Moritz Messmer and Mark-Matthias Zymla.
 * This file is part of the Glue Semantics Workbench
 * The Glue Semantics Workbench is free software and distributed under the conditions of the GNU General Public License,
 * without any warranty.
 * You should have received a copy of the GNU General Public License along with the source code.
 * If not, please visit http://www.gnu.org/licenses/ for more information.
 */

public class Settings {


 //Outputnodes
    public static final int PLAIN = 0;
    public static final int PROLOG = 1;
    public static final int NLTK = 3;
    // TODO implement latex output
    //private final int LATEX = 2;

    //Provers
    public static  final int HEPPLE = 0;
    public static final int LEV = 1;

    //resources
    public String resources = "gswb_resources/";

    //naturaldeduction output
    public static final int NDFULL = 0;
    public static final int NDREDUCED = 1;
    public static final int NOSEM = 2;

    private boolean betaReduce = true;
    private int semanticOutputStyle = PLAIN;
    private boolean debugging = false;
    private boolean partial = false;
    private boolean glueOnly = false;
    private boolean multipleEntries;
    private boolean outputFile;
    private boolean parseSemantics = false;
    private boolean solutionOnly = false;
    private boolean testExpression = false;
    private boolean visualize = false;
    private int proverType = HEPPLE;

    private int naturalDeductionOutput = NDFULL;


    private String explanation = "";
    private boolean explainFail = false;
    private boolean assureGlueParsing = false;


    private boolean webService = false;

    public Settings() {
    }

    public Settings (boolean betaReduce, int semanticOutputStyle) {
        this.betaReduce = betaReduce;
        this.semanticOutputStyle = semanticOutputStyle;
        this.debugging = false;
        this.partial = false;
        this.glueOnly = false;
        this.parseSemantics = false;
        this.solutionOnly = false;
        this.testExpression = false;
        this.proverType = HEPPLE;
    }

    public boolean isBetaReduce() {
        return this.betaReduce;
    }

    public void setBetaReduce(boolean betaReduce) {this.betaReduce = betaReduce;
    }

    public int getSemanticOutputStyle() {
        return this.semanticOutputStyle;
    }

    public void setSemanticOutputStyle(int semanticOutputStyle) {
        this.semanticOutputStyle = semanticOutputStyle;
    }

    public boolean isDebugging() {
        return this.debugging;
    }

    public void setDebugging(boolean debugging) {
        this.debugging = debugging;
    }

    public boolean isPartial() {
        return this.partial;
    }

    public void setPartial(boolean partial) {
        this.partial = partial;
    }

    public boolean isGlueOnly() {
        return this.glueOnly;
    }

    public void setGlueOnly(boolean glueOnly) {
        glueOnly = this.glueOnly;
    }

    public boolean isMultipleEntries() {
        return this.multipleEntries;
    }

    public void setMultipleEntries(boolean multipleEntries) {
        this.multipleEntries = multipleEntries;
    }

    public boolean isOutputFile() {
        return this.outputFile;
    }

    public void setOutputFile(boolean outputFile) {
        this.outputFile = outputFile;
    }

    public boolean isParseSemantics() {
        return this.parseSemantics;
    }

    public void setParseSemantics(boolean parseSemantics) {
        this.parseSemantics = parseSemantics;
    }


    public Boolean getSolutionOnly() {
        return this.solutionOnly;
    }

    public void setSolutionOnly(Boolean solutionOnly) {
        this.solutionOnly = solutionOnly;
    }

    public Boolean getTestExpression() {
        return this.testExpression;
    }

    public void setTestExpression(Boolean testExpression) {
        this.testExpression = testExpression;
    }

    public int getProverType() {
        return this.proverType;
    }

    public void setProverType(int proverType) {
        this.proverType = proverType;
    }
    public boolean isVisualize() {
        return this.visualize;
    }

    public void setVisualize(boolean visualize) {this.visualize = visualize;
    }

    public boolean isWebService() {
        return this.webService;
    }

    public void setWebService(boolean webService) {
        this.webService = webService;
    }

    public String getExplanation() {
        return this.explanation;
    }

    public void setExplanation(String explanation) {
        this.explanation = explanation;
    }

    public boolean isExplainFail() {
        return this.explainFail;
    }

    public void setExplainFail(boolean explainFail) {
        this.explainFail = explainFail;
    }

    public boolean isAssureGlueParsing() {
        return this.assureGlueParsing;
    }

    public void setAssureGlueParsing(boolean assureGlueParsing) {
        this.assureGlueParsing = assureGlueParsing;
    }

    public int getNaturalDeductionOutput() {
        return naturalDeductionOutput;
    }

    public void setNaturalDeductionOutput(int naturalDeductionOutput) {
        this.naturalDeductionOutput = naturalDeductionOutput;
    }
}

