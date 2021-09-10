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

    private static boolean betaReduce = true;
    private static int semanticOutputStyle = PLAIN;
    private static boolean debugging = false;
    private static boolean partial = false;
    private static boolean glueOnly = false;
    private static boolean multipleEntries;
    private static boolean outputFile;
    private static boolean parseSemantics = false;
    private static boolean solutionOnly = false;
    private static boolean testExpression = false;
    private static boolean visualize = false;
    private static int proverType = HEPPLE;

    public Settings() {
    }

    public Settings (boolean betaReduce, int semanticOutputStyle) {
        Settings.betaReduce = betaReduce;
        Settings.semanticOutputStyle = semanticOutputStyle;
        debugging = false;
        partial = false;
        glueOnly = false;
        parseSemantics = false;
        solutionOnly = false;
        testExpression = false;
        proverType = HEPPLE;
    }

    public boolean isBetaReduce() {
        return betaReduce;
    }

    public void setBetaReduce(boolean betaReduce) {
        Settings.betaReduce = betaReduce;
    }

    public int getSemanticOutputStyle() {
        return semanticOutputStyle;
    }

    public void setSemanticOutputStyle(int semanticOutputStyle) {
        Settings.semanticOutputStyle = semanticOutputStyle;
    }

    public boolean isDebugging() {
        return debugging;
    }

    public void setDebugging(boolean debugging) {
        Settings.debugging = debugging;
    }

    public boolean isPartial() {
        return partial;
    }

    public void setPartial(boolean partial) {
        Settings.partial = partial;
    }

    public boolean isGlueOnly() {
        return glueOnly;
    }

    public void setGlueOnly(boolean glueOnly) {
        Settings.glueOnly = glueOnly;
    }

    public boolean isMultipleEntries() {
        return multipleEntries;
    }

    public void setMultipleEntries(boolean multipleEntries) {
        Settings.multipleEntries = multipleEntries;
    }

    public boolean isOutputFile() {
        return outputFile;
    }

    public void setOutputFile(boolean outputFile) {
        Settings.outputFile = outputFile;
    }

    public boolean isParseSemantics() {
        return parseSemantics;
    }

    public void setParseSemantics(boolean parseSemantics) {
        Settings.parseSemantics = parseSemantics;
    }


    public Boolean getSolutionOnly() {
        return solutionOnly;
    }

    public void setSolutionOnly(Boolean solutionOnly) {
        Settings.solutionOnly = solutionOnly;
    }

    public Boolean getTestExpression() {
        return testExpression;
    }

    public void setTestExpression(Boolean testExpression) {
        Settings.testExpression = testExpression;
    }

    public int getProverType() {
        return proverType;
    }

    public void setProverType(int proverType) {
        Settings.proverType = proverType;
    }
    public static boolean isVisualize() {
        return visualize;
    }

    public static void setVisualize(boolean visualize) {
        Settings.visualize = visualize;
    }

    public static boolean isVisualize() {
        return visualize;
    }

    public static void setVisualize(boolean visualize) {
        Settings.visualize = visualize;
    }


}
