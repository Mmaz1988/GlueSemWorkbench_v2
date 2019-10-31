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
    public static final int PLAIN = 0;
    public static final int PROLOG = 1;
    // TODO implement latex output
    //private final int LATEX = 2;

    private boolean betaReduce;
    private int semanticOutputStyle;
    private boolean debugging;
    private boolean partial;
    private boolean glueOnly;

    public Settings() {
        this.betaReduce = true;
        this.semanticOutputStyle = PLAIN;
        this.debugging = false;
        this.partial = false;
        this.glueOnly = false;
    }

    public Settings (boolean betaReduce, int semanticOutputStyle) {
        this.betaReduce = betaReduce;
        this.semanticOutputStyle = semanticOutputStyle;
        this.debugging = false;
        this.partial = false;
        this.glueOnly = false;
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

    public boolean isDebugging() {
        return debugging;
    }

    public void setDebugging(boolean debugging) {
        this.debugging = debugging;
    }

    public boolean isPartial() {
        return partial;
    }

    public void setPartial(boolean partial) {
        this.partial = partial;
    }

    public boolean isGlueOnly() {
        return glueOnly;
    }

    public void setGlueOnly(boolean glueOnly) {
        this.glueOnly = glueOnly;
    }

}
