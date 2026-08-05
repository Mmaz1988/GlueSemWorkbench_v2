package webservice.rest.dtos;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;

public enum SemanticModel {
    LFGXDRT("lfgxdrt"),
    PROLOG_DRT("prolog-drt");

    private final String value;

    SemanticModel(String value) {
        this.value = value;
    }

    @JsonValue
    public String value() {
        return value;
    }

    @JsonCreator
    public static SemanticModel fromValue(String value) {
        if (value == null || value.isBlank()) {
            return LFGXDRT;
        }
        for (SemanticModel model : values()) {
            if (model.value.equals(value)) {
                return model;
            }
        }
        throw new IllegalArgumentException("Unsupported semantic model: " + value);
    }
}
