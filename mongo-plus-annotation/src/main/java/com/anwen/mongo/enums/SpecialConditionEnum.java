package com.anwen.mongo.enums;

public enum SpecialConditionEnum {


    OR("$or"),

    NOR("$nor"),

    SET("$set"),

    IN("$in"),

    EQ("$eq"),

    ELEM_MATCH("$elemMatch"),

    REGEX("$regex"),

    TEXT("$text"),

    SEARCH("$search")

    ;


    private final String condition;

    public String getCondition() {
        return condition;
    }

    SpecialConditionEnum(String condition) {
        this.condition = condition;
    }
}
