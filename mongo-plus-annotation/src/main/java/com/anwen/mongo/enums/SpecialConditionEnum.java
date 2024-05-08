package com.anwen.mongo.enums;

public enum SpecialConditionEnum {


    OR("$or"),

    AND("$and"),

    NOR("$nor"),

    SET("$set"),

    INC("$inc"),

    PUSH("$push"),

    EACH("$each"),

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

    public String getSubCondition(){
        return condition.substring(1);
    }

    SpecialConditionEnum(String condition) {
        this.condition = condition;
    }
}
