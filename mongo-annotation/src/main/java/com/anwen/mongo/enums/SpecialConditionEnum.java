package com.anwen.mongo.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
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

}
