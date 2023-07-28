package com.anwen.mongo.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum SpecialConditionEnum {

    AND("and"),

    OR("$or"),

    NOR("$nor"),

    ELEM_MATCH("$elemMatch"),

    LIKE("like"),

    REGEX("$regex")

    ;



    private final String condition;

}
