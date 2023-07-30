package com.anwen.mongo.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * 条件枚举
 * @author JiaChaoYang
 * @date 2023/7/30 0:32
*/
@Getter
@AllArgsConstructor
public enum QueryOperatorEnum {

    MOD("mod"),

    LT("lt"),

    ELEM_MATCH("elemMatch"),

    TYPE("type"),

    NOR("nor"),

    NIN("nin"),

    NOT("not"),

    AND("and"),

    GTE("gte"),

    EXPR("expr"),

    LTE("lte"),

    ALL("all"),

    OR("or"),

    IN("in"),

    LIKE("like"),

    EQ("eq"),

    GT("gt"),

    REGEX("regex"),

    NE("ne"),

    TEXT("text"),

    EXISTS("exists");

    private final String value;

}
