package com.anwen.mongo.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * @author JiaChaoYang
 **/
@Getter
@AllArgsConstructor
public enum AggregateTypeEnum {

    PROJECT("project"),

    MATCH("match"),

    LIMIT("limit"),

    SKIP("skip"),

    SORT("sort"),

    GROUP("group"),

    LOOKUP("lookup"),

    ADD_FIELDS("addFields"),

    OUT("out")

    ;

    private final String type;

}
